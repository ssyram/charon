//! Generate Haskell deserialization code for our types.
//!
//! This binary runs charon on itself and generates the appropriate Aeson FromJSON instances for
//! our types. The generated functions are inserted into `./generate-hs/templates/*.hs` to
//! construct the final Haskell modules.
//!
//! To run it, call `cargo run --bin generate-hs`. It is also run by `make generate-hs` in the
//! crate root. Don't forget to format the output code after regenerating.
#![feature(if_let_guard)]

use anyhow::{Context, Result, bail};
use assert_cmd::cargo::CommandCargoExt;
use charon_lib::ast::*;
use convert_case::{Case, Casing};
use indoc::indoc;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

/// `Name` is a complex datastructure; to inspect it we serialize it a little bit.
fn repr_name(_crate_data: &TranslatedCrate, n: &Name) -> String {
    n.name
        .iter()
        .map(|path_elem| match path_elem {
            PathElem::Ident(i, _) => i.clone(),
            PathElem::Impl(..) => "<impl>".to_string(),
            PathElem::Instantiated(..) => "<mono>".to_string(),
        })
        .join("::")
}

fn make_haskell_ident(name: &str) -> String {
    let mut name = name.to_case(Case::Pascal);
    // Haskell type names must start with uppercase
    // Constructor names must also start with uppercase
    if matches!(
        &*name,
        "Type" | "Data" | "Class" | "Instance" | "Module" | "Import"
    ) {
        name += "_";
    }
    name
}

fn make_haskell_field_name(name: &str) -> String {
    let mut name = name.to_case(Case::Camel);
    // Haskell field names must start with lowercase
    if matches!(
        &*name,
        "type" | "data" | "class" | "instance" | "module" | "import" | "let" | "in" | "case" | "of"
    ) {
        name += "_";
    }
    name
}

fn type_name_to_haskell_ident(item_meta: &ItemMeta) -> String {
    let name = item_meta
        .attr_info
        .rename
        .as_ref()
        .unwrap_or(item_meta.name.name.last().unwrap().as_ident().unwrap().0);
    make_haskell_ident(name)
}

struct GenerateCtx<'a> {
    crate_data: &'a TranslatedCrate,
    name_to_type: HashMap<String, &'a TypeDecl>,
    /// For each type, list the types it contains.
    type_tree: HashMap<TypeDeclId, HashSet<TypeDeclId>>,
    manual_type_impls: HashMap<TypeDeclId, String>,
    manual_json_impls: HashMap<TypeDeclId, String>,
}

impl<'a> GenerateCtx<'a> {
    fn new(
        crate_data: &'a TranslatedCrate,
        manual_type_impls: &[(&str, &str)],
        manual_json_impls: &[(&str, &str)],
    ) -> Self {
        let mut name_to_type: HashMap<String, &TypeDecl> = Default::default();
        let mut type_tree = HashMap::default();
        for ty in &crate_data.type_decls {
            let long_name = repr_name(crate_data, &ty.item_meta.name);
            if long_name.starts_with("charon_lib") {
                let short_name = ty
                    .item_meta
                    .name
                    .name
                    .last()
                    .unwrap()
                    .as_ident()
                    .unwrap()
                    .0
                    .clone();
                name_to_type.insert(short_name, ty);
            }
            name_to_type.insert(long_name, ty);

            let mut contained = HashSet::new();
            ty.dyn_visit(|id: &TypeDeclId| {
                contained.insert(*id);
            });
            type_tree.insert(ty.def_id, contained);
        }

        let mut ctx = GenerateCtx {
            crate_data: &crate_data,
            name_to_type,
            type_tree,
            manual_type_impls: Default::default(),
            manual_json_impls: Default::default(),
        };
        ctx.manual_type_impls = manual_type_impls
            .iter()
            .map(|(name, def)| (ctx.id_from_name(name), def.to_string()))
            .collect();
        ctx.manual_json_impls = manual_json_impls
            .iter()
            .map(|(name, def)| (ctx.id_from_name(name), def.to_string()))
            .collect();
        ctx
    }

    fn id_from_name(&self, name: &str) -> TypeDeclId {
        self.name_to_type
            .get(name)
            .expect(&format!("Name not found: `{name}`"))
            .def_id
    }

    /// List the (recursive) children of this type.
    fn children_of(&self, name: &str) -> HashSet<TypeDeclId> {
        let start_id = self.id_from_name(name);
        self.children_of_inner(vec![start_id])
    }

    /// List the (recursive) children of these types.
    fn children_of_many(&self, names: &[&str]) -> HashSet<TypeDeclId> {
        self.children_of_inner(names.iter().map(|name| self.id_from_name(name)).collect())
    }

    fn children_of_inner(&self, ty: Vec<TypeDeclId>) -> HashSet<TypeDeclId> {
        let mut children = HashSet::new();
        let mut stack = ty.to_vec();
        while let Some(id) = stack.pop() {
            if !children.contains(&id)
                && self
                    .crate_data
                    .type_decls
                    .get(id)
                    .is_some_and(|decl| decl.item_meta.is_local)
            {
                children.insert(id);
                if let Some(contained) = self.type_tree.get(&id) {
                    stack.extend(contained);
                }
            }
        }
        children
    }
}

/// Converts a type to the appropriate Haskell type name.
fn type_to_haskell_name(ctx: &GenerateCtx, ty: &Ty) -> String {
    match ty.kind() {
        TyKind::Literal(LiteralTy::Bool) => "Bool".to_string(),
        TyKind::Literal(LiteralTy::Char) => "Char".to_string(),
        TyKind::Literal(LiteralTy::Int(int_ty)) => match int_ty {
            IntTy::I128 => "Integer".to_string(),
            _ => "Int".to_string(),
        },
        TyKind::Literal(LiteralTy::UInt(uint_ty)) => match uint_ty {
            UIntTy::U128 => "Integer".to_string(),
            _ => "Int".to_string(),
        },
        TyKind::Literal(LiteralTy::Float(_)) => "Double".to_string(),
        TyKind::Adt(tref) => {
            let args = tref
                .generics
                .types
                .iter()
                .map(|ty| type_to_haskell_name(ctx, ty))
                .collect_vec();
            match tref.id {
                TypeId::Adt(id) => {
                    let base_ty = if let Some(tdecl) = ctx.crate_data.type_decls.get(id) {
                        type_name_to_haskell_ident(&tdecl.item_meta)
                    } else {
                        format!("MissingType{id}")
                    };
                    // Convert Rust types to Haskell equivalents
                    if base_ty == "Vec" {
                        return format!("[{}]", args[0]);
                    }
                    if base_ty == "Ustr" || base_ty == "String_" {
                        return "Text".to_string();
                    }
                    if base_ty == "Vector" {
                        return format!("[{}]", args[0]);
                    }
                    if base_ty == "Option" {
                        return format!("Maybe {}", args[0]);
                    }
                    if args.is_empty() {
                        base_ty
                    } else {
                        format!("({} {})", base_ty, args.join(" "))
                    }
                }
                TypeId::Builtin(BuiltinTy::Box) => args[0].clone(),
                TypeId::Tuple => {
                    if args.is_empty() {
                        "()".to_string()
                    } else {
                        format!("({})", args.join(", "))
                    }
                }
                _ => unimplemented!("{ty:?}"),
            }
        }
        TyKind::TypeVar(DeBruijnVar::Free(id)) => format!("a{id}"),
        _ => unimplemented!("{ty:?}"),
    }
}

fn extract_doc_comments(attr_info: &AttrInfo) -> String {
    attr_info
        .attributes
        .iter()
        .filter_map(|a| a.as_doc_comment())
        .join("\n")
}

/// Make a Haskell doc comment (Haddock format)
fn build_doc_comment(comment: String, indent_level: usize) -> String {
    if comment.is_empty() {
        return comment;
    }
    let indent = "  ".repeat(indent_level);
    let lines = comment
        .lines()
        .map(|line| {
            let line = line.strip_prefix(" ").unwrap_or(line);
            format!("{indent}-- | {line}")
        })
        .join("\n");
    lines
}

fn build_type(_ctx: &GenerateCtx, decl: &TypeDecl, body: &str) -> String {
    let ty_name = type_name_to_haskell_ident(&decl.item_meta);
    let generics = decl
        .generics
        .types
        .iter()
        .enumerate()
        .map(|(i, _)| format!("a{i}"))
        .collect_vec();
    let generics_str = if generics.is_empty() {
        String::new()
    } else {
        format!(" {}", generics.join(" "))
    };
    let comment = extract_doc_comments(&decl.item_meta.attr_info);
    let comment = build_doc_comment(comment, 0);
    let comment_part = if comment.is_empty() {
        String::new()
    } else {
        format!("{comment}\n")
    };
    // Add deriving clause for Show, Eq, Ord
    let deriving = "\n  deriving (Show, Eq, Ord)";
    format!("{comment_part}data {ty_name}{generics_str} = {body}{deriving}")
}

/// Generate a Haskell type declaration that mirrors `decl`.
fn type_decl_to_haskell_decl(ctx: &GenerateCtx, decl: &TypeDecl) -> String {
    let body = match &decl.kind {
        _ if let Some(def) = ctx.manual_type_impls.get(&decl.def_id) => def.clone(),
        TypeDeclKind::Alias(ty) => {
            let ty_str = type_to_haskell_name(ctx, ty);
            return format!("type {} = {}", type_name_to_haskell_ident(&decl.item_meta), ty_str);
        }
        TypeDeclKind::Struct(fields) if fields.is_empty() => {
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            format!("{ty_name}")
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => {
            // Tuple struct
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            let field_types = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let ty_str = type_to_haskell_name(ctx, &f.ty);
                    // Wrap complex types in parentheses
                    if ty_str.contains(' ') || ty_str.starts_with('(') {
                        format!("({ty_str})")
                    } else {
                        ty_str
                    }
                })
                .join(" ");
            format!("{ty_name} {field_types}")
        }
        TypeDeclKind::Struct(fields) => {
            // Record struct
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            let field_decls = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let base_field_name = f.renamed_name().unwrap_or_else(|| f.name.as_deref().unwrap());
                    // Prefix field name with type name to avoid conflicts in Haskell
                    let field_name = make_haskell_field_name(&format!("{}_{}", ty_name.to_lowercase(), base_field_name));
                    let field_ty = type_to_haskell_name(ctx, &f.ty);
                    let comment = extract_doc_comments(&f.attr_info);
                    let comment = build_doc_comment(comment, 1);
                    let comment_part = if comment.is_empty() {
                        String::new()
                    } else {
                        format!("{comment}\n  ")
                    };
                    format!("{comment_part}{field_name} :: {field_ty}")
                })
                .join("\n  , ");
            format!("{ty_name}\n  {{ {field_decls}\n  }}")
        }
        TypeDeclKind::Enum(variants) => {
            let variant_decls = variants
                .iter()
                .filter(|v| !v.is_opaque())
                .map(|variant| {
                    let variant_name = make_haskell_ident(&variant.renamed_name());
                    if variant.fields.is_empty() {
                        // Unit variant
                        variant_name
                    } else if variant.fields.iter().all(|f| f.name.is_none()) {
                        // Tuple variant
                        let field_types = variant
                            .fields
                            .iter()
                            .map(|f| {
                                let ty_str = type_to_haskell_name(ctx, &f.ty);
                                // Wrap complex types in parentheses
                                if ty_str.contains(' ') || ty_str.starts_with('(') {
                                    format!("({ty_str})")
                                } else {
                                    ty_str
                                }
                            })
                            .join(" ");
                        format!("{variant_name} {field_types}")
                    } else {
                        // Record variant (not commonly used in Haskell, treat as tuple)
                        let field_types = variant
                            .fields
                            .iter()
                            .map(|f| {
                                let ty_str = type_to_haskell_name(ctx, &f.ty);
                                // Wrap complex types in parentheses
                                if ty_str.contains(' ') || ty_str.starts_with('(') {
                                    format!("({ty_str})")
                                } else {
                                    ty_str
                                }
                            })
                            .join(" ");
                        format!("{variant_name} {field_types}")
                    }
                })
                .join("\n  | ");
            variant_decls
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
    };
    build_type(ctx, decl, &body)
}

/// Generate Aeson FromJSON instance for a type
fn type_decl_to_json_deserializer(ctx: &GenerateCtx, decl: &TypeDecl) -> String {
    // Skip generating FromJSON instances for type aliases to avoid overlapping instances
    if matches!(&decl.kind, TypeDeclKind::Alias(_)) {
        return String::new();
    }
    
    let ty_name = type_name_to_haskell_ident(&decl.item_meta);
    
    let generics = decl
        .generics
        .types
        .iter()
        .enumerate()
        .map(|(i, _)| format!("a{i}"))
        .collect_vec();
    
    let instance_context = if !generics.is_empty() {
        let constraints = generics
            .iter()
            .map(|g| format!("FromJSON {g}"))
            .join(", ");
        format!("({constraints}) => ", constraints = constraints)
    } else {
        String::new()
    };
    
    let ty_with_params = if generics.is_empty() {
        ty_name.clone()
    } else {
        format!("({} {})", ty_name, generics.join(" "))
    };

    let parse_impl = match &decl.kind {
        _ if let Some(def) = ctx.manual_json_impls.get(&decl.def_id) => def.clone(),
        TypeDeclKind::Struct(fields) if fields.is_empty() => {
            indoc! {r#"
                parseJSON Null = pure ()
                parseJSON _ = fail "Expected null"
            "#}.to_string()
        }
        // Special case for ID types: single field named "_raw" of type Int
        // These are serialized as plain numbers in JSON
        TypeDeclKind::Struct(fields) if fields.elem_count() == 1 
            && fields.iter().next().and_then(|f| f.name.as_deref()) == Some("_raw") 
            && fields.iter().next().is_some_and(|f| matches!(f.ty.kind(), TyKind::Literal(LiteralTy::Int(_) | LiteralTy::UInt(_)))) => {
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            // Deserialize from a plain number
            format!("parseJSON = fmap {ty_name} . parseJSON", ty_name = ty_name)
        }
        TypeDeclKind::Struct(fields) if fields.iter().all(|f| f.name.is_none()) => {
            // Tuple struct - parse as array
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            let field_parsers = fields
                .iter()
                .enumerate()
                .filter(|(_, f)| !f.is_opaque())
                .map(|(i, _)| format!("    v{i} <- parseJSON (v V.! {i})"))
                .join("\n");
            let field_vars = fields
                .iter()
                .enumerate()
                .filter(|(_, f)| !f.is_opaque())
                .map(|(i, _)| format!("v{i}"))
                .join(" ");
            format!(
                "parseJSON = withArray \"{ty_name}\" $ \\v -> do\n{field_parsers}\n    pure ({ty_name} {field_vars})",
                ty_name = ty_name,
                field_parsers = field_parsers,
                field_vars = field_vars
            )
        }
        TypeDeclKind::Struct(fields) => {
            // Record struct - parse as object
            let ty_name = type_name_to_haskell_ident(&decl.item_meta);
            let field_parsers = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let rust_name = f.name.as_ref().unwrap();
                    let base_field_name = f.renamed_name().unwrap_or(rust_name);
                    // Prefix field name with type name to match the type definition
                    let hs_name = make_haskell_field_name(&format!("{}_{}", ty_name.to_lowercase(), base_field_name));
                    format!("    {hs_name} <- o .: \"{rust_name}\"")
                })
                .join("\n");
            let field_list = fields
                .iter()
                .filter(|f| !f.is_opaque())
                .map(|f| {
                    let base_field_name = f.renamed_name().unwrap_or_else(|| f.name.as_deref().unwrap());
                    // Prefix field name with type name to match the type definition
                    make_haskell_field_name(&format!("{}_{}", ty_name.to_lowercase(), base_field_name))
                })
                .join(" ");
            format!(
                "parseJSON = withObject \"{ty_name}\" $ \\o -> do\n{field_parsers}\n    pure ({ty_name} {field_list})",
                ty_name = ty_name,
                field_parsers = field_parsers,
                field_list = field_list
            )
        }
        TypeDeclKind::Enum(variants) => {
            // List of enum variant constructors that conflict with struct names from other modules
            // TraitImpl (in TraitRefKind) and TraitMethod (in FnPtrKind) conflict with GAst structs
            let types_variant_conflicts = ["TraitImpl", "TraitMethod"];
            // Meta variants that conflict with GAst structs  
            let meta_variant_conflicts = ["Local"];
            
            let variant_parsers = variants
                .iter()
                .filter(|v| !v.is_opaque())
                .map(|variant| {
                    let variant_name = make_haskell_ident(&variant.renamed_name());
                    // Qualify with appropriate prefix if this variant conflicts
                    let qualified_variant = if types_variant_conflicts.contains(&variant_name.as_str()) {
                        format!("T.{}", variant_name)
                    } else if meta_variant_conflicts.contains(&variant_name.as_str()) {
                        format!("M.{}", variant_name)
                    } else {
                        variant_name.clone()
                    };
                    let rust_name = &variant.name;
                    if variant.fields.is_empty() {
                        // Unit variant
                        format!(r#"String "{rust_name}" -> pure {qualified_variant}"#)
                    } else {
                        // Complex variant - parse as object with single key
                        let field_count = variant.fields.iter().filter(|f| !f.is_opaque()).count();
                        if field_count == 1 {
                            // Single field variant
                            let lines = vec![
                                format!("Object o | H.lookup \"{rust_name}\" o /= Nothing -> do"),
                                format!("  v <- o .: \"{rust_name}\""),
                                format!("  {qualified_variant} <$> parseJSON v"),
                            ];
                            lines.join("\n      ")
                        } else {
                            // Multiple fields - parse inner value as array
                            let field_parsers = variant
                                .fields
                                .iter()
                                .enumerate()
                                .filter(|(_, f)| !f.is_opaque())
                                .map(|(i, _)| format!("v{i} <- parseJSON (v V.! {i})"))
                                .collect_vec();
                            let field_vars = variant
                                .fields
                                .iter()
                                .enumerate()
                                .filter(|(_, f)| !f.is_opaque())
                                .map(|(i, _)| format!("v{i}"))
                                .join(" ");
                            
                            let mut lines = vec![
                                format!("Object o | H.lookup \"{rust_name}\" o /= Nothing -> do"),
                            ];
                            lines.push(format!("  withArray \"{variant_name}\" (\\v -> do"));
                            for parser in field_parsers {
                                lines.push(format!("    {parser}"));
                            }
                            lines.push(format!("    pure ({qualified_variant} {field_vars})) =<< o .: \"{rust_name}\""));
                            lines.join("\n      ")
                        }
                    }
                })
                .collect_vec();
            
            // Join variant parsers with proper indentation
            // Each variant pattern should be indented 4 spaces from "parseJSON v = case v of"
            let formatted_parsers = variant_parsers
                .iter()
                .map(|p| format!("    {}", p.replace("\n      ", "\n    ")))
                .join("\n");
            
            format!(
                "parseJSON v = case v of\n{}\n    _ -> fail \"Unknown variant\"",
                formatted_parsers
            )
        }
        TypeDeclKind::Union(..) => todo!(),
        TypeDeclKind::Opaque => todo!(),
        TypeDeclKind::Error(_) => todo!(),
        TypeDeclKind::Alias(_) => unreachable!("Type aliases are skipped earlier"),
    };

    format!(
        indoc! {r#"
            instance {instance_context}FromJSON {ty_with_params} where
              {parse_impl}
        "#},
        instance_context = instance_context,
        ty_with_params = ty_with_params,
        parse_impl = parse_impl.trim_end()
    )
}

/// The kind of code generation to perform.
#[derive(Clone, Copy)]
enum GenerationKind {
    FromJson,
    TypeDecl,
}

/// Replace markers in `template` with auto-generated code.
struct GenerateCodeFor {
    template: PathBuf,
    target: PathBuf,
    /// Each list corresponds to a marker. We replace the ith `{- __REPLACE{i}__ -}` marker with
    /// generated code for each definition in the ith list.
    markers: Vec<(GenerationKind, HashSet<TypeDeclId>)>,
}

impl GenerateCodeFor {
    fn generate(&self, ctx: &GenerateCtx) -> Result<()> {
        let mut template = fs::read_to_string(&self.template)
            .with_context(|| format!("Failed to read template file {}", self.template.display()))?;
        for (i, (kind, names)) in self.markers.iter().enumerate() {
            let tys = names
                .iter()
                .map(|&id| &ctx.crate_data[id])
                .sorted_by_key(|tdecl| {
                    tdecl
                        .item_meta
                        .name
                        .name
                        .last()
                        .unwrap()
                        .as_ident()
                        .unwrap()
                });
            let generated = match kind {
                GenerationKind::FromJson => {
                    let instances = tys
                        .map(|ty| type_decl_to_json_deserializer(ctx, ty))
                        .format("\n\n");
                    format!("{instances}")
                }
                GenerationKind::TypeDecl => {
                    let decls = tys
                        .map(|ty| type_decl_to_haskell_decl(ctx, ty))
                        .format("\n\n");
                    format!("{decls}")
                }
            };
            let placeholder = format!("{{- __REPLACE{i}__ -}}");
            template = template.replace(&placeholder, &generated);
        }

        fs::write(&self.target, template)
            .with_context(|| format!("Failed to write generated file {}", self.target.display()))?;
        Ok(())
    }
}

fn main() -> Result<()> {
    let dir = PathBuf::from("src/bin/generate-hs");
    let charon_llbc = dir.join("charon-itself.ullbc");
    let reuse_llbc = std::env::var("CHARON_HS_REUSE_LLBC").is_ok(); // Useful when developing
    if !reuse_llbc {
        // Call charon on itself
        let mut cmd = Command::cargo_bin("charon")?;
        cmd.arg("cargo");
        cmd.arg("--hide-marker-traits");
        cmd.arg("--hide-allocator");
        cmd.arg("--ullbc");
        cmd.arg("--start-from=charon_lib::ast::krate::TranslatedCrate");
        cmd.arg("--start-from=charon_lib::ast::ullbc_ast::BodyContents");
        cmd.arg("--exclude=charon_lib::common::hash_consing::HashConsed");
        cmd.arg("--dest-file");
        cmd.arg(&charon_llbc);
        cmd.arg("--");
        cmd.arg("--lib");
        let output = cmd.output()?;

        if !output.status.success() {
            let stderr = String::from_utf8(output.stderr.clone())?;
            bail!("Compilation failed: {stderr}")
        }
    }

    let crate_data: TranslatedCrate = charon_lib::deserialize_llbc(&charon_llbc)?;
    let output_dir = if std::env::var("IN_CI").as_deref() == Ok("1") {
        dir.join("generated")
    } else {
        dir.join("../../../../charon-hs/src/generated")
    };
    generate_hs(crate_data, dir.join("templates"), output_dir)
}

fn generate_hs(
    crate_data: TranslatedCrate,
    template_dir: PathBuf,
    output_dir: PathBuf,
) -> anyhow::Result<()> {
    let manual_type_impls = &[
        // None currently needed
    ];
    let manual_json_impls = &[
        // Hand-written because we filter out `None` values.
        (
            "Vector",
            indoc!(
                r#"
                parseJSON = fmap catMaybes . parseJSON
                "#
            ),
        ),
        // Hand-written because Name is transparent in Rust (serializes as just the inner vec)
        (
            "Name",
            indoc!(
                r#"
                parseJSON = fmap Name . parseJSON
                "#,
            ),
        ),
    ];
    
    let ctx = GenerateCtx::new(
        &crate_data,
        manual_type_impls,
        manual_json_impls,
    );

    // Compute the sets of types to be put in each module (similar to generate-ml).
    let manually_implemented: HashSet<_> = [
        "ItemOpacity",
        "PredicateOrigin",
        "Ty",
        "Opaque",
        "Body",
        "FunDecl",
        "TranslatedCrate",  // Too complex with LLBC/ULLBC dependencies - manually implement
        "Vector",  // Type alias for [v] with phantom type parameter - don't generate instance (would conflict with list instance)
        // These have name conflicts between GAst structs and Types variants/fields
        // Manual instances in GAstOfJson.hs template and type defs in GAst.hs template
        "TraitImpl",
        "TraitMethod",
        "Field",
        "Local",
        "Assert",  // Renamed to "Assertion" in JSON
        "Call",
        "CopyNonOverlapping",
    ]
    .iter()
    .map(|name| ctx.id_from_name(name))
    .collect();

    // Compute type sets for json deserializers.
    let (gast_types, llbc_types, ullbc_types) = {
        let llbc_types: HashSet<_> = ctx.children_of("charon_lib::ast::llbc_ast::Statement");
        let ullbc_types: HashSet<_> = ctx.children_of("charon_lib::ast::ullbc_ast::BodyContents");
        let all_types: HashSet<_> = ctx.children_of("TranslatedCrate");

        let shared_types: HashSet<_> = llbc_types.intersection(&ullbc_types).copied().collect();
        let llbc_types: HashSet<_> = llbc_types.difference(&shared_types).copied().collect();
        let ullbc_types: HashSet<_> = ullbc_types.difference(&shared_types).copied().collect();

        let body_specific_types: HashSet<_> = llbc_types.union(&ullbc_types).copied().collect();
        let gast_types: HashSet<_> = all_types
            .difference(&body_specific_types)
            .copied()
            .collect();

        let gast_types: HashSet<_> = gast_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        let llbc_types: HashSet<_> = llbc_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        let ullbc_types: HashSet<_> = ullbc_types
            .difference(&manually_implemented)
            .copied()
            .collect();
        (gast_types, llbc_types, ullbc_types)
    };

    let dont_generate_ty = &[
        "ItemOpacity",
        "PredicateOrigin",
        "TraitTypeConstraintId",
        "Ty",
        "Vector",
        "TargetInfo",  // Manually defined in GAst.hs template
        // These have name conflicts and are manually defined in GAst.hs template
        "TraitImpl",
        "TraitMethod",
        "Field",
        "Local",
        "Assert",  // Renamed to "Assertion" in JSON
        "Call",
        "CopyNonOverlapping",
    ];

    let mut processed_tys: HashSet<TypeDeclId> = dont_generate_ty
        .iter()
        .map(|name| ctx.id_from_name(name))
        .collect();
    
    // Helper to get children of types that haven't been processed yet
    let mut markers_from_children = |ctx: &GenerateCtx, markers: &[_]| {
        markers
            .iter()
            .copied()
            .map(|(kind, type_names)| {
                let types: HashSet<_> = ctx.children_of_many(type_names);
                let unprocessed_types: HashSet<_> =
                    types.difference(&processed_tys).copied().collect();
                processed_tys.extend(unprocessed_types.iter().copied());
                (kind, unprocessed_types)
            })
            .collect()
    };

    #[rustfmt::skip]
    let generate_code_for = vec![
        GenerateCodeFor {
            template: template_dir.join("Meta.hs"),
            target: output_dir.join("Generated_Meta.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "File",
                    "Span",
                    "AttrInfo",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Values.hs"),
            target: output_dir.join("Generated_Values.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "Literal",
                    "IntegerTy",
                    "LiteralTy",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Types.hs"),
            target: output_dir.join("Generated_Types.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "TypeVarId",
                    "ConstGeneric",
                    "TraitClauseId",
                    "DeBruijnVar",
                    "ItemId",
                    "TyKind",
                    "TraitImplRef",
                    "FunDeclRef",
                    "GlobalDeclRef",
                    "Binder",
                    "AbortKind",
                    "TypeDecl",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("Expressions.hs"),
            target: output_dir.join("Generated_Expressions.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "Rvalue",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("GAst.hs"),
            target: output_dir.join("Generated_GAst.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "Call",
                    "Assert",
                    "ItemSource",
                    "Locals",
                    "FunSig",
                    "CopyNonOverlapping",
                    "GlobalDecl",
                    "TraitDecl",
                    "TraitImpl",
                    "CliOpts",
                    "GExprBody",
                    "DeclarationGroup",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("LlbcAst.hs"),
            target: output_dir.join("Generated_LlbcAst.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "charon_lib::ast::llbc_ast::Statement",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("UllbcAst.hs"),
            target: output_dir.join("Generated_UllbcAst.hs"),
            markers: markers_from_children(&ctx, &[
                (GenerationKind::TypeDecl, &[
                    "charon_lib::ast::ullbc_ast::Statement",
                    "charon_lib::ast::ullbc_ast::SwitchTargets",
                    "charon_lib::ast::ullbc_ast::BodyContents",
                ]),
            ]),
        },
        GenerateCodeFor {
            template: template_dir.join("GAstOfJson.hs"),
            target: output_dir.join("Generated_GAstOfJson.hs"),
            markers: vec![(GenerationKind::FromJson, gast_types)],
        },
        GenerateCodeFor {
            template: template_dir.join("LlbcOfJson.hs"),
            target: output_dir.join("Generated_LlbcOfJson.hs"),
            markers: vec![(GenerationKind::FromJson, llbc_types)],
        },
        GenerateCodeFor {
            template: template_dir.join("UllbcOfJson.hs"),
            target: output_dir.join("Generated_UllbcOfJson.hs"),
            markers: vec![(GenerationKind::FromJson, ullbc_types)],
        },
    ];
    
    // Create output directory if it doesn't exist
    fs::create_dir_all(&output_dir)?;
    
    for file in generate_code_for {
        file.generate(&ctx)?;
    }
    Ok(())
}
