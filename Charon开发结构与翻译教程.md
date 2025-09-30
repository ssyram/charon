# Charon 开发结构与翻译教程

本教程面向希望快速理解并修改 Charon 项目的编译器中间表示开发者。从整体功能概述到详细内部机制，逐步深入讲解 Charon 的架构和实现。

## 1. 仓库功能概述

**Charon 主功能**：将 Rust crate 编译为 LLBC (Low-Level Borrow Calculus) 中间表示
- 输入：Rust 源代码项目
- 处理：通过 Rustc 获取 MIR → 翻译为 ULLBC → 变换为 LLBC
- 输出：JSON 格式的 (U)LLBC 文件，供下游形式化验证工具使用

**charon-ml 功能**：为 OCaml 工具提供 AST 解析
- 自动生成 OCaml 类型定义和 JSON 反序列化代码
- 当 Rust 中 AST 修改后，运行 `make generate-ml` 同步到 ML
- 包含手写的打印和名称匹配工具

## 2. 完整目录结构

```
charon/                          # 项目根目录
├── charon/                      # Rust 实现主体
│   ├── src/
│   │   ├── bin/
│   │   │   ├── charon/          # 入口可执行文件
│   │   │   │   └── main.rs      # 主入口: 设置环境调用 charon-driver
│   │   │   ├── charon-driver/   # 核心翻译驱动
│   │   │   │   ├── main.rs      # 驱动入口: 调用 Rustc
│   │   │   │   ├── driver.rs    # Rustc 回调配置
│   │   │   │   └── translate/   # 翻译模块 ⭐ 最复杂
│   │   │   │       ├── translate_crate.rs     # 翻译调度主循环
│   │   │   │       ├── translate_ctx.rs       # 翻译上下文
│   │   │   │       ├── translate_items.rs     # 项分派翻译
│   │   │   │       ├── translate_generics.rs  # 泛型处理
│   │   │   │       ├── translate_functions.rs # 函数翻译
│   │   │   │       ├── translate_types.rs     # 类型翻译
│   │   │   │       └── ...                    # 其他翻译子模块
│   │   │   └── generate-ml/     # ML 代码生成器
│   │   ├── ast/                 # AST 定义
│   │   │   ├── gast.rs          # 通用 AST 结构
│   │   │   ├── types.rs         # 类型系统和泛型
│   │   │   ├── llbc_ast.rs      # LLBC AST
│   │   │   └── ullbc_ast.rs     # ULLBC AST
│   │   ├── transform/           # 变换 passes
│   │   │   ├── mod.rs           # Pass 定义和顺序
│   │   │   ├── ullbc_to_llbc.rs # 控制流重构
│   │   │   └── ...              # 各种清理 passes
│   │   ├── ids/                 # ID 类型定义
│   │   ├── common.rs            # 通用工具
│   │   ├── export.rs            # 序列化输出
│   │   └── lib.rs               # 库入口
│   ├── tests/                   # 测试用例
│   └── Makefile
├── charon-ml/                   # OCaml 实现 (下游工具接口)
│   ├── src/
│   │   ├── generated/           # 自动生成 (勿手动修改)
│   │   │   ├── Generated_GAst.ml
│   │   │   ├── Generated_GAstOfJson.ml
│   │   │   └── ...
│   │   ├── GAst.ml              # 手写 AST 辅助
│   │   ├── PrintGAst.ml         # 手写打印工具
│   │   ├── NameMatcher.ml       # 手写名称匹配
│   │   └── ...
│   └── tests/
├── docs/                        # 文档
├── scripts/                     # 辅助脚本
└── Makefile                     # 顶层构建
```

**重点标注**：
- `translate/` 目录复杂度最高，核心翻译逻辑所在
- `charon-ml/src/generated/` 由 `make generate-ml` 自动生成，勿手动修改
- AST 修改后必须运行 `make generate-ml` 同步到 ML

## 3. Charon 整体工作机制

### 3.1 完整调用链路

**入口流程**：`charon` → `charon-driver` → Rustc → MIR → translate → transform

**各阶段关键函数**：

1. **charon 可执行文件** (`charon/src/bin/charon/main.rs`)
   - `main()` 函数：解析命令行参数
   - 设置环境变量 `RUSTC_WRAPPER=charon-driver`
   - 调用 `cargo` 或直接运行 Rustc

2. **charon-driver** (`charon/src/bin/charon-driver/main.rs`)
   - `main()` → `run_charon()` 入口
   - `driver::run_rustc_driver()` 启动 Rustc 编译
   - Rustc 回调配置在 `driver.rs` 中

3. **driver.rs 关键配置** (`charon/src/bin/charon-driver/driver.rs`)
   - `run_rustc_driver()` 设置 Rustc 回调
   - `after_analysis()` 回调：MIR 生成完成后被调用
   - 在此调用 `translate_crate::translate()` 开始翻译

4. **翻译调度** (`translate/translate_crate.rs`)
   - `translate()` 函数：翻译主入口
   - 创建 `TranslateCtx` 上下文
   - 初始化并启动翻译循环

5. **变换阶段** (回到 `charon-driver/main.rs`)
   - 翻译完成后在 `run_charon()` 中
   - 遍历执行 `transformation_passes()`
   - 最后序列化输出

### 3.2 关键转换点代码

```rust
// 入口: charon/src/bin/charon/main.rs
fn main() {
    // 设置 RUSTC_WRAPPER 环境变量指向 charon-driver
    // 调用 cargo 或 rustc
}

// 驱动: charon/src/bin/charon-driver/main.rs
fn run_charon(options: CliOpts) {
    let ctx = driver::run_rustc_driver(&options)?;  // ← 调用 Rustc
    // 执行 transform passes
    for pass in transformation_passes(&options) {
        pass.run(&mut ctx);
    }
}

// 回调: charon/src/bin/charon-driver/driver.rs
impl Callbacks for CharonCallbacks {
    fn after_analysis(&mut self, compiler: &Compiler) {
        // Rustc 分析完成，MIR 可用
        let tcx = compiler.tcx();  // 获取类型上下文
        translate_crate::translate(tcx, options)  // ← 启动翻译
    }
}

// 翻译: charon/src/bin/charon-driver/translate/translate_crate.rs
pub fn translate(tcx: TyCtxt, options: CliOpts) -> TransformCtx {
    let mut ctx = TranslateCtx::new(tcx, options);
    ctx.enqueue_module_item(...);  // 加入起始项
    
    // ⭐ 核心翻译循环
    while let Some(item_src) = ctx.items_to_translate.pop_first() {
        ctx.translate_item(&item_src);
    }
    
    // 返回用于 transform 的上下文
}
```

## 4. 翻译阶段详细流程

### 4.1 翻译抽象算法

**核心循环** (`translate_crate.rs` 约 674 行)：
```rust
while let Some(item_src) = ctx.items_to_translate.pop_first() {
    if ctx.processed.insert(item_src.clone()) {
        ctx.translate_item(&item_src);
    }
}
```

**算法步骤**：
1. **初始化**：创建 `TranslateCtx`，包含空的 `items_to_translate` 队列
2. **种子加入**：调用 `enqueue_module_item()` 将起始项（通常是 crate 根）加入队列
3. **循环处理**：从队列中取出一项，调用 `translate_item()` 翻译
4. **依赖发现**：翻译过程中遇到依赖项，通过 `enqueue` 加入队列
5. **去重检查**：`processed` 集合确保每项只翻译一次
6. **完成条件**：队列为空时翻译结束

### 4.2 TranslateCtx 核心字段

**全局翻译上下文** (`translate/translate_ctx.rs` 约 48 行)：
```rust
pub struct TranslateCtx<'tcx> {
    pub tcx: TyCtxt<'tcx>,                          // Rustc 类型上下文
    pub hax_state: hax::State<'tcx, Base>,          // Hax 转换状态
    pub options: CliOpts,                            // 命令行选项
    
    // ⭐ 翻译调度核心
    pub items_to_translate: BTreeSet<TransItemSource>,  // 待翻译队列
    pub processed: BTreeSet<TransItemSource>,           // 已处理集合
    
    // 注册与存储
    pub id_map: HashMap<TransItemSource, AnyTransId>,   // 源 → ID 映射
    pub translated: TranslatedCrate,                     // 翻译结果存储
    
    // 错误处理
    pub errors: ErrorCtx,                                // 错误收集器
    pub translate_stack: Vec<AnyTransId>,               // 翻译栈（检测循环）
}
```

**用途说明**：
- `items_to_translate`：BTreeSet 保证确定性顺序，存储待翻译项
- `processed`：防止重复翻译同一项
- `id_map`：记录已注册项的 ID，用于引用
- `translated`：存储翻译完成的所有声明

### 4.3 enqueue vs register 机制

**两种操作模式**：

| 操作 | 函数 | 注册 ID | 加入队列 | 使用场景 |
|------|------|---------|----------|----------|
| enqueue | `register_and_enqueue` | ✓ | ✓ | 发现新依赖需要翻译 |
| register | `register_no_enqueue` | ✓ | ✗ | 仅需引用ID不需翻译内容 |

**register_and_enqueue** (`translate_crate.rs` 约 289 行)：
- 分配或获取 ID
- 插入 `items_to_translate` 队列
- 返回 ID 供引用

**register_no_enqueue** (`translate_crate.rs` 约 329 行)：
- 仅分配或获取 ID
- 不加入翻译队列
- 用于 opaque 项或已知存在的项

**典型调用场景**：
```rust
// 场景 1: 翻译函数时遇到调用
let callee_id = ctx.register_and_enqueue(span, callee_src); // 需要翻译被调用函数

// 场景 2: 引用 opaque 外部类型
let type_id = ctx.register_no_enqueue(span, type_src);  // 只需 ID 不翻译内容
```

### 4.4 translate_item 分派

**项分派流程** (`translate_items.rs` 约 13 行)：
```rust
pub(crate) fn translate_item(&mut self, item_src: &TransItemSource) {
    match item_src.kind {
        TransItemSourceKind::Fun => self.translate_function(...),
        TransItemSourceKind::Global => self.translate_global(...),
        TransItemSourceKind::Type => self.translate_type(...),
        TransItemSourceKind::TraitDecl => self.translate_trait_decl(...),
        TransItemSourceKind::TraitImpl(_) => self.translate_trait_impl(...),
        // ...其他类型
    }
}
```

**RustcItem 结构**：
```rust
pub enum RustcItem {
    Poly(hax::DefId),       // 泛型版本
    Mono(hax::ItemRef),     // 单态化版本
}
```

**TransItemSourceKind 分类**：
- `Fun`：函数声明
- `Global`：全局常量/静态变量
- `Type`：类型定义 (struct/enum/union)
- `TraitDecl`：trait 声明
- `TraitImpl`：trait 实现
- `ClosureMethod`：闭包方法
- ...更多特殊类型

## 5. translate 模块深度剖析

### 5.1 ItemTransCtx 增强上下文

**项级翻译上下文** (`translate_ctx.rs` 约 74 行)：
```rust
pub(crate) struct ItemTransCtx<'tcx, 'ctx> {
    pub t_ctx: &'ctx mut TranslateCtx<'tcx>,     // 全局上下文引用
    pub item_src: TransItemSource,                // 当前翻译项
    pub item_id: Option<AnyTransId>,              // 当前项 ID
    
    // ⭐ 泛型绑定核心
    pub binding_levels: BindingStack<BindingLevel>,  // 绑定层级栈
    pub parent_trait_clauses: Vector<TraitClauseId, TraitClause>,
    pub item_trait_clauses: HashMap<TraitItemName, Vector<TraitClauseId, TraitClause>>,
}
```

**binding_levels 作用**：
- 管理嵌套泛型作用域
- 栈结构：栈顶是最内层绑定器
- 支持 `DeBruijnIndex` 变量查找

### 5.2 泛型处理机制

**Early Bound vs Late Bound**：
- **Early Bound**：项级泛型参数 `<T>`，编译时已知，可单态化
- **Late Bound**：局部量化 `for<'a>`，运行时绑定，保留 binder

**单态化策略**：
- Early Bound 类型参数：替换为具体类型
- Late Bound 生命周期：保留在 `RegionBinder<T>` 中
- 常量泛型：视情况实例化

### 5.3 translate_def_generics 详解

**四重职责** (`translate_generics.rs` 约 406 行)：

1. **收集参数**：
   ```rust
   // 从 hax::FullDef 提取泛型声明
   for param in def.generics.params {
       match param.kind {
           ParamKind::Type => binding_level.push_type_var(...),
           ParamKind::Lifetime => binding_level.push_early_region(...),
           ParamKind::Const => binding_level.push_const_generic_var(...),
       }
   }
   ```

2. **环境设置**：
   ```rust
   // 创建新的 BindingLevel 并压栈
   let binding_level = BindingLevel::new(is_item_binder);
   self.binding_levels.push(binding_level);
   ```

3. **Early 替换**：
   ```rust
   // 更新类型变量映射
   for (rust_id, our_id) in type_vars_map {
       // 后续类型翻译时查找此映射进行替换
   }
   ```

4. **Late 生命周期 binder**：
   ```rust
   // 为 Late Bound 生命周期创建 RegionBinder
   if has_late_bound_regions {
       RegionBinder {
           regions: late_bound_regions,
           skip_binder: inner_value,
       }
   }
   ```

**GenericParams 结构** (`types.rs` 约 248 行)：
```rust
pub struct GenericParams {
    pub regions: Vector<RegionId, RegionVar>,                    // 生命周期参数
    pub types: Vector<TypeVarId, TypeVar>,                       // 类型参数
    pub const_generics: Vector<ConstGenericVarId, ConstGenericVar>, // 常量参数
    pub trait_clauses: Vector<TraitClauseId, TraitClause>,      // trait 约束
    pub regions_outlive: Vec<RegionBinder<RegionOutlives>>,
    pub types_outlive: Vec<RegionBinder<TypeOutlives>>,
    pub trait_type_constraints: Vector<TraitTypeConstraintId, RegionBinder<TraitTypeConstraint>>,
}
```

**GenericArgs 结构** (`types.rs` 约 184 行)：
```rust
pub struct GenericArgs {
    pub regions: Vector<RegionId, Region>,                       // 生命周期实参
    pub types: Vector<TypeVarId, Ty>,                           // 类型实参
    pub const_generics: Vector<ConstGenericVarId, ConstGeneric>, // 常量实参
    pub trait_refs: Vector<TraitClauseId, TraitRef>,           // trait 实例
}
```

**映射关系**：`GenericParams` 定义形式参数，`GenericArgs` 提供实际参数，ID 一一对应

### 5.4 DeBruijnIndex 系统

**定义** (`types/vars.rs` 约 31 行)：
```rust
pub struct DeBruijnId {
    pub index: usize,  // 从内向外计数，0 表示最内层
}
```

**为何从 0 开始**：
- 最内层绑定器访问频率最高
- 0 起点减少索引计算开销
- 符合直觉：当前作用域即 index 0

**DeBruijnVar 结构**：
```rust
pub enum DeBruijnVar<Id> {
    Bound(DeBruijnId, Id),  // 绑定变量: (层级, 变量ID)
    Free(Id),                // 自由变量: 顶层参数
}
```

**嵌套示例**：
```rust
fn f<'a>(                              // 层级 2
    x: for<'b> fn(                     // 层级 1
        for<'c> fn(&'a u8, &'b u16, &'c u32)  // 层级 0
    )
) {}
```
引用表示：
- `'c`: `Bound(0, c)` - 最内层
- `'b`: `Bound(1, b)` - 中间层
- `'a`: `Free(a)` - 顶层（经过 `unbind_item_vars` pass 后）

### 5.5 Binder vs RegionBinder

**RegionBinder** (`types.rs` 约 198 行)：
```rust
pub struct RegionBinder<T> {
    pub regions: Vector<RegionId, RegionVar>,  // 仅绑定生命周期
    pub skip_binder: T,
}
```

**Binder** (`types.rs` 约 227 行)：
```rust
pub struct Binder<T> {
    pub params: GenericParams,  // 绑定完整泛型参数
    pub skip_binder: T,
    pub kind: BinderKind,
}
```

**使用场景**：
- `RegionBinder`：`for<'a>` 函数指针类型
- `Binder`：trait 方法（可能有类型参数）

### 5.6 DeclRef 与泛型实参

**DeclRef 结构** (`gast.rs` 约 205 行)：
```rust
pub struct FunDeclRef {
    pub id: FunDeclId,              // 函数声明 ID
    pub generics: BoxedArgs,        // 泛型实参
}
```

**优势**：
- 明确区分声明 vs 使用
- 支持同一声明的多个实例化
- 携带完整类型信息

**对比**：
- 仅用 `FunDeclId`：无法区分 `Vec<u32>` vs `Vec<String>`
- 使用 `FunDeclRef`：完整记录实例化信息

### 5.7 函数体翻译流程

**完整路径**：
```
Rustc MIR 
  ↓ Hax 转换
Hax MIR
  ↓ translate_function_signature (translate_functions.rs)
签名 (参数/返回类型)
  ↓ translate_def_generics (translate_generics.rs)
泛型环境设置
  ↓ translate_body (translate_bodies.rs)
函数体 → ULLBC
  ↓ transform passes
清理和优化
  ↓ ullbc_to_llbc
控制流重构 → LLBC
```

**关键函数**：
- `translate_function_signature`：处理签名和泛型
- `translate_body`：转换 MIR 基本块为 ULLBC
- `translate_statement`：单个语句翻译
- `translate_operand`/`translate_place`：表达式组件

## 6. transform 阶段概览

**定位**：结构清理与规范化，为下游工具准备干净的 LLBC

**Pass 分类**：
- **Normalize**：规范化表示（如 `expand_associated_types`）
- **Simplify**：简化结构（如 `remove_unit_locals`）
- **Sanity**：完整性检查（如 `check_generics`）

**执行顺序** (`transform/mod.rs`)：
```rust
INITIAL_CLEANUP_PASSES      // 初始清理
  → ULLBC_PASSES            // ULLBC 专用
  → ullbc_to_llbc           // 控制流重构
  → LLBC_PASSES             // LLBC 专用
  → SHARED_FINALIZING_PASSES // 最终清理
  → FINAL_CLEANUP_PASSES    // 完整性检查
```

**关键 Pass 示例**：
- `monomorphize`：单态化实例化
- `reorder_decls`：依赖图排序
- `reconstruct_asserts`：重构断言
- `ops_to_function_calls`：操作符转函数调用

## 7. Trait/Impl 设计

**核心策略**：方法独立存在 + 引用机制

**方法存储**：
```rust
// 方法作为独立 FunDecl 存储
pub struct FunDecl {
    pub def_id: FunDeclId,
    pub generics: GenericParams,
    // ...
}

// Trait 通过 Binder<FunDeclRef> 引用方法
pub struct TraitDecl {
    pub methods: Vec<(TraitItemName, Binder<FunDeclRef>)>,
    // ...
}
```

**泛型"平铺"策略**：
```rust
// Rust 代码
trait MyTrait<T> {
    fn method(&self, x: T);
}
impl<U: Clone> MyTrait<U> for Vec<U> { ... }

// Charon 表示
TraitImpl {
    generics: [U],           // impl 块泛型
    impl_trait: TraitDeclRef {
        id: MyTrait,
        generics: [U],       // trait 实例化参数
    },
    // method 独立存储，带有 [U] 泛型参数
}
```

## 8. 构建与运行

### 基本构建
```bash
make build          # 发布模式构建 (Rust + OCaml)
make build-dev      # 调试模式构建
make test           # 运行测试
```

### 使用示例
```bash
# 生成人类可读 LLBC
./bin/charon rustc --print-llbc -- file.rs

# 生成 JSON 文件
./bin/charon rustc --dest-file output.llbc -- file.rs

# 只生成 ULLBC
./bin/charon rustc --ullbc --dest-file output.ullbc -- file.rs
```

### 日志调试
```bash
# 启用所有 trace 日志
RUST_LOG=trace ./bin/charon ...

# 过滤特定模块
RUST_LOG=charon::translate=debug ./bin/charon ...

# 精确控制
RUST_LOG=charon::translate::translate_generics=trace ./bin/charon ...
```

## 9. 调试与打印

### trace! 宏使用
```rust
// 基础追踪
trace!("Processing item: {item_id:?}");

// 上下文感知打印
trace!("{}", item_ctx.with_ctx(|| format!("Current generics: {generics}")));
```

### 定位泛型/生命周期错误三步法

**步骤 1: 打印 binding_levels**
```rust
trace!("Current binding levels: {:#?}", ctx.binding_levels);
// 输出栈结构，检查层级是否正确
```

**步骤 2: 检查变量映射**
```rust
trace!("Type vars map: {:#?}", binding_level.type_vars_map);
trace!("Early region vars: {:#?}", binding_level.early_region_vars);
```

**步骤 3: 验证 DeclRef**
```rust
trace!("DeclRef - id: {:?}, generics: {:#?}", decl_ref.id, decl_ref.generics);
// 检查泛型实参是否完整匹配
```

## 10. charon-ml 简述

**作用**：为 OCaml 下游工具生成 AST 类型定义和反序列化函数

**命令**：
```bash
make generate-ml    # 重新生成 charon-ml/src/generated/
```

**机制对比**：
- **自动生成**：`Generated_*.ml` - AST 类型 + JSON 解析器
- **手写维护**：`Print*.ml`, `NameMatcher.ml` - 工具函数

**修改 AST 步骤**：
1. 修改 `charon/src/ast/*.rs`
2. 运行 `make generate-ml`
3. **勿直接修改** `charon-ml/src/generated/` 内文件
4. 如需 ML 侧扩展，在 `charon-ml/src/` 下手写新文件

## 11. 开发实践与常见陷阱

### 必跑检查
```bash
make test           # 完整测试套件
cargo test          # 仅 Rust 单元测试
```

### 新增 Pass 最小步骤
1. 在 `transform/` 下创建 `my_pass.rs`
2. 实现 `UllbcPass` 或 `LlbcPass` trait：
   ```rust
   pub struct MyPass;
   impl UllbcPass for MyPass {
       fn transform_body(&self, ctx: &mut TransformCtx, body: &mut ExprBody) {
           // 实现逻辑
       }
       fn name(&self) -> &str { "MyPass" }
   }
   ```
3. 在 `transform/mod.rs` 相应数组中注册
4. 添加测试用例

### 常见陷阱与解决方案

**绑定层级错误**：
- 症状：`DeBruijnIndex` 计算错误导致变量查找失败
- 解决：使用 `lookup_param` 而非手动计算索引

**重复 enqueue**：
- 症状：相同项多次加入队列
- 解决：优先用 `register_no_enqueue`，确认是否需要翻译

**泛型实例化时机**：
- 症状：Early Bound 过早替换导致信息丢失
- 解决：在 `translate_def_generics` 后再进行类型替换

**生命周期 binder 丢失**：
- 症状：Late Bound 生命周期被错误单态化
- 解决：检查 `RegionBinder` 是否正确保留

### 关键代码路径追踪

**翻译入口路径**：
1. `charon/main.rs:main` → 设置环境
2. `charon-driver/main.rs:run_charon` → 调用 Rustc
3. `driver.rs:after_analysis` → MIR 回调
4. `translate_crate.rs:translate` → 翻译主循环
5. `translate_items.rs:translate_item` → 分派具体项

**错误处理**：所有错误通过 `register_error!` 宏收集到 `ErrorCtx`

## 12. FAQ

**Q: 单态化后为何仍有生命周期 binder？**  
A: 仅 Early Bound 参数被单态化，Late Bound 生命周期保留在 `RegionBinder` 中。

**Q: DeBruijnIndex 有何用途？**  
A: 通过层级索引准确引用嵌套作用域中的绑定变量，避免名字冲突。

**Q: 何时触发重复 enqueue？**  
A: 当翻译过程中发现新依赖项且该项尚未在 `processed` 集合中时自动 enqueue。

**Q: transform passes 可否跳过？**  
A: 部分可选（通过命令行标志），但 `ullbc_to_llbc` 等核心 pass 必须执行。

**Q: DeclRef 相比直接 ID 的优势？**  
A: 明确携带泛型实参，支持同一声明的多种实例化，完整记录类型信息。

**Q: binding_levels vs Binder vs RegionBinder 区别？**  
A: `binding_levels` 是翻译时栈结构；`Binder` 是 AST 节点（完整泛型）；`RegionBinder` 仅处理生命周期。

**Q: 如何处理递归类型定义？**  
A: 通过 `reorder_decls` pass 检测依赖图强连通分量，识别互递归定义组。

**Q: 泛型替换何时发生？**  
A: Early Bound 在 `translate_def_generics` 中建立映射，实际替换在类型翻译时；Late Bound 保留不替换。

## 13. 术语速查表

| 英文术语 | 中文解释 | 上下文示例 |
|----------|----------|------------|
| binder | 绑定器 - 引入变量的语法结构 | `for<'a>`, `Binder<T>` |
| early bound | 早期绑定 - 编译时确定 | 类型参数 `<T>` |
| late bound | 晚期绑定 - 局部作用域 | 生命周期 `for<'a>` |
| monomorphization | 单态化 - 泛型实例化 | `Vec<u32>` |
| substitution | 替换 - 参数到实参映射 | `T` → `u32` |
| instantiation | 实例化 - 创建具体实例 | 泛型 → 具体类型 |
| visitor | 访问器 - AST 遍历模式 | `Drive`, `DriveMut` |
| pass | 变换步骤 - transform 阶段单元 | `UllbcPass` |
| DeBruijn index | 德布勒恩索引 - 嵌套绑定计数 | `Bound(1, var)` |
| enqueue | 入队 - 加入翻译队列 | `register_and_enqueue` |
| register | 注册 - 分配 ID | `register_no_enqueue` |

---

此教程从整体概览到详细机制层层递进，帮助新开发者快速理解 Charon 架构。详细实现请参考源码中的具体函数和模块。
