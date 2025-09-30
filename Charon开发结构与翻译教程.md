# Charon 开发结构与翻译教程

本教程面向希望快速理解并修改 Charon 项目的编译器中间表示开发者。Charon 将 Rust crate 翻译为 LLBC (Low-Level Borrow Calculus) 中间表示，支持下游工具如 Aeneas 进行形式化验证。

## 1. 引言速览

Charon 工作流程：**输入 Rust crate → translate (翻译) → transform (变换) → 输出 LLBC**

- **translate 阶段**：将 Rust MIR 转换为 ULLBC (Unstructured LLBC)
- **transform 阶段**：清理和规范化 ULLBC，最终重构控制流为 LLBC
- **输出格式**：JSON 序列化的 LLBC 文件，可用 charon-ml 进一步处理

## 2. 目录结构概述

核心目录结构：

```
charon/
├── src/
│   ├── bin/
│   │   ├── charon-driver/      # 主驱动程序
│   │   │   ├── translate/      # 翻译模块 (复杂度最高)
│   │   │   └── main.rs         # 驱动入口
│   │   └── generate-ml/        # charon-ml 代码生成器
│   ├── ast/                    # AST 定义
│   │   ├── gast.rs            # 通用 AST 结构
│   │   ├── types.rs           # 类型系统和泛型
│   │   ├── llbc_ast.rs        # LLBC AST
│   │   └── ullbc_ast.rs       # ULLBC AST  
│   ├── transform/              # 变换 passes
│   ├── common.rs              # 通用工具函数
│   ├── export.rs              # 序列化导出
│   └── lib.rs                 # 库入口
├── tests/                     # 测试用例
└── Makefile                   # 构建脚本
```

**关键说明**：
- `translate/` 复杂度最高，负责从 Rust MIR 到 ULLBC 的核心翻译
- `transform/` 包含数十个清理 passes，按顺序执行
- `charon-ml/` 辅助下游 OCaml 工具，提供类型定义

## 3. 构建与运行

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

# 只生成 ULLBC (跳过控制流重构)
./bin/charon rustc --ullbc --dest-file output.ullbc -- file.rs
```

### 日志调试
```bash
# 启用所有trace日志
RUST_LOG=trace ./bin/charon ...

# 过滤特定模块
RUST_LOG=charon::translate=debug ./bin/charon ...
```

## 4. 翻译主流程

翻译流程：**driver::run_rustc_driver → enqueue/register/translate → 生成 (U)LLBC → transform passes**

关键步骤：
1. **初始化**：`TranslateCtx` 创建，注册 crate 根模块
2. **递归探索**：从根开始，`enqueue_module_item` 加入待翻译队列
3. **批量翻译**：`translate_item` 处理队列中每个 `TransItemSource`
4. **依赖发现**：翻译过程中发现新依赖，继续 `enqueue`
5. **生成输出**：完成后序列化为 LLBC/ULLBC

## 5. translate 深度剖析

### RustcItem 与分派机制

`RustcItem` 表示待翻译的 Rust 项：
```rust
pub enum RustcItem {
    Poly(hax::DefId),      // 泛型版本
    Mono(hax::ItemRef),    // 单态化版本
}
```

`TranslateSourceKind` 决定翻译类型：
- `Fun`：函数声明
- `Global`：全局常量/静态变量  
- `TraitDecl`/`TraitImpl`：trait 声明/实现
- `Type`：类型定义

### 三阶段处理机制

| 阶段 | 函数 | 作用 | 时机 |
|------|------|------|------|
| enqueue | `register_and_enqueue` | 注册ID + 加入队列 | 发现新依赖时 |
| register | `register_no_enqueue` | 仅注册ID | 已知存在但暂不翻译 |
| translate | `translate_item` | 实际翻译内容 | 队列处理时 |

**依赖触发机制**：翻译函数体时遇到函数调用 → `register_and_enqueue` 被调用函数 → 该函数进入队列等待翻译

### DeclRef 与泛型实参

`DeclRef` 结构体包含：
- `id`：声明的唯一标识符
- `generics`：泛型实参 (`GenericArgs`)

优势：明确区分声明 vs 使用，支持同一声明的多个实例化

### 泛型处理：Early Bound vs Late Bound

Rust 区分两种泛型参数绑定：
- **Early Bound**：编译时确定，可被单态化替换
- **Late Bound**：生命周期参数，保留 binder 结构

Charon 策略：Early Bound 参数实例化；Late Bound 生命周期保留在 `RegionBinder` 中

### 关键上下文结构

#### TranslateCtx 
包含全局翻译状态：
- `translated`：已翻译项的存储
- `item_sources`：项源码映射  
- `type_context`：类型系统上下文
- `errors`：错误收集器

#### ItemTransCtx
增强 `TranslateCtx`，添加：
- `binding_levels`：`BindingStack<BindingLevel>` - 泛型参数绑定栈
- `region_binders`：区域绑定器管理
- `subst_map`：替换映射表

### translate_def_generics 四重职责

此函数 (`charon/src/bin/charon-driver/translate/translate_generics.rs`) 处理项的泛型参数：

1. **收集参数**：从 hax `FullDef` 提取泛型声明，填充 `GenericParams`
2. **环境设置**：建立 `BindingLevel` 绑定环境，更新 `binding_levels` 栈
3. **Early 替换**：处理 Early Bound 类型参数替换，更新 `subst_map`
4. **Late 生命周期 binder**：为 Late Bound 生命周期创建 `RegionBinder`

**核心机制**：
- `GenericParams` 存储收集的参数：`regions`, `types`, `const_generics`, `trait_clauses`
- `GenericArgs` 提供实例化实参，结构完全对应 `GenericParams`
- `binding_levels` 栈管理嵌套作用域，每层一个 `BindingLevel`
- `DeBruijnVar<Id>` 通过 `(DeBruijnId, Id)` 对精确定位变量

### Binder 与 DeBruijnIndex

**DeBruijnIndex 定义**：从内向外计数的绑定器层级，0 表示最内层

示例：
```rust
fn f<'a>(x: for<'b> fn(for<'c> fn(&'a u8, &'b u16, &'c u32))) {}
//  ^^            ^^         ^^
//  DeBruijnIndex: 2        1         0
```

**原因**：内层绑定器更常访问，0 起点提高效率

`RegionBinder<T>` vs `Binder<T>`：
- `RegionBinder`：仅绑定生命周期参数
- `Binder`：绑定完整 `GenericParams` (类型、常量、trait子句)

### 函数体翻译流程详解

**完整路径**：MIR/Hax → `translate_function_signature` → `translate_body` → 生成 ULLBC → transform passes → LLBC

**关键步骤**：
1. **签名翻译**：`translate_function_signature` 处理参数和返回类型
2. **泛型处理**：调用 `translate_def_generics` 建立绑定环境
3. **体翻译**：`translate_body` 转换 MIR 基本块为 ULLBC
4. **局部变量**：翻译 `Locals` 包括参数、返回值、临时变量
5. **控制流**：保持基本块结构，后续 `ullbc_to_llbc` 重构

## 6. transform 概览

**定位**：结构清理与规范化，为下游工具准备干净的 LLBC

Pass 分类：
- **Normalize**：规范化表示 (如 `expand_associated_types`)
- **Simplify**：简化结构 (如 `remove_unit_locals`) 
- **Sanity**：完整性检查 (如 `check_generics`)

执行顺序：`INITIAL_CLEANUP_PASSES` → `ULLBC_PASSES` → `LLBC_PASSES` → `SHARED_FINALIZING_PASSES`

## 7. Trait/Impl 设计

**核心策略**：方法独立存在 + 引用机制

- 方法存储在 `FunDecl` 中，具有完整签名
- Trait 声明通过 `TraitDeclRef` 引用方法
- Impl 块通过 `TraitImplRef` 提供具体实现

**泛型"平铺"策略**：
```rust
// Rust
impl<T: Clone> MyTrait<T> for Vec<T> { ... }

// Charon 表示
TraitImpl {
    generics: [T],           // impl 泛型参数
    impl_trait: TraitDeclRef {
        id: MyTrait,
        generics: [T]        // trait 实例化参数
    },
    // ...
}
```

## 8. 调试与打印

### trace! 宏使用
```rust
// 基础追踪
trace!("Processing item: {item_id:?}");

// 上下文感知打印
trace!("{}", item_ctx.with_ctx(|| format!("Current generics: {generics}")));
```

### 定位泛型/生命周期错误三步法

1. **打印 binding_levels**：
   ```rust
   trace!("Current binding levels: {:#?}", ctx.binding_levels);
   ```

2. **检查 subst_map**：
   ```rust
   trace!("Substitution map: {:#?}", ctx.subst_map);
   ```

3. **验证 DeclRef**：
   ```rust
   trace!("DeclRef - id: {:?}, generics: {:#?}", decl_ref.id, decl_ref.generics);
   ```

## 9. 泛型与绑定系统详解

### Parameter vs Argument 深度解析

**GenericParams** (形式参数，声明侧)：
```rust
pub struct GenericParams {
    pub regions: Vector<RegionId, RegionVar>,           // 生命周期参数
    pub types: Vector<TypeVarId, TypeVar>,              // 类型参数  
    pub const_generics: Vector<ConstGenericVarId, ConstGenericVar>, // 常量参数
    pub trait_clauses: Vector<TraitClauseId, TraitClause>,          // trait 约束
}
```

**GenericArgs** (实际参数，使用侧)：
```rust
pub struct GenericArgs {
    pub regions: Vector<RegionId, Region>,              // 生命周期实参
    pub types: Vector<TypeVarId, Ty>,                   // 类型实参
    pub const_generics: Vector<ConstGenericVarId, ConstGeneric>, // 常量实参  
    pub trait_refs: Vector<TraitClauseId, TraitRef>,            // trait 实例
}
```

**映射关系**：`GenericParams` 中每个 `Id` 在 `GenericArgs` 中有对应实参

### Early Bound / Late Bound 机制剖析

**Early Bound 特征**：
- 在项签名中直接声明：`fn f<T>(x: T) {}`
- 编译时已知具体类型，支持单态化
- 存储在 `GenericParams` 顶层，可被 `GenericArgs` 替换

**Late Bound 特征**：  
- 局部量化声明：`fn f(x: for<'a> fn(&'a str)) {}`
- 运行时绑定，无法提前单态化
- 保存在 `RegionBinder<T>` 结构中

**处理差异**：
```rust
// Early Bound - 直接替换
fn early<T>(x: T) -> T { x }   // T 可被替换为 u32

// Late Bound - 保留绑定器
fn late(f: for<'a> fn(&'a str)) {} // 'a 保留在 RegionBinder 中
```

### DeBruijnIndex 层级系统详解

**设计原理**：内层绑定器使用频率更高，0 起点优化访问效率

**计算规则**：从当前位置向外计数到目标绑定器层级

**复杂示例**：
```rust
fn nested<'a>(                           // 层级 3
    f: for<'b> fn(                       // 层级 2  
        for<'c> fn(                      // 层级 1
            for<'d> fn(&'a str, &'b str, &'c str, &'d str)  // 层级 0
        )
    )
) {}
```
访问表示：
- `'d`: `Bound(0, d)` - 最内层
- `'c`: `Bound(1, c)` - 向外一层  
- `'b`: `Bound(2, b)` - 向外两层
- `'a`: `Free(a)` 或 `Bound(3, a)` - 顶层

### binding_levels 栈结构运作

**BindingLevel 结构** (`charon/src/bin/charon-driver/translate/translate_generics.rs`):
```rust
pub(crate) struct BindingLevel {
    pub params: GenericParams,                    // 本层绑定参数
    pub is_item_binder: bool,                     // 是否项级绑定器
    pub early_region_vars: BTreeMap<EarlyParamRegion, RegionId>,  // Early 区域映射
    pub bound_region_vars: Vec<RegionId>,         // Late 区域映射
    pub type_vars_map: HashMap<u32, TypeVarId>,  // 类型变量映射
    pub const_generic_vars_map: HashMap<u32, ConstGenericVarId>, // 常量映射
}
```

**栈操作模式**：
1. **推入**：遇到新绑定器时 `push` 新层级
2. **查找**：`lookup_param` 从栈顶向下搜索变量
3. **弹出**：退出作用域时 `pop` 恢复上层环境

### 变量查找算法

**DeBruijnVar 查找流程**：
```rust
pub(crate) fn lookup_param<Id: Copy>(
    &mut self,
    span: Span,
    f: impl for<'a> Fn(&'a BindingLevel) -> Option<Id>,
) -> Result<DeBruijnVar<Id>, Error> {
    // 从栈顶 (DeBruijnId::ZERO) 开始搜索
    for (db_id, level) in self.binding_levels.indexed_iter() {
        if let Some(id) = f(level) {
            return Ok(DeBruijnVar { db_id, id });
        }
    }
    // 未找到则报错
}
```

**查找优先级**：内层绑定器优先，实现变量遮蔽 (shadowing)

## 10. charon-ml 简述

**作用**：为 OCaml 下游工具生成 AST 类型定义和反序列化函数

**命令**：
```bash
make generate-ml    # 重新生成 charon-ml/src/generated/
```

**机制对比**：
- **自动生成**：AST 类型定义 → OCaml 类型 + JSON 解析器
- **手写维护**：`Print.ml`、`NameMatcher.ml` 等工具模块

**修改 AST 步骤**：
1. 修改 `charon/src/ast/*.rs`
2. 运行 `make generate-ml`  
3. **勿直接修改** `charon-ml/src/generated/` 内文件

## 11. 开发实践与常见陷阱

### 必跑检查
```bash
make test           # 完整测试套件
cargo test          # 仅 Rust 单元测试
```

### 新增 Pass 最小步骤
1. 在 `transform/` 下创建 `my_pass.rs`
2. 实现 `UllbcPass` 或 `LlbcPass` trait
3. 在 `transform/mod.rs` 相应数组中注册
4. 添加测试用例

### 常见陷阱与解决方案

- **绑定层级错误**：访问变量时 `DeBruijnIndex` 计算错误  
  解决：使用 `lookup_param` 而非手动计算索引
- **重复 enqueue**：相同项多次加入队列，检查 `register_no_enqueue` vs `register_and_enqueue`  
  解决：先检查 `translated` 中是否已存在
- **泛型实例化时机**：Early Bound 过早替换导致信息丢失  
  解决：在 `translate_def_generics` 后再进行替换
- **生命周期 binder 丢失**：Late Bound 生命周期被错误单态化  
  解决：检查 `RegionBinder` 是否正确保留

### 关键代码路径追踪

**翻译入口路径**：
1. `main.rs:run_charon` → `driver::run_rustc_driver`
2. `translate_crate.rs:enqueue_module_item` → 启动翻译
3. `translate_items.rs:translate_item` → 处理单个项
4. `translate_generics.rs:translate_def_generics` → 泛型处理
5. `translate_bodies.rs:translate_body` → 函数体翻译

**错误处理路径**：
所有翻译错误通过 `register_error!` 宏收集到 `ErrorCtx`，最终在 `main.rs` 中统计报告

## 12. FAQ

**为什么单态化后仍有生命周期 binder？**  
仅 Early Bound 参数被单态化，Late Bound 生命周期保留 `RegionBinder` 结构。

**DeBruijnIndex 有何用途？**  
通过层级索引准确引用嵌套作用域中的绑定变量，避免名字冲突。

**何时触发重复 enqueue？**  
当翻译过程中发现新的依赖项，且该项尚未注册时自动触发。

**transform passes 可否跳过？**  
部分可选，但 `ullbc_to_llbc` 等核心 pass 必须执行以保证输出正确性。

**DeclRef 相比直接 ID 的优势？**  
明确携带泛型实参信息，支持同一声明的多种实例化。

**binding_levels vs Binder vs RegionBinder 区别？**  
`binding_levels` 是翻译时栈结构；`Binder` 是 AST 节点；`RegionBinder` 仅处理生命周期。

**如何处理递归类型定义？**  
通过 `reorder_decls` pass 检测和重排依赖图，识别强连通分量。

**泛型替换何时发生？**  
Early Bound 参数在 `translate_def_generics` 中替换；Late Bound 保留到使用时。

## 13. 术语速查表

| 英文术语 | 中文解释 | 上下文 |
|----------|----------|--------|
| binder | 绑定器 - 引入新变量的语法结构 | `for<'a>`, `Binder<T>` |
| early bound | 早期绑定 - 编译时确定 | 类型参数 `<T>` |
| late bound | 晚期绑定 - 局部作用域 | 生命周期 `for<'a>` |
| monomorphization | 单态化 - 泛型实例化 | `Vec<u32>` |
| substitution | 替换 - 参数到实参映射 | `T` → `u32` |
| instantiation | 实例化 - 创建具体实例 | 泛型 → 具体类型 |
| visitor | 访问器 - AST 遍历模式 | `Drive`, `DriveMut` |
| pass | 变换步骤 - transform 阶段单元 | `UllbcPass` |
| DeBruijn index | 德布勒恩索引 - 嵌套绑定计数 | `Bound(1, var)` |

## 14. 实战示例与代码定位

### 简单翻译示例追踪

给定 Rust 代码：
```rust
fn add<T: std::ops::Add<Output = T>>(x: T, y: T) -> T {
    x + y
}
```

**翻译路径**：
1. `enqueue_module_item` 发现函数 `add`
2. `translate_item` 调用，`kind = TransItemSourceKind::Fun`
3. `translate_def_generics` 处理 `<T: Add<Output = T>>`：
   - 收集类型参数 `T` 到 `GenericParams.types`
   - 收集 trait 约束到 `GenericParams.trait_clauses`
4. `translate_function_signature` 翻译签名
5. `translate_body` 转换函数体为 ULLBC

**生成的 LLBC 结构**：
- `FunDecl` 包含完整泛型参数
- 函数体中 `x + y` 转换为 trait 方法调用
- Add trait 约束表示为 `TraitClause`

### 调试实战技巧

**定位泛型错误**：
```rust
// 在 translate_generics.rs 中添加
trace!("Processing generics for {:?}", def_id);
trace!("Binding levels stack: {:#?}", self.binding_levels);
```

**追踪依赖链**：
```rust  
// 在 translate_crate.rs 中添加
trace!("Enqueueing item: {:?} -> {:?}", item_src, trans_id);
```

**验证 transform 效果**：使用 `--print-ullbc` 和 `--print-llbc` 对比前后差异

这些示例基于 `charon/src/bin/charon-driver/translate/` 模块中的实际实现，帮助开发者快速定位和调试问题。