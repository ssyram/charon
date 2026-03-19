#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import shutil

def clean_output_dir(dir_path):
    """如果目录存在，则删除并重新创建空目录"""
    if os.path.exists(dir_path):
        shutil.rmtree(dir_path)
    os.makedirs(dir_path)

def main():
    parser = argparse.ArgumentParser(description="运行 charon 处理 std 模块")
    parser.add_argument("--sysroot", help="Rust sysroot 路径，如果不提供则从环境变量 SYSROOT 读取")
    parser.add_argument("--failed-list", default="failed_modules.txt",
                        help="记录失败模块列表的文件名，默认为 failed_modules.txt")
    args = parser.parse_args()

    # 获取 sysroot：优先命令行参数，否则环境变量
    sysroot = args.sysroot or os.environ.get("SYSROOT")
    if not sysroot:
        print("错误：未指定 sysroot，请通过 --sysroot 参数或设置环境变量 SYSROOT", file=sys.stderr)
        sys.exit(1)

    # 确保存在一个可编译的 Rust 源文件
    test_file = "file.rs"
    if not os.path.isfile(test_file):
        with open(test_file, "w") as f:
            f.write("fn main() {}\n")
        print(f"已创建临时文件 {test_file}")

    out_dir = "translate_std"
    # 清空并创建输出目录
    clean_output_dir(out_dir)

    list_file = "std_modules.txt"
    if not os.path.isfile(list_file):
        print(f"错误：找不到文件 {list_file}", file=sys.stderr)
        sys.exit(1)

    with open(list_file, "r") as f:
        lines = f.readlines()

    failed_modules = []  # 用于记录失败的模块原始字符串

    for line in lines:
        arg = line.strip()
        if not arg:
            continue

        print(f"正在处理: {arg}")

        # 生成安全的文件名：去掉末尾的 "::*"，然后将 "::" 替换为 "_"
        if arg.endswith("::*"):
            base_arg = arg[:-3]  # 去掉最后的三个字符 "::*"
        else:
            base_arg = arg       # 如果没有，则保持原样（但文件中应该都有）
        safe_arg = base_arg.replace("::", "_")

        cmd = [
            "./target/debug/charon",
            "rustc",
            f"--start-from={arg}",   # 注意：命令中仍使用原始 arg，只文件名处理时去掉 ::*
            "--include=std",
            "--print-llbc",
            "--no-serialize",
            "--",
            f"--sysroot={sysroot}",
            test_file
        ]

        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=None)
        except Exception as e:
            print(f"执行命令失败 ({arg}): {e}", file=sys.stderr)
            failed_modules.append(arg)  # 记录失败模块
            continue

        if result.returncode == 0:
            filename = os.path.join(out_dir, f"{safe_arg}.txt")
            content = result.stdout
        else:
            filename = os.path.join(out_dir, f"{safe_arg}_error.txt")
            content = result.stderr
            failed_modules.append(arg)  # 记录失败模块

        with open(filename, "w") as f:
            f.write(content)

        print(f"结果已保存至: {filename}")

    # 将失败的模块列表写入当前目录下的文件
    if failed_modules:
        with open(args.failed_list, "w") as f:
            for mod in failed_modules:
                f.write(mod + "\n")
        print(f"失败模块列表已保存至: {args.failed_list}")
    else:
        # 没有失败模块时，也可以创建一个空文件或跳过
        # 这里选择创建空文件以表明运行完成但无失败
        with open(args.failed_list, "w") as f:
            pass
        print("所有模块处理成功，失败列表为空。")

if __name__ == "__main__":
    main()