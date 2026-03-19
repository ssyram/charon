#!/usr/bin/env python3
import os
import subprocess
import sys
import argparse
import shutil

def clean_output_dir(dir_path):
    """If the directory exists, delete it and recreate an empty one"""
    if os.path.exists(dir_path):
        shutil.rmtree(dir_path)
    os.makedirs(dir_path)

def main():
    parser = argparse.ArgumentParser(description="Run charon to process std modules")
    parser.add_argument("--sysroot", help="Rust sysroot path; if not provided, read from environment variable SYSROOT")
    parser.add_argument("--failed-list", default="failed_modules.txt",
                        help="Filename to record failed modules, default is failed_modules.txt")
    args = parser.parse_args()

    # Get sysroot: priority to command line argument, then environment variable
    sysroot = args.sysroot or os.environ.get("SYSROOT")
    if not sysroot:
        print("Error: sysroot not specified. Please provide via --sysroot or set environment variable SYSROOT", file=sys.stderr)
        sys.exit(1)

    # Ensure there is a compilable Rust source file
    test_file = "file.rs"
    if not os.path.isfile(test_file):
        with open(test_file, "w") as f:
            f.write("fn main() {}\n")
        print(f"Temporary file {test_file} created")

    out_dir = "translate_std"
    # Clean and create output directory
    clean_output_dir(out_dir)

    list_file = "std_modules.txt"
    if not os.path.isfile(list_file):
        print(f"Error: file {list_file} not found", file=sys.stderr)
        sys.exit(1)

    with open(list_file, "r") as f:
        lines = f.readlines()

    failed_modules = []  # Record original strings of failed modules

    for line in lines:
        arg = line.strip()
        if not arg:
            continue

        print(f"Processing     : {arg}")

        # Generate safe filename: remove trailing "::*", then replace "::" with "_"
        if arg.endswith("::*"):
            base_arg = arg[:-3]  # Remove the last three characters "::*"
        else:
            base_arg = arg       # If not, keep as is (but all lines in file should have it)
        safe_arg = base_arg.replace("::", "_")

        cmd = [
            "./target/debug/charon",
            "rustc",
            f"--start-from={arg}",   # Note: command still uses original arg, only filename removes ::*
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
            print(f"Command execution failed ({arg}): {e}", file=sys.stderr)
            failed_modules.append(arg)  # Record failed module
            continue

        if result.returncode == 0:
            filename = os.path.join(out_dir, f"{safe_arg}.txt")
            content = result.stdout
        else:
            filename = os.path.join(out_dir, f"{safe_arg}_error.txt")
            content = result.stderr
            failed_modules.append(arg)  # Record failed module

        with open(filename, "w") as f:
            f.write(content)

        print(f"Result saved to: {filename}")

    # Write failed modules list to file in current directory
    if failed_modules:
        with open(args.failed_list, "w") as f:
            for mod in failed_modules:
                f.write(mod + "\n")
        print(f"Failed modules list saved to: {args.failed_list}")
    else:
        # If no failed modules, create an empty file or skip
        # Here choose to create an empty file to indicate completion with no failures
        with open(args.failed_list, "w") as f:
            pass
        print("All modules processed successfully, failed list is empty.")

if __name__ == "__main__":
    main()