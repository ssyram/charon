# Hax Development Environment Setup

This repository includes a joint development environment for both Charon and Hax. The setup allows you to develop and test changes to both projects simultaneously.

## Current Setup

The Charon project is configured to use a local copy of the hax development repository instead of the remote git dependency. This allows for:

- Joint development on both Charon and Hax
- Testing Charon changes against local Hax modifications
- Easier debugging and development workflow

## Directory Structure

```
charon/
├── charon/          # Charon Rust codebase
├── charon-ml/       # Charon OCaml bindings
├── hax/             # Local hax development environment (from ssyram/hax-dev)
├── ...
```

## How It Works

1. **Hax Dependency**: The `charon/Cargo.toml` file has been modified to point to the local hax directory:
   ```toml
   hax-frontend-exporter = { path = "../hax/frontend/exporter", optional = true }
   ```

2. **Git Ignore**: The `hax/` directory is added to `.gitignore` so it's not tracked as part of the Charon repository.

3. **CI Updates**: The CI script `scripts/ci-check-hax-commit.sh` has been updated to handle local hax development environment.

## Setup Instructions

If you're setting up this environment from scratch:

1. Clone the Charon repository:
   ```bash
   git clone https://github.com/ssyram/charon.git
   cd charon
   ```

2. Clone the hax development repository into the `hax/` directory:
   ```bash
   git clone https://github.com/ssyram/hax-dev.git hax
   ```

3. Build Charon with the local hax:
   ```bash
   make build-dev
   ```

## Development Workflow

### Making Changes to Hax

1. Navigate to the `hax/` directory
2. Make your changes to the hax codebase
3. Build Charon to test your changes:
   ```bash
   cd .. # Back to charon root
   cd charon && cargo build
   ```

### Making Changes to Charon

1. Make changes to the Charon codebase in `charon/`
2. Build and test as usual:
   ```bash
   cd charon && cargo build && cargo test
   ```

### Testing the Integration

Test that everything works together:

```bash
# Test basic functionality
echo 'fn main() { println!("Hello world"); }' > /tmp/test.rs
./bin/charon rustc --no-serialize --print-llbc -- /tmp/test.rs

# Run Charon tests
cd charon && cargo test
```

## Switching Back to Remote Hax

If you want to switch back to using the remote hax dependency:

1. Edit `charon/Cargo.toml` and uncomment the git dependency:
   ```toml
   hax-frontend-exporter = { git = "https://github.com/AeneasVerif/hax", branch = "main", optional = true }
   # hax-frontend-exporter = { path = "../hax/frontend/exporter", optional = true }
   ```

2. Remove the local hax directory:
   ```bash
   rm -rf hax/
   ```

3. Rebuild:
   ```bash
   cd charon && cargo update && cargo build
   ```

## Notes

- The local hax directory (`hax/`) is ignored by git and won't be committed to the Charon repository
- Both repositories maintain their own git history and can be developed independently
- CI checks have been updated to handle both local and remote hax configurations
- This setup is designed for development and testing purposes