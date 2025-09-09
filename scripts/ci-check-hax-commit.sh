#!/usr/bin/env bash
# Check that we're using a local hax development environment or a hax commit that's merged into main.

# Check if we're using local hax development environment by looking at Cargo.toml
if grep -q 'hax-frontend-exporter.*path.*=.*"../hax' charon/Cargo.toml; then
    echo "This PR uses local hax development environment"
    
    # Verify the local hax directory exists and is properly set up
    if [[ ! -d "hax" ]]; then
        echo "Error: hax directory not found. Please clone ssyram/hax-dev into the hax/ directory."
        exit 1
    fi
    
    if [[ ! -f "hax/frontend/exporter/Cargo.toml" ]]; then
        echo "Error: hax frontend exporter not found in expected location."
        exit 1
    fi
    
    echo "Local hax development environment is properly set up."
    exit 0
fi

# Check if we have toml2json available for remote commit checking
if ! command -v toml2json &> /dev/null; then
    echo "Warning: toml2json not available, skipping hax commit check."
    echo "Please ensure hax dependency is correctly configured."
    exit 0
fi

# Fall back to original remote commit checking logic
HAX_COMMIT="$(toml2json charon/Cargo.lock | jq -r \
    '.package[]
    | select(.name == "hax-frontend-exporter").source
    | capture("^git\\+https://github.com/(cryspen|AeneasVerif)/hax\\?branch=(?<branch>[a-z]+)#(?<commit>[a-f0-9]+)$")
    | select(.branch == "main")
    | .commit
    ')"

if [[ -z "$HAX_COMMIT" ]]; then
    echo "Error: Unable to determine hax commit from Cargo.lock"
    exit 1
fi

echo "This PR uses hax commit $HAX_COMMIT"

git clone https://github.com/AeneasVerif/hax
cd hax
HAX_MAIN="$(git rev-parse HEAD)"

if ! git merge-base --is-ancestor "$HAX_COMMIT" "$HAX_MAIN"; then
    echo "Error: commit $HAX_COMMIT is not merged into the main branch of AeneasVerif/hax."
    exit 1
fi
