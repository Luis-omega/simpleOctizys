run:
  cabal run

test:
  cabal test

build:
  cabal build

format:
  nix fmt

check:
  nix flake check

spell:
  nix build .\#spell-check

format-check:
  nix build .\#format-check


# haddock complains about post-qualified imports
qualify:
  find test -type f -name "*.hs" -exec sed -i -E 's/^import[[:space:]]+([A-Z][A-Za-z0-9_.]*)[[:space:]]+qualified[[:space:]]+as[[:space:]]+([A-Z][A-Za-z0-9_]*)/import qualified \1 as \2/' {} +
  find src -type f -name "*.hs" -exec sed -i -E 's/^import[[:space:]]+([A-Z][A-Za-z0-9_.]*)[[:space:]]+qualified[[:space:]]+as[[:space:]]+([A-Z][A-Za-z0-9_]*)/import qualified \1 as \2/' {} +
