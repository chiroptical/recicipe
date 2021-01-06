import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-20.09";
  url = "https://github.com/nixos/nixpkgs";
  # Commit hash for nixos-20.09 as of 2020-11-24
  # `git ls-remote https://github.com/nixos/nixpkgs nixos-20.09`
  ref = "refs/heads/nixos-20.09";
  rev = "c5c6009fb436efe5732e07cd0e5692f495321752";
})
