{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
    opam-repository =
      {
        url = "github:ocaml/opam-repository";
        flake = false;
      };
  };
  outputs =
    { self
    , flake-utils
    , opam-nix
    , nixpkgs
    , opam-repository
    }@inputs:

    flake-utils.lib.eachDefaultSystem
      (system: {
        legacyPackages =
          let inherit (opam-nix.lib.${system}) buildOpamProject;
            scope = buildOpamProject
              { repos = [ "${opam-repository}" ]; } "forester" ./.
              { ocaml-system = "*"; };
          in
          scope;
        packages.default = self.legacyPackages.${system}."forester";
      });
}
