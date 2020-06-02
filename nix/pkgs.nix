# our packages overlay
pkgs: _: {
  cardanoLedgerSpecsHaskellPackages = pkgs.callPackage ./haskell.nix { };
  cbor-diag = pkgs.callPackage ./pkgs/cbor-diag { };
  cddl = pkgs.callPackage ./pkgs/cddl { };
}
