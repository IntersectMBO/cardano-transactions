# our packages overlay
pkgs: _: with pkgs; {
  cardanoTransactionsHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
