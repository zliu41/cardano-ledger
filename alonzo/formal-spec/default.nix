{ pkgs ? import ../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "alonzo-spec";
  texFiles = [ "alonzo-changes" ];
  meta = with lib; {
    description = "Alonzo ledger specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)
      scheme-small
      collection-latexextra
      collection-latexrecommended
      collection-mathscience
      bclogo
      ;
  };
}
