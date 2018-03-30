{ nixpkgs ? (import ~/.nix-overlay) }:

with nixpkgs;
rec {
  hphoton = haskell.lib.dontCheck (haskellPackages.callCabal2nix "hphoton" ./hphoton {});
  hphoton-correlate = haskellPackages.callCabal2nix "hphoton-correlate" ./hphoton-correlate { inherit hphoton hphoton-io; };
  hphoton-io = haskellPackages.callCabal2nix "hphoton-io" ./hphoton-io { inherit hphoton hphoton-fpga-timetagger pipes-bytestring; };
  hphoton-fcsfit = haskellPackages.callCabal2nix "hphoton-fcsfit" ./hphoton-fcsfit {};
  hphoton-fpga-timetagger = haskellPackages.callCabal2nix "hphoton-fpga-timetagger" ./hphoton-fpga-timetagger { inherit hphoton pipes-bytestring; };
  hphoton-picoquant = haskellPackages.callCabal2nix "hphoton-picoquant" ./hphoton-picoquant {};
  hphoton-tools = haskellPackages.callCabal2nix "hphoton-tools" ./hphoton-tools { inherit hphoton hphoton-fpga-timetagger mixture-model pipes-bytestring; };
  mixture-model = haskellPackages.callCabal2nix "mixture-model" ./mixture-model { };

  pipes-group = haskell.lib.doJailbreak (haskellPackages.callHackage "pipes-group" "1.0.9" { });
  pipes-bytestring = haskell.lib.doJailbreak (haskellPackages.callHackage "pipes-bytestring" "2.1.6" { inherit pipes-group; });
}
