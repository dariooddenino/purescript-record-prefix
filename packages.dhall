let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210506/packages.dhall sha256:d199e142515f9cc15838d8e6d724a98cd0ca776ceb426b7b36e841311643e3ef
in upstream
  with
    typelevel-eval = mkPackage
      [ "console", "effect", "leibniz", "psci-support", "record" ]
      "https://github.com/natefaubion/purescript-typelevel-eval"
      "v0.5.0"
