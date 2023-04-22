let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230420/packages.dhall
        sha256:01f6ef030637be27a334e8f0977d563f9699543f596d60e8fb067e4f60d2e571

in  upstream
with tidy =
    { dependencies =
      [ "maybe"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy.git"
    , version = "v0.9.0"
    }
  with tidy-codegen =
    { dependencies =
      [ "language-cst-parser", "tidy"
      ]
    , repo = "https://github.com/natefaubion/purescript-tidy-codegen.git"
    , version = "v3.0.0"
    }
