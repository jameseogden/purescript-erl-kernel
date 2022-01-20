let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.4-20211012-1/packages.dhall sha256:04b7cb6aaf4cc7323c2560c7b5c2f5e8459d2951997cf5084748e0f1cdbabd26

in  upstream
 with convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
 with erl-process.repo = "https://github.com/id3as/purescript-erl-process.git"
 with erl-process.version = "67787f787d3f6a0523f931e651156ec82709e7f1"
 with erl-untagged-union.version = "781b2894f9ffcc91b7aea482e435bb9284596f62"
 with erl-binary =
        { repo = "https://github.com/id3as/purescript-erl-binary.git"
        , dependencies = [ "erl-lists", "maybe", "prelude" ]
        , version = "e3a5da78a9264a800eb7bad918a58de5ac57ba4c"
        }
