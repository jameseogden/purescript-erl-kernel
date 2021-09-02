let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

in  upstream
 with convertable-options =
        { repo = "https://github.com/natefaubion/purescript-convertable-options"
        , dependencies = [ "effect", "maybe", "record" ]
        , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
        }
 with erl-untagged =
        { repo = "https://github.com/id3as/purescript-erl-untagged.git"
        , dependencies =
        [ "erl-atom"
        , "erl-binary"
        , "erl-lists"
        , "erl-tuples"
        , "debug"
        , "foreign"
        , "typelevel-prelude"
        , "maybe"
        , "partial"
        , "prelude"
        , "unsafe-coerce"
        ]
        ,   version = "c7ee97165d2c916e56b280eabba866a551a86e49"
        }
