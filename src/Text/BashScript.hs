{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.BashScript (
  BashScript (),
  BashFragment (),
  stmt,
  showBash,
  asBashLine,
  encodeBashScript,
  raw,
  var,
  shellEscape,
  cmdLine,
  expandCmd,
  commentLine,
  shebang,
  emptyLine,
  escapeRawText,
  exportVar,
) where

import Data.ByteString.Builder qualified as BB
import Data.Coerce (coerce)
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Semigroup.Foldable (foldMap1, intercalateMap1)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Text.ShellEscape qualified as Shell

-- | Bash Script, concatenated as lines
newtype BashScript = BashScript BB.Builder
  deriving (Show, Generic)
  deriving newtype (Semigroup)

-- | Pre-escaped bash string fragment
newtype BashFragment = BashFragment {getFragment :: BB.Builder}
  deriving (Show, Generic)
  deriving newtype (Semigroup, Monoid)

raw :: T.Text -> BashFragment
raw = BashFragment . BB.byteString . TE.encodeUtf8

instance IsString BashFragment where
  fromString = shellEscape . T.pack
  {-# INLINE fromString #-}

shebang :: BashScript
shebang = asBashLine $ raw "#!/usr/bin/env bash"

encodeBashScript :: BashScript -> BB.Builder
encodeBashScript = coerce

expandCmd :: BashFragment -> [BashFragment] -> BashFragment
expandCmd cmd args =
  raw [trimming|"$(|] <> callFragment cmd args <> raw [trimming|)"|]

showBash :: (Show a) => a -> BashFragment
showBash = fromString . show

asBashLine :: BashFragment -> BashScript
asBashLine = BashScript . (<> "\n") . getFragment

commentLine :: Text -> BashScript
commentLine =
  maybe
    (asBashLine $ BashFragment "#")
    (foldMap1 (asBashLine . BashFragment . ("# " <>) . BB.byteString . TE.encodeUtf8))
    . NE.nonEmpty
    . T.lines

stmt :: [BashFragment] -> BashScript
stmt = asBashLine . BashFragment . mconcat . L.intersperse " " . map getFragment

cmdLine :: BashFragment -> [BashFragment] -> BashScript
cmdLine = fmap asBashLine . callFragment

exportVar :: Text -> BashFragment -> BashScript
exportVar v val =
  asBashLine $
    BashFragment $
      "export " <> getFragment (raw v) <> "=" <> getFragment val

callFragment :: BashFragment -> [BashFragment] -> BashFragment
callFragment cmd args = BashFragment $ intercalateMap1 " " getFragment $ cmd NE.:| args

shellEscape :: Text -> BashFragment
shellEscape = BashFragment . BB.byteString . Shell.bytes . Shell.bash . TE.encodeUtf8

escapeRawText :: Text -> Text
escapeRawText = TE.decodeUtf8 . Shell.bytes . Shell.bash . TE.encodeUtf8

var :: Text -> BashFragment
var v = BashFragment $ textToBuilder [trimming|"$${${v}}"|]

textToBuilder :: Text -> BB.Builder
textToBuilder = BB.byteString . TE.encodeUtf8

emptyLine :: BashScript
emptyLine = asBashLine ""
