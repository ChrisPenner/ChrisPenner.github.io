{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Pandoc where

import Control.Arrow
import Control.Exception (Exception, SomeException (..))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Flinch
import Forge (Forge, reportException)
import Text.Pandoc
import Text.Pandoc.Highlighting

type PandocReader textType = textType -> PandocIO Pandoc

type PandocWriter = Pandoc -> PandocIO T.Text

-- | Reasonable options for reading a markdown file. Behaves similar to Github Flavoured
-- Markdown
defaultMarkdownOptions :: ReaderOptions
defaultMarkdownOptions =
  def {readerExtensions = exts}
  where
    exts =
      mconcat
        [ extensionsFromList
            [ Ext_yaml_metadata_block,
              Ext_fenced_code_attributes,
              Ext_auto_identifiers,
              Ext_footnotes
            ],
          githubMarkdownExtensions
        ]

-- | Reasonable options for rendering to HTML. Includes default code highlighting rules
defaultHtml5Options :: WriterOptions
defaultHtml5Options =
  def
    { writerHighlightStyle = Just tango,
      writerExtensions = writerExtensions def
    }

-- | Reasonable options for reading an org-mode file
defaultOrgModeOptions :: ReaderOptions
defaultOrgModeOptions =
  def {readerExtensions = exts}
  where
    exts =
      mconcat
        [ extensionsFromList
            [ Ext_fenced_code_attributes,
              Ext_auto_identifiers
            ]
        ]

--------------------------------------------------------------------------------

-- | Handle possible pandoc failure within the IO Monad
unPandocM :: PandocIO a -> IO a
unPandocM p = do
  result <- runIO p
  either (fail . show) return result

-- | Convert markdown text into a 'Value';
--
--   The 'Value'  has a "content" key containing rendered HTML.
--
--   Metadata is assigned on the respective keys in the 'Value'
markdownToHTML ::
  Forge T.Text Value
markdownToHTML =
  markdownToHTMLWithOpts defaultMarkdownOptions defaultHtml5Options

-- | Like 'markdownToHTML' but allows returning any JSON serializable object
markdownToHTML' ::
  (FromJSON a) =>
  Forge T.Text a
markdownToHTML' =
  markdownToHTML >>> convert

-- | Like 'markdownToHTML' but allows providing additional pandoc reader and writer options
markdownToHTMLWithOpts ::
  -- | Pandoc reader options to specify extensions or other functionality
  ReaderOptions ->
  -- | Pandoc writer options to modify output
  WriterOptions ->
  Forge
    -- \| Text for conversion
    T.Text
    Value
markdownToHTMLWithOpts rops wops =
  loadUsing (readMarkdown rops) (writeHtml5String wops)

-- | Like 'markdownToHTMLWithOpts' but returns any JSON serializable object.
markdownToHTMLWithOpts' ::
  (FromJSON a) =>
  -- | Pandoc reader options to specify extensions or other functionality
  ReaderOptions ->
  -- | Pandoc writer options to modify output
  WriterOptions ->
  -- | Text for conversion
  Forge T.Text a
markdownToHTMLWithOpts' rops wops =
  markdownToHTMLWithOpts rops wops >>> convert

-- | Convert org-mode text into a 'Value';
--
--   The 'Value'  has a "content" key containing rendered HTML.
--
--   Metadata is assigned on the respective keys in the 'Value'
orgModeToHTML ::
  Forge T.Text Value
orgModeToHTML = orgModeToHTMLWithOpts defaultOrgModeOptions defaultHtml5Options

-- | Like 'orgModeToHTML' but allows returning any JSON compatible object.
orgModeToHTML' ::
  (FromJSON a) =>
  Forge T.Text a
orgModeToHTML' =
  orgModeToHTML >>> convert

-- | Like 'orgModeToHTML' but allows providing additional pandoc reader and writer options
orgModeToHTMLWithOpts ::
  -- | Pandoc reader options to specify extensions or other functionality
  ReaderOptions ->
  -- | Pandoc writer options to modify output
  WriterOptions ->
  -- | Text for conversion
  Forge
    T.Text
    Value
orgModeToHTMLWithOpts rops wops = loadUsing (readOrg rops) (writeHtml5String wops)

-- | Like 'orgModeToHTMLWithOpts' but allows returning any JSON compatible object
orgModeToHTMLWithOpts' ::
  (FromJSON a) =>
  -- | Pandoc reader options to specify extensions or other functionality
  ReaderOptions ->
  -- | Pandoc writer options to modify output
  WriterOptions ->
  -- | Text for conversion
  Forge T.Text a
orgModeToHTMLWithOpts' rops wops =
  orgModeToHTMLWithOpts rops wops >>> convert

-- | Given a reader from 'Text.Pandoc.Readers' this creates a loader which
--   given the source document will read its metadata into a 'Value'
--   returning both the 'Pandoc' object and the metadata within an 'Action'.
--   The metadata values will be read as Markdown but rendered as plain text,
--   removing any links, pictures, and inline formatting.
makePandocReader ::
  PandocReader textType ->
  textType ->
  IO (Pandoc, Value)
makePandocReader readerFunc text =
  makePandocReaderWithMetaWriter readerFunc (writePlain def) text

-- | Given a reader from 'Text.Pandoc.Readers', and a writer from
--   'Text.Pandoc.Writers', this creates a loader which given the source
--   document will read its metadata as Markdown, then render it into a
--   'Value' using the writer, returning both the 'Pandoc' object and the
--   metadata within an 'Action'
makePandocReaderWithMetaWriter ::
  PandocReader textType ->
  PandocWriter ->
  textType ->
  IO (Pandoc, Value)
makePandocReaderWithMetaWriter readerFunc writerFunc text = do
  pdoc@(Pandoc meta _) <- unPandocM $ readerFunc text
  meta' <- flattenMeta writerFunc meta
  return (pdoc, meta')

--------------------------------------------------------------------------------

data MetadataError = MetadataParseFailure
  deriving (Show)
  deriving anyclass (Exception)

data PandocError = PandocError String
  deriving (Show)
  deriving anyclass (Exception)

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   into text using the given 'PandocWriter'. Takes a second 'PandocWriter' to render
--   metadata.
--   Returns a 'Value' wherein the rendered text is set to the "content" key and
--   any metadata is set to its respective key in the 'Value'
loadUsingMeta ::
  -- | The reader used to load the document
  PandocReader textType ->
  -- | The writer used to render the document itself
  PandocWriter ->
  -- | The writer used to process metadata.
  PandocWriter ->
  Forge textType Value
loadUsingMeta reader writer metaWriter =
  mapIO go >>> reportException
  where
    go text = do
      (pdoc, meta) <- makePandocReaderWithMetaWriter reader metaWriter text
      outText <- unPandocM $ writer pdoc
      withContent <- case meta of
        Object m -> pure . Right . Object $ KM.insert "content" (String outText) m
        -- meta & _Object . at "content" ?~ String outText
        _ -> pure (Left $ SomeException MetadataParseFailure)
      pure withContent

-- | Load in a source document using the given 'PandocReader', then render the 'Pandoc'
--   into text using the given 'PandocWriter'.
--   Returns a 'Value' wherein the rendered text is set to the "content" key and
--   any metadata is set to its respective key in the 'Value'
loadUsing ::
  PandocReader textType ->
  PandocWriter ->
  Forge textType Value
loadUsing reader writer = loadUsingMeta reader writer writer

-- | Like 'loadUsing' but allows also deserializes the 'Value' into any object
--   which implements 'FromJSON'.  Failure to deserialize will fail the Shake
--   build.
loadUsing' ::
  (FromJSON a) =>
  PandocReader textType ->
  PandocWriter ->
  Forge textType a
loadUsing' reader writer =
  loadUsing reader writer >>> convert

--------------------------------------------------------------------------------

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
--   text objects into plain strings along the way.
flattenMeta :: PandocWriter -> Meta -> IO Value
flattenMeta writer (Meta meta) = toJSON <$> traverse go meta
  where
    go :: MetaValue -> IO Value
    go (MetaMap m) = toJSON <$> traverse go m
    go (MetaList m) = toJSONList <$> traverse go m
    go (MetaBool m) = pure $ toJSON m
    go (MetaString m) = pure $ toJSON m
    go (MetaInlines m) = toJSON <$> (unPandocM . writer . Pandoc mempty . (: []) . Plain $ m)
    go (MetaBlocks m) = toJSON <$> (unPandocM . writer . Pandoc mempty $ m)

convert :: (ToJSON a, FromJSON b) => Forge a b
convert = arr (toException . fromJSON . toJSON) >>> reportException
  where
    toException = \case
      (Error err) -> Left (SomeException $ PandocError err)
      (Success val) -> Right val
