{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DDG.FatHead.Util.DB
  ( Entry
  , Title
  , title
  , redirect
  , article
  , disambiguation
  , writeOutput
  ) where


import Data.Char (ord)
import Data.Csv
import qualified Data.Text as DT
import qualified Data.ByteString.Lazy as BSZ
import qualified Data.ByteString as BS
import Data.Text (Text)
import Network.URI (URI)
import Data.Monoid ((<>))
import Data.List (intercalate)


type FieldText = String


-- | The title of an entry.
newtype Title = Title { getTitle :: String } deriving (ToField)


-- | Construct an 'Entry' title from a 'String'.
title :: String -> Title
title = Title


type Abstract = String


newtype Disambiguation = Disambiguation { getDisambiguation :: [(Entry, String)] }


instance ToField Disambiguation where
  toField (Disambiguation xs) = toField . intercalate "\\n"
    $ fmap (\(e,d) -> concat ["*[[", getTitle (entryTitle e), "]], ", d, "."]) xs


newtype Categories = Categories { getCategories :: [Text] }


instance ToField Categories where
  toField (Categories { getCategories = cs }) = toField (DT.unlines cs)


instance ToField URI where
  toField = toField . show


data Entry =
  EntryArticle { articleTitle :: Title
               , articleCategories :: Maybe Categories
               , articleAbstract :: Abstract
               , articleUrl :: URI
               }
  | EntryRedirect { redirectFrom :: Title
                  , redirectTo   :: Title
                  }
  | EntryDisambiguation { disambiguationTitle :: Title
                        , disambiguationD     :: Disambiguation
                        }


entryTitle :: Entry -> Title
entryTitle a@(EntryArticle{}) = articleTitle a
entryTitle r@(EntryRedirect{}) = redirectFrom r
entryTitle d@(EntryDisambiguation{}) = disambiguationTitle d


emptyField :: Field
emptyField = toField ("" :: FieldText)


typeArticle, typeDisambiguation, typeRedirect :: (Name, Field)
typeArticle        = ("type", "A")
typeDisambiguation = ("type", "D")
typeRedirect       = ("type", "R")


instance ToNamedRecord Entry where
  toNamedRecord (EntryRedirect { redirectFrom = from, redirectTo = to }) =
    namedRecord ([ "title" .= toField from
                 , "redirect" .= toField to
                 , typeRedirect ] ++
      emptyFieldsExcept ["title", "type", "redirect"])
  toNamedRecord (EntryDisambiguation { disambiguationTitle = title
                                     , disambiguationD = disambiguation }) =
    namedRecord ([ "title" .= toField title
                 , "disambiguation" .= toField disambiguation
                 , typeDisambiguation ] ++
      emptyFieldsExcept ["title", "type", "disambiguation"])
  toNamedRecord (EntryArticle { articleTitle = title
                              , articleCategories = cs
                              , articleAbstract = a
                              , articleUrl = u }) =
    namedRecord ([ "title"      .= toField title
                 , "categories" .= toField cs
                 , "abstract"   .= toField a
                 , "souce_url"  .= toField u
                 , typeArticle ] ++
      emptyFieldsExcept ["title", "type", "categories", "abstract", "source_url"])


encodeOptions :: EncodeOptions
encodeOptions = defaultEncodeOptions { encDelimiter = tab
                                     , encIncludeHeader = True
                                     , encUseCrLf = False
                                     , encQuoting = QuoteNone
                                     }
  where tab = fromIntegral . ord $ '\t'


outputFields :: [BS.ByteString]
outputFields = [ "title" , "type", "redirect", "null1"
               , "categories", "null2", "see_also", "null3"
               , "external_links", "disambiguation"
               , "images", "abstract", "source_url"
               ]


emptyFieldsExcept :: [BS.ByteString] -> [(Name, Field)]
emptyFieldsExcept xs = fmap (`namedField` emptyField) $ filter (`notElem` xs) outputFields


outputHeader :: Header
outputHeader = header outputFields


article :: Title -> Abstract -> URI -> Categories -> Entry
article t a u cs = EntryArticle { articleTitle =  t
                             , articleCategories = Just cs
                             , articleAbstract = a
                             , articleUrl = u
                             }


-- | @redirect from to@ creates an 'Entry' that redirects
-- queries for @from@ to the entry at @to@.
redirect :: Title -> Title -> Entry
redirect from to = EntryRedirect { redirectFrom = from
                                 , redirectTo   = to
                                 }


-- | Create a disambiguation page for the given title.
disambiguation :: Title -> Disambiguation -> Entry
disambiguation t d = EntryDisambiguation { disambiguationTitle = t
                                         , disambiguationD = d
                                         }


writeOutput :: [Entry] -> IO ()
writeOutput = BSZ.writeFile "output.txt"
              . encodeByNameWith encodeOptions outputHeader
