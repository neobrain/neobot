{-
 Codec for de/encoding form data shipped in URL query strings
 or in POST request bodies. (application/x-www-form-urlencoded)
 (cf. RFC 3986.)
-}
module MediaWiki.Util.Codec.URLEncoder 
       ( encodeString
       , decodeString
       ) where

import qualified Codec.Binary.UTF8.String as UTF8 ( encodeString )
import MediaWiki.Util.Codec.Percent ( getEncodedChar, getDecodedChar )

encodeString :: String -> String
encodeString str = go (UTF8.encodeString str)
 where
  go "" = ""
  go (' ':xs) = '+':go xs
  go ('\r':'\n':xs) = '%':'0':'D':'%':'0':'A':go xs
  go ('\r':xs) = go ('\r':'\n':xs)
  go ('\n':xs) = go ('\r':'\n':xs)
  go (x:xs) = 
    case getEncodedChar x of
      Nothing -> x : go xs
      Just ss -> ss ++ go xs
      
decodeString :: String -> String
decodeString "" = ""
decodeString ('+':xs) = ' ':decodeString xs
decodeString ls@(x:xs) = 
  case getDecodedChar ls of
    Nothing -> x : decodeString xs
    Just (ch,xs1) -> ch : decodeString xs1
