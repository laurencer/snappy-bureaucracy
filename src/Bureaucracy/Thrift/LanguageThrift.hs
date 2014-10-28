{-# LANGUAGE BangPatterns #-}

module Bureaucracy.Thrift.LanguageThrift where

import           Control.Applicative
import           Control.Exception (bracket)

import           Text.Parser.Combinators
-- import           Text.Parser.Char
import           Text.Parser.Token
import           Text.Parser.Token.Highlight as Highlight
import           Text.Parser.Token.Style

import           Text.Trifecta hiding (token)

import           Data.Char
import           Data.CharSet.ByteSet as S
import qualified Data.ByteString as B
import           Data.Maybe
import qualified Data.HashSet as H

import           System.Environment (getArgs)
import           System.IO (hClose, openFile, IOMode(ReadMode))


-- | Helpers

infixl 4 <$!>

(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

-- | Records

data Document = 
  Document {
    headers :: [ Header ]
  } deriving (Show)

data Header =
  Namespace { language :: String, path :: String } |
  IncludeFile { includePath  :: String }
  deriving (Show)

-- | Parsers

document :: (Monad m, TokenParsing m) => m Document
document = Document <$> (many parseNamespace) <* skipComments <* eof

parseHeader :: (Monad m, TokenParsing m) => m Header
parseHeader = parseNamespace <* skipComments

parseNamespace :: (Monad m, TokenParsing m) => m Header
parseNamespace = do
  highlight ReservedIdentifier (symbol "namespace") <?> "namespace"
  name <- highlight Identifier (token identifier) <?> "namespace name"
  path <- highlight Identifier (token identifier) <?> "namespace path"
  return $ Namespace name path

parseInclude :: (Monad m, TokenParsing m) => m Header
parseInclude = do
  highlight ReservedIdentifier (symbol "include") <?> "include"
  name <- highlight Identifier (token identifier) <?> "filepath"
  return $ IncludeFile name


skipComments :: (Monad m, TokenParsing m) => m ()
skipComments = skipMany ((buildSomeSpaceParser (char ' ' *> return ()) scalaCommentStyle) <|> (char '\n' *> return ()))

-- | Basic Definitions

listSeparator :: (Monad m, TokenParsing m) => m Char
listSeparator = (char ',') <|> (char ';')

identifier :: (Monad m, TokenParsing m) => m String
identifier = do 
  first <- return <$> (letter <|> char '_') 
  rest  <- many (letter <|> char '_' <|> digit <|> dot)
  return $ first ++ rest

