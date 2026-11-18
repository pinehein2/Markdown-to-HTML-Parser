module Assignment (markdown, convertADTHTML, getTime, htmlParser, markdownParser) where

import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Instances        (Parser (..))
import           Parser           (is, isNot, tok, spaces1, inlineSpace, inlineSpace1, alpha, int, 
                                  noneof, spaces, string, eof)
import           Control.Applicative (Alternative (many, some, (<|>)))

---------------------------------------------------------------------------
-- Naming Convention:
-- Datatypes = UpperCamelCase
-- Functions = lowerCamelCase
---------------------------------------------------------------------------

data ADT = ItalicADT ADT
         | BoldADT ADT
         | StrikethroughADT ADT
         | LinkADT (ADT, String)
         | InlineCodeADT String
         | FootnoteADT Int
         | ImageADT (String, String, String)
         | FootnoteReferenceADT (Int, String)
         | CharADT Char
         | PlainTextADT String --Plain text without any modifiers
         | FreeTextADT [ADT] --Free text with modifiers
         | NormalTextADT [ADT] --text with modifiers that isn't free text
         | HeadingsADT (String, ADT)
         | AltHeadingsADT (ADT, String)
         | BlockQuotesADT [ADT]
         | CodeADT (String, String)
         | OrderedSubListADT (ADT, [ADT], String)
         | OrderedListADT (ADT, [ADT])
         | OrderedListItemADT (ADT, ADT)
         | TableADT (ADT, [ADT])
         | TableHeaderADT [ADT]
         | TableRowsADT [ADT]
         | TableCellADT [ADT]
         | SVGADT (Int, Int, [ADT])
         | ShapeADT (String, [(String, String)])
         | LineADT ADT --A line in the markdown text
         | MarkdownADT [ADT]
         | HTMLADT (String, ADT) --HTML wrapping for markdown text
         | TestADT ADT --Test HTML wrapping for markdown text
         | Empty
  deriving (Show, Eq)

---------------------------------------------------------------------------
-- ADT to HTML converter

convertADTHTML :: ADT -> String

convertADTHTML (ItalicADT text) = "<em>" ++ convertADTHTML text ++ "</em>"

convertADTHTML (BoldADT text) = 
      "<strong>" ++ convertADTHTML text ++ "</strong>"

convertADTHTML (StrikethroughADT text) = 
      "<del>" ++ convertADTHTML text ++ "</del>"

convertADTHTML (LinkADT (text, url)) = 
      "<a href=\"" ++ url ++ "\">" ++ convertADTHTML text ++ "</a>"

convertADTHTML (InlineCodeADT text) = "<code>" ++ text ++ "</code>"

convertADTHTML (FootnoteADT num) =  
      "<sup><a id=\"fn1ref\" href=\"#fn1\">" ++ show num ++ "</a></sup>"

convertADTHTML (ImageADT (altText, url, captionText)) = 
      "    <img src=\"" ++ url ++ "\" alt=\"" ++ altText ++ 
      "\" title=\"" ++ captionText ++ "\">"

convertADTHTML (FootnoteReferenceADT (num, text)) = 
      "    <p id=\"fn" ++ show num ++ "\">" ++ text ++ "</p>"

convertADTHTML (CharADT letter) = show letter

convertADTHTML (PlainTextADT text) = text

convertADTHTML (FreeTextADT adts) = 
      "    <p>" ++ foldr (\a x -> convertADTHTML a ++ x) "" adts ++ "</p>"

convertADTHTML (NormalTextADT adts) = 
      foldr (\a x -> convertADTHTML a ++ x) "" adts 

convertADTHTML (HeadingsADT (level, heading)) = 
      "    <h" ++ a ++ ">" ++ convertADTHTML heading ++ 
      "</h" ++ a ++ ">" where a = show (min (length level) 6)

convertADTHTML (AltHeadingsADT (heading, level)) = 
      if level == "==" 
      then "    <h1>" ++ convertADTHTML heading ++ "</h1>" 
      else "    <h2>" ++ convertADTHTML heading ++"</h2>"

convertADTHTML (BlockQuotesADT quotes) = 
      "    <blockquote>\n" ++ foldr (\a x -> "    " ++ 
      convertADTHTML a ++ "\n" ++ x) "" quotes ++ "    </blockquote>"

convertADTHTML (CodeADT (lang, code)) = 
      ("    <pre><code" ++ a ++ ">") ++ code ++ "</code></pre>"
      where a = if lang == "" 
            then "" 
            else " class=\"language-" ++ lang ++ "\""

convertADTHTML (OrderedSubListADT (head, items, spacing)) = 
      "\n" ++ 
            (spacing ++ "        <ol>\n        " ++ spacing ++ 
            convertADTHTML head ++ "\n        " ++ spacing) ++ 
      foldr (\a x -> convertADTHTML a ++ "\n        " ++ spacing ++ x) 
            "" items 
      ++ "</ol>\n        "

convertADTHTML (OrderedListADT (head, items)) = 
      ("    <ol>\n    " ++ convertADTHTML head ++ "\n    ") ++ 
      foldr (\a x -> convertADTHTML a ++ "\n    " ++ x) "" items 
      ++ "</ol>"

convertADTHTML (OrderedListItemADT (item, list)) = 
      "    <li>" ++ convertADTHTML item ++ convertADTHTML list ++ "</li>"

convertADTHTML (TableADT (headers, rows)) = 
      "    <table>\n" ++ convertADTHTML headers ++ 
      foldr (\a x -> convertADTHTML a ++ x) "" rows 
      ++ "    </table>"

convertADTHTML (TableHeaderADT headers) = 
      "        <tr>\n" ++ 
      foldr (\a x -> "            <th>" ++ 
            convertADTHTML a ++ "</th>\n" ++ x)
            "" headers 
      ++ "        </tr>\n"

convertADTHTML (TableRowsADT items) = 
      "        <tr>\n" ++ foldr (\a x -> "            <td>" ++ 
      convertADTHTML a ++ "</td>\n" ++ x) "" items 
      ++ "        </tr>\n"

convertADTHTML (TableCellADT adts) = unwords (map convertADTHTML adts)

convertADTHTML (SVGADT (width, height, shape)) = 
      "    <svg width=\"" ++ show width ++ "\" height=\"" ++ 
      show height ++ "\">\n    " ++ 
      foldr (\a x -> convertADTHTML a ++ "\n" ++ x) "" shape ++ "    </svg>"

convertADTHTML (ShapeADT (shape, params)) = 
      "    <" ++ shape ++ " " ++ 
      foldr (\(key,value) x -> key ++ "=\"" ++ value ++ "\" " ++ x) "" params
      ++ ">"

convertADTHTML (LineADT adt) = convertADTHTML adt ++ "\n"

convertADTHTML (MarkdownADT adts) = 
      foldr (\a x -> convertADTHTML a ++ x) "" adts

convertADTHTML (HTMLADT (title, markdown)) = 
      a ++ convertADTHTML markdown ++ b
      where a = "<!DOCTYPE html>\n" ++ 
                  "<html lang=\"en\">\n" ++ "\n" ++ "<head>\n" ++ 
                  "    <meta charset=\"UTF-8\">\n" ++ "    <title>" ++ 
                  c ++ "</title>\n" ++ "</head>\n" ++ "\n" ++ "<body>\n"
                  where c = if title == "" then "test" else title
            b = "</body>\n" ++ "\n" ++ "</html>\n"

convertADTHTML (TestADT markdown) = 
      a ++ convertADTHTML markdown ++ b
      where a = "<!DOCTYPE html>\n" ++ "<html lang=\"en\">\n" ++
                  "\n" ++ "<head>\n" ++ "    <meta charset=\"UTF-8\">\n" ++
                  "    <title>Test</title>\n" ++ 
                  "</head>\n" ++ "\n" ++ "<body>\n"
            b = "</body>\n" ++ "\n" ++ "</html>\n"

convertADTHTML Empty = ""

---------------------------------------------------------------------------
-- Composed Markdown Parser

-- Markdown parser for test cases
markdownParser :: Parser ADT
markdownParser = TestADT <$> markdown

-- Markdown parser for custom titles
htmlParser :: String -> Parser ADT 
htmlParser s = HTMLADT <$> ((,) <$> pure s <*> markdown)

-- Base markdown parser without outer HTML code
markdown :: Parser ADT
markdown = MarkdownADT <$> 
      many (LineADT <$> 
            ((table 
            <|> svg
            <|> footnoteReference
            <|> orderedList
            <|> code
            <|> headings 
            <|> headingsAlt 
            <|> image 
            <|> blockQuotes 
            <|> freeText
            ) 
      <* optionalNextLine) <|> emptyLine)

---------------------------------------------------------------------------
-- Part A Parsers

italic :: Parser ADT
italic = ItalicADT <$> nestedSingleBracketParser ('_', '_')

bold :: Parser ADT
bold = BoldADT <$> nestedDoubleBracketParser ('*', '*')

strikethrough :: Parser ADT
strikethrough = StrikethroughADT <$> nestedDoubleBracketParser ('~', '~')

-- Parses links (Link Text, URL)
link :: Parser ADT
link = LinkADT <$> 
      ((,) <$> 
            tok (nestedSingleBracketParser ('[', ']')) 
            <*> 
            singleBracketParser ('(', ')') )

inlineCode :: Parser ADT
inlineCode = InlineCodeADT <$> singleBracketParser ('`', '`')

footnote :: Parser ADT
footnote = FootnoteADT <$> (spaces *> string "[^" *> int <* is ']')

-- Parses images (Alt Text, URL, Caption Text)
image :: Parser ADT 
image = ImageADT <$> 
      ((,,) <$> 
            (is '!' *> singleBracketParser ('[', ']') <* inlineSpace) 
            <*> 
            (is '(' *> many (isNot ' ') <* inlineSpace) 
            <*> 
            (singleBracketParser ('"','"') <* inlineSpace <* is ')') <* many (isNot '\n')
      )

-- Parses footnote references (Number, Reference Text)
footnoteReference :: Parser ADT
footnoteReference = FootnoteReferenceADT <$> 
      ((,) <$> 
            (spaces *> string "[^" *> int <* string "]:" <* inlineSpace) 
            <*> 
            many (isNot '\n')
      )

normalText :: Parser ADT
normalText = PlainTextADT <$> some (noneof "_*~[(!`\n")

freeText :: Parser ADT
freeText = FreeTextADT <$> 
      some (textModifier 
            <|> normalText 
            <|> errorHandling
            )

plainText :: Parser ADT
plainText = NormalTextADT <$> 
      some (textModifier
            <|> normalText 
            <|> errorHandling
            )

nestedPlainText :: Parser ADT
nestedPlainText = NormalTextADT <$> 
      some (textModifier
            <|> normalText
            )

textModifier :: Parser ADT
textModifier = italic 
      <|> bold 
      <|> strikethrough 
      <|> link 
      <|> inlineCode 
      <|> footnote
      <|> svg

-- Parses headings (Heading level, Heading)
headings :: Parser ADT
headings = HeadingsADT <$> 
      ((,) <$> 
            (inlineSpace *> some (is '#')) 
            <*> 
            (inlineSpace1 *> plainText)
      )

-- Parses alternate headings (Heading, Heading level)
headingsAlt :: Parser ADT
headingsAlt = AltHeadingsADT <$> 
      ((,) <$> 
            (plainText <* is '\n') 
            <*> 
            ((string "==" <|> string "--") <* many (isNot '\n'))
      )

-- Parses block quotes [Quotes]
blockQuotes :: Parser ADT
blockQuotes = BlockQuotesADT <$> 
      some (inlineSpace *> 
            is '>' *> 
            inlineSpace *> 
            freeText <* 
            optionalNextLine
            )

-- Parses code blocks (Language, Code)
code :: Parser ADT
code = CodeADT <$> 
      ((,) <$> 
            (inlineSpace *> string "```" *> many (isNot '\n') <* is '\n') 
            <*> 
            (some ((isNot '\n' <* string "\n```") <|> isNot '\n' <|> is '\n') 
                  <* optionalNextLine)
      )

-- Parses the rest of the items in a list (Item, Optional nested list)
orderedListItem :: Parser ADT
orderedListItem = OrderedListItemADT <$> 
      ((,) <$> 
            (int *> is '.' *> inlineSpace1 *> plainText) 
            <*> 
            ((is '\n' *> orderedSubList "    ") <|> Empty <$ many(is '\n'))
      )

-- Parses the first item in a list (Item, Optional nested list)
orderedListHead :: Parser ADT
orderedListHead = OrderedListItemADT <$> 
      ((,) <$> 
            (string "1." *> inlineSpace1 *> plainText) 
            <*> 
            ((is '\n' *> orderedSubList "    ") <|> Empty <$ is '\n')
      )

-- Parses nested lists (Head, Items, Spacing)
orderedSubList :: String -> Parser ADT
orderedSubList s = OrderedSubListADT <$> 
      ((,,) <$> 
            (string s *> orderedListHead) 
            <*> 
            many (orderedSubList (s ++ "    ") 
                  <|> (string s *> orderedListItem <* many (is '\n'))) 
            <*> 
            pure s
      )

-- Parses the ordered list (Head, Items)
orderedList :: Parser ADT
orderedList = OrderedListADT <$> 
      ((,) <$> 
            orderedListHead 
            <*> 
            many (orderedListItem <* many (is '\n')))

-- Parses each row in a table 
tableRows :: Parser [ADT] 
tableRows = inlineSpace *> is '|' *> 
      many (inlineSpace *> (TableCellADT <$> some ((textModifier <* inlineSpace) 
            <|> (PlainTextADT <$> some (noneof " \n|")) <* inlineSpace) <* many (is '|'))) 
      <* many (is '\n')

-- Parses a column divider in a table
tableDivider :: Parser String
tableDivider = some (
            inlineSpace *> 
            is '|' <* 
            inlineSpace <* 
            string "---" <* 
            many (isNot '|')) 
      <* is '|'

-- Parses a table
table :: Parser ADT
table = TableADT <$> 
      ((,) <$> 
            (TableHeaderADT <$> tableRows) <* tableDivider <* is '\n' 
            <*> 
            some (TableRowsADT <$> tableRows)
      )

-- Parses svgs containers and the shapes within it (Width, Height, [Shapes])
svg :: Parser ADT
svg = SVGADT <$> 
      ((,,) <$> 
            (string "(svg" *> spaces1 *> string "w:" *> int) 
            <*> 
            (spaces1 *> string "h:" *> tok int) 
            <*> 
            (tok (many (tok shape)) <* is ')')
      )

-- Parses shapes with and stores the object type and key-values 
-- (Shape type, [(attribute, value)])
shape :: Parser ADT
shape = ShapeADT <$> 
      ((,) <$> 
            (is '(' *> tok(many alpha)) 
            <*> 
            many ((,) <$> 
                  some (isNot ':') 
                  <*> 
                  (is ':' *> singleBracketParser ('"','"') <* spaces)
                  ) 
            <* is ')'
      )
---------------------------------------------------------------------------
-- Error handling

-- Handles situations where there may or may 
-- not be a next line at the end of a parse
optionalNextLine :: Parser ADT
optionalNextLine = CharADT <$> is '\n' <|> Empty <$ eof

-- Handles situations where there are multiple empty lines
emptyLine :: Parser ADT
emptyLine = Empty <$ is '\n'

-- Handles situations where the all the parsers fail
errorHandling :: Parser ADT
errorHandling = CharADT <$> isNot '\n'

---------------------------------------------------------------------------
-- Helper Parsers

-- Parses nested plain text inside a single bracket
nestedSingleBracketParser :: (Char, Char) -> Parser ADT
nestedSingleBracketParser (a, b) = is a *> nestedPlainText <* is b

-- Parses nested plain text inside a double bracket
nestedDoubleBracketParser :: (Char, Char) -> Parser ADT
nestedDoubleBracketParser (a, b) = 
      is a *> nestedSingleBracketParser (a,b) <* is b

singleBracketParser :: (Char, Char) -> Parser String
singleBracketParser (a, b) = is a *> many (isNot b) <* is b

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime