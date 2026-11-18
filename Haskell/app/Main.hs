{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (convertADTHTML, getTime, htmlParser)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty, header, liftAndCatchIO)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]

-- Saves files
saveFile :: String -> IO ()
saveFile html = do
  putStrLn "Saving HTML"
  time <- getTime
  writeFile (time ++ ".html") html

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- gets the save status
    requestSave <- header "save"
    -- processes the save status
    save <- case requestSave of
        Nothing -> pure False
        Just x -> if x == "false" then pure False else pure True
    -- gets the new title for the HTML
    requestTitle <- header "title"
    -- processes the new title
    title <- case requestTitle of
        Nothing -> pure "Converted HTML"
        Just x -> pure x
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        titleStr = unpack title
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse (htmlParser titleStr) str) convertADTHTML

    -- Checks whether to save or not
    if save then (liftAndCatchIO $ saveFile converted_html) else return ()

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]
