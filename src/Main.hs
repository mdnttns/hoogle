module Main where

import Text.HTML.TagSoup
import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Char8 (unpack)
import System.Environment (getArgs)


main = do
    args <- getArgs
    rawHtml <- httpReq $ url ++ (args !! 0)
    putStrLn . handleTags . getTags $ rawHtml
    where url = "https://www.haskell.org/hoogle/?hoogle="
    
handleTags :: [Tag [Char]] -> String
handleTags []   = ""
handleTags tags = toLines ans from doc ++ (handleTags $ if tags_ == [] then [] else tail tags_)
    where 
    ans = ansTag' $ takeWhile (/= TagClose "div") $ tags
    from = fromTag . getTagsFromTo (TagOpen "div" [("class", "from")]) (TagClose "div") False $ tags
    doc = docTag . getTagsFromTo (TagOpen "div" [("class", "doc")]) (TagClose "div") False $ tags
    tags_ = (dropWhile (/= TagOpen "div" [("class","ans")])) tags
    toLines a b c = a ++ "\n" ++ b ++ "\n" ++ c ++ "\n"
    

ansTag :: [Tag [Char]] -> String
ansTag tags = first ++ second ++ third
    where
    first  = findTagText' (getTagsFromTo (TagOpen "a" [("class","dull")]) (TagClose "a") False tags)
    second = findTagText' (getTagsFromTo (TagOpen "a" [("class","a")]) (TagClose "a") False tags)
    third  = findTagText' . (getTagsFromTo (TagOpen "a" [("class","dull")]) (TagClose "a") False) 
                            . (dropWhile (~/= TagOpen "a" [("class","a")])) $ tags

                           
ansTag' :: [Tag [Char]] -> String
ansTag' tags = findTagText' tags  
                            
                            
fromTag :: [Tag [Char]] -> String
fromTag tags = findTagText' tags    


docTag :: [Tag [Char]] -> String
docTag tags = findTagText' tags

            
findTagText' :: [Tag [Char]] -> String
findTagText' []   = ""
findTagText' tags = text ++ (findTagText' . saveTail $ tags_)
    where tags_ = dropWhile (~/= TagText "") $ tags
          TagText text = if tags_ == [] then TagText "" else head tags_  
          

getTagsFromTo :: Tag [Char] -> Tag [Char] -> Bool -> [Tag [Char]] -> [Tag [Char]]
getTagsFromTo from to firstInclusive tags
    | not firstInclusive = saveTail fromTo
    | firstInclusive     = fromTo     
    where fromTo = takeWhile (~/= to) . dropWhile (~/= from) $ tags

    
saveTail [] = []
saveTail x = tail x 
    
    
getTags :: String -> [Tag [Char]]
getTags rawHtml = tags
    where 
    tags =
        getTagsFromTo (TagOpen "div" [("class","ans")]) (TagOpen "div" [("class","push")]) False $ parseTags rawHtml      
        
            
httpReq :: String -> IO String
httpReq url = do
    rawHtml <- simpleHttp url
    return . unpack . toStrict $ rawHtml