module Day2 (getTotalPaperArea, getPaperArea, getDims, getRibbonLength, getTotalRibbonLength) where
import qualified Data.Text as Text
import Data.List

getTotalPaperArea :: [String] -> Integer
getTotalPaperArea xs = foldl (+) 0 $ map getPaperArea $ map getDims xs

getTotalRibbonLength :: [String] -> Integer
getTotalRibbonLength xs = foldl (+) 0 $ map getRibbonLength $ map getDims xs

getPaperArea :: (Integer, Integer, Integer) -> Integer
getPaperArea (w,l,h) = let areas = [l*w, w*h, h*l]
                           minArea = minimum areas
                       in minArea + (foldl (\total a -> total + 2*a) 0 areas)

getRibbonLength :: (Integer, Integer, Integer) -> Integer
getRibbonLength (w,l,h) = let sortedSides = sort [w,l,h]
                              volume = w*l*h
                          in volume + (foldl (+) 0 $ map (*2) $ take 2 sortedSides)

getDims :: String -> (Integer, Integer, Integer)
getDims s = let txtDims = Text.split (=='x') $ Text.pack s
                intDims = map (\s -> read (Text.unpack s) :: Integer) txtDims
            in case intDims of
              (l:w:h:_) -> (l,w,h)
              e -> error ("could not parse dims: " ++ show(e))


