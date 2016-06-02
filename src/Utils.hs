module Utils where

index :: [a] -> Int -> Maybe a
index l i = case l of
  [] -> Nothing
  x : xs -> if i == 0 then Just x else index xs (i - 1)

duplicated :: Eq a => [a] -> Maybe a
duplicated l = let helper l r = case r of
                     [] -> Nothing
                     x : xs -> if elem x l then Just x
                               else helper (x : l) xs
               in helper [] l
