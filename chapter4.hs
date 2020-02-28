module Main where

(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
(>=>) amb bmc = \a ->
    case amb a of
        Just b ->
            bmc b
        Nothing -> Nothing

return' :: a -> Maybe a
return' = Just

safe_reciprocal x = if x == 0 then Nothing else Just (1/x)

safe_root x = if x >= 0 then Just (sqrt x) else Nothing

safe_root_reciprocal = safe_root >=> safe_reciprocal

main :: IO ()
main = do
    putStrLn $ show $ safe_root_reciprocal 0.25
