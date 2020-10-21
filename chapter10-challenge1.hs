natTrans :: Maybe a -> [a]
natTrans (Just a) = [a]
natTrans Nothing = []

--Proof
fmap f (natTrans Nothing) =
fmap f [] =
[] =
natTrans Nothing =
natTrans (fmap f Nothing)

fmap f (natTrans (Just a)) =
fmap f [a] =
[f a] =
natTrans (Just (f a)) =
natTrans (fmap f (Just a))
