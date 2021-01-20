Definition:

fmap f g = f . g

Preserves identity (fmap id g = id):

fmap id g
= id . g
= g
= id g

Preserves composition (fmap (f . g) h = (fmap f) . (fmap g) $ h):

fmap (f . g) h
= (f . g) . h
= f . (g . h)
= f . (fmap g h)
= fmap f (fmap g h)
= (fmap f) . (fmap g) $ h
