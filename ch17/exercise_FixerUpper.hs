j = const <$> Just "Hello" <*> pure "World"

k = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
