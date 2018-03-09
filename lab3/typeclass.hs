data MyList a = EmptyList | MyHead a (MyList a)

instance (Show a) => Show (MyList a) where
  show EmptyList = ""
  show (MyHead a tail) = show a ++ ", " ++ show tail