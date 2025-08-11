module PPrint where

import Exp

class PPrint x where
    pprint :: x -> String

instance PPrint Lit where
    pprint (Lit d) = show d

instance (PPrint s1, PPrint s2) => PPrint (Plus s1 s2) where
    pprint (Plus s1 s2) = '(' : pprint s1 ++ " + " ++ pprint s2 ++ ")"  