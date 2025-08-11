module TimesPPrint where

import Times
import PPrint

instance (PPrint s1, PPrint s2) => PPrint (Times s1 s2) where
    pprint (Times s1 s2) = '(' : pprint s1 ++ " * " ++ pprint s2 ++ ")"  