import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe

-- 1.) Modified run-length encoding
-- (encode-modified '(a a a a b c c a a d e e e e))
-- 		((4 A) B (2 C) (2 A) D (4 E))
