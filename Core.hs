module Core where

import Control.Exception
import Control.Monad
import Test.HUnit


data StandardError = EmptyList | IndexOutOfRange
instance Show StandardError where
	show EmptyList 			= "Empty list"
	show IndexOutOfRange 	= "Index out of range"
		

assertException :: IO a -> IO ()
assertException action =
    handle handler $ do
		action
		assertFailure "Expected exception."		
	where handler = (const $ return ()) :: SomeException -> IO ()
		
assertExceptionOfType :: (Exception e, Eq e) => e -> IO a -> IO ()
assertExceptionOfType ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
	where isWanted = guard . (== ex)

	
instance Eq ErrorCall where
    x == y = (show x) == (show y)
	
assertError ex f = assertExceptionOfType (ErrorCall ex) $ evaluate f
