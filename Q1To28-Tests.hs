import Control.Exception
import Control.Monad
import Q1To28
import Test.HUnit


assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: "-- ++ show ex
	where isWanted = guard . (== ex)


q1Test impl = TestCase $ 
	do
--		assertException PatternMatchFail (evaluate $ impl [])
		assertEqual "Singleton list" 1 (impl [1])
		assertEqual "General test" 4 (impl [1, 2, 3, 4])

q2Test impl = TestCase $ 
	do
--		assertException PatternMatchFail (evaluate $ impl [])
		--assertEqual "Singleton list" 1 (impl [1])
		assertEqual "Two item list" 1 (impl [1, 2])
		assertEqual "General test" 3 (impl [1, 2, 3, 4])

		
		
-- Must be a better way of doing this
q1Tests = map q1Test [last', last'', last''', last'''', last''''', last'''''', last''''''']
q2Tests = map q2Test [penultimate, penultimate', penultimate'', penultimate''', penultimate'''', penultimate''''']

main = runTestTT $ TestList $ concat [q1Tests, q2Tests]


