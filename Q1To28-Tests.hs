import Control.Exception
import Control.Monad
import Q1To28
import Test.HUnit
import Core


q1Test impl = TestCase $ 
	do
		assertException . evaluate $ impl []
		assertEqual "Singleton list" 1 (impl [1])
		assertEqual "General test" 4 (impl [1, 2, 3, 4])

q2Test impl = TestCase $ 
	do
		assertException . evaluate $ impl []
		assertException . evaluate $ impl [1]
		assertEqual "Two item list" 1 (impl [1, 2])
		assertEqual "General" 3 (impl [1, 2, 3, 4])

q3Test impl = TestCase $
	do 
		assertEqual "Singleton list" 1 $ impl 1 [1]
		assertEqual "General" 4 $ impl 3 [1, 2, 4, 8]
		
kthTest = TestCase $ 
	do
		let empty = show EmptyList
		let oor = show IndexOutOfRange
			
		assertErrorCall empty $ evaluate $ kth (5) []			
		assertErrorCall empty $ evaluate $ kth (-5) []
		assertErrorCall empty $ evaluate $ kth (0) []
		assertErrorCall empty $ evaluate $ kth (1) []
		assertErrorCall oor $ evaluate $ kth (-5) [1, 2, 3, 4, 5]
		assertErrorCall oor $ evaluate $ kth (-50) [1, 2, 3, 4, 5]
		
q4Test impl = TestCase $
	do
		assertEqual "Empty" 0 $ impl []
		assertEqual "Singleton" 1 $ impl [1]
		assertEqual "General" 3 $ impl [1, 2, 3]
		
q5Test impl = TestCase $
	do
		assertEqual "Empty" [] $ impl []
		assertEqual "Singleton" [1] $ impl [1]
		assertEqual "General" [3, 2, 1] $ impl [1, 2, 3]
		
main = do 
	let doTests = runTestTT . TestList

	-- There must be a better way of doing this
	doTests $ map q1Test [last', last'', last''', last'''', last''''', last'''''', last''''''']
	doTests $ map q2Test [penultimate, penultimate', penultimate'', penultimate''', penultimate'''', penultimate''''']
	doTests [kthTest]
	-- doTests $ (kthTest:) $ map q3Test [kth', kth'', kth''']	
	-- doTests $ map q4Test [length', length'', length''', length'''']
	-- doTests $ map q5Test [reverse', reverse'']

