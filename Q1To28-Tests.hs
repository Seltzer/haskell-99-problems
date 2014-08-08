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
		let oor = show IndexOutOfRange
			
		assertErrorCall oor $ evaluate $ kth (5) []			
		assertErrorCall oor $ evaluate $ kth (-5) []
		assertErrorCall oor $ evaluate $ kth (0) []
		assertErrorCall oor $ evaluate $ kth (1) []
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

q6Test impl = TestCase $
	do
		assertEqual "Empty" True $ impl []
		assertEqual "Singleton" True $ impl [1]
		assertEqual "Not palindrome" False $ impl [1, 2, 3]
		assertEqual "Palindrome" True $ impl [1, 2, 1]
		assertEqual "Palindrome" True $ impl [1, 2, 3, 2, 1]

q7Test impl = TestCase $
	do
		assertEqual "Single empty" [] $ impl [[]]
		assertEqual "Multiple empty" [] $ impl [[], [], []]
		assertEqual "Single singleton" [1] $ impl [[1]]
		assertEqual "Multiple singleton" [1, 3, 5] $ impl [[1], [3], [5]]
		assertEqual "Single non-singleton" [1, 7] $ impl [[1, 7]]
		assertEqual "Multiple non-singleton" [1, 7, 2, 2, 6, 3] $ impl [[1, 7, 2], [2, 6, 3]]
		assertEqual "All together now" [1, 7, 2, 18, 2, 6, 3] $ impl [[1, 7, 2], [18], [], [2, 6, 3], []]

-- FIXME: Need better way of doing this... impl pattern above doesn't work		
q7Test2 = TestCase $ assertEqual "For science" "asdfqwertyuiop" ((flatten' . flatten') $ [["as", "df" ], ["qw"], ["er"], ["ty", "ui", "op"], []])
q7Test3 = TestCase $ assertEqual "For science" "asdfqwertyuiop" ((flatten'' . flatten'') $ [["as", "df" ], ["qw"], ["er"], ["ty", "ui", "op"], []])


q8Test impl = TestCase $
	do
		assertEqual "Empty" "" $ impl ""
		assertEqual "Singleton" "a" $ impl "a"
		assertEqual "General" "abcade" $ impl "aaaabccaadeeee"
	
q9Test impl = TestCase $
	do
		assertEqual "Empty"	[] $ impl []
		assertEqual "Singleton"	["a"] $ impl ['a']
		assertEqual "General 1" ["a","b","c","a","d","e"] $ impl ['a', 'b', 'c', 'a', 'd', 'e']
		assertEqual "General 2" ["aaaa","b","cc","aa","d","eeee"] $ impl ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

q10Test impl = TestCase $
	do
		assertEqual "Empty"	[] $ impl []
		assertEqual "Singleton"	[(1, 'a')] $ impl ['a']
		assertEqual "General 1" [(1, 'a'), (1, 'b'), (1, 'c'), (1, 'a'), (1, 'd'), (1, 'e')] $ impl ['a', 'b', 'c', 'a', 'd', 'e']
		assertEqual "General 2" [(4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e')] $ impl ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
		
q11Test impl = TestCase $
	do
		assertEqual "Empty"	[] $ impl []
		assertEqual "Singleton"	[Single 'a'] $ impl ['a']
		assertEqual "General 1" [Single 'a', Single 'b', Single 'c', Single 'a', Single 'd', Single 'e'] $ impl ['a', 'b', 'c', 'a', 'd', 'e']
		assertEqual "General 2" [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] 
			$ impl ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

-- TODO: Could instead inject flipped assertEqual into q11Test
q12Test impl = TestCase $
	do
		assertEqual "Empty"	[] $ impl []
		assertEqual "Singleton"	['a'] $ impl [Single 'a']
		assertEqual "General 1" ['a', 'b', 'c', 'a', 'd', 'e'] $ impl [Single 'a', Single 'b', Single 'c', Single 'a', Single 'd', Single 'e'] 
		assertEqual "General 2" ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] $ impl [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] 

			
main = do 
	let doTests = runTestTT . TestList

	-- There must be a better way of doing this
	putStrLn "Q1 Tests - last"
	doTests $ map q1Test [last', last'', last''', last'''', last''''', last'''''', last''''''']
	
	putStrLn "Q2 Tests - penultimate"
	doTests $ map q2Test [penultimate, penultimate', penultimate'', penultimate''', penultimate'''', penultimate''''']
	
	putStrLn "Q3 Tests - kth"
	doTests $ (kthTest:) $ map q3Test [kth', kth'', kth''']
	
	putStrLn "Q4 Tests - length"
	doTests $ map q4Test [length', length'', length''', length'''']
	
	putStrLn "Q5 Tests - reverse"
	doTests $ map q5Test [reverse', reverse'']
	
	putStrLn "Q6 Tests - isPalindrome"
	doTests $ map q6Test [isPalindrome, isPalindrome', isPalindrome'', isPalindrome''', isPalindrome''', isPalindrome'''', isPalindrome''''']
	
	putStrLn "Q7 Tests - flatten"
	doTests $ (++ [q7Test2, q7Test3]) $ map q7Test [flatten', flatten'']
	
	putStrLn "Q8 Tests - compress"
	doTests $ map q8Test [compress', compress'', compress''', compress'''', compress''''', compress'''''']
	
	putStrLn "Q9 Tests - pack"
	doTests $ map q9Test [pack', pack'']

	putStrLn "Q10 Tests - encode"
	doTests $ map q10Test [encode']
	
	putStrLn "Q11 Tests - encodeModified"
	doTests $ map q11Test [encodeModified]
	
	putStrLn "Q12 Tests - decode'"
	doTests $ map q12Test [decode', decode'']

