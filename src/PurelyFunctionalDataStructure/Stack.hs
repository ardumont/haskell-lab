-- | A module to reference work on Stack

module PurelyFunctionalDataStructure.Stack where

-- Exercise 2.1 Write a function suffixes of type a list -» a list list that takes a
-- list xs and returns a list of all the suffixes of xs in decreasing order of length.
--
-- For example:
-- suffixes [1,2,3,4] = [[1,2,3,4], [2,3,4], [3,4], [4], [ ] ]
-- Show that the resulting list of suffixes can be generated in O(n) time and represented in O(n) space.
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(_:xs) = l:suffixes xs

-- λ> suffixes [1,2,3,4]
-- [[1,2,3,4],[2,3,4],[3,4],[4],[]]

-- time:  We walk over the complete sequence of the list of size n so O(n) time.
-- space: We keep the reference of the previous values.
-- The first element of the result is the list itself, no duplication.
-- After that, the second element is the tail of the list, this is shared with the initial list too.
-- And so on and so forth.
-- So O(n) space.
