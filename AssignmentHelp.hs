module AssignmentHelp where

 -- useful functions & data for COM2001
 import Data.Char -- needed for Char ordering etc
 import Data.List -- needed for delete etc

 ----------------------------------------------------------- 
 -- alphabetic posiiton for an uppercase letter
 -- starting @ 0 for 'A'
 -- ord in Data.Char gives ordering for Chars - 'A' is 65
 
 alphaPos :: Char-> Int
 alphaPos c = (ord c) -65
 
 -- upper case letter to lower case
 
 u2Lo :: Char->Char
 
 u2Lo c = chr ((ord c)+32) 
 
 ------------------------------------------------
 
 -- percent x of y, both Ints, rounded to an Int
 
 percent :: Int->Int->Int
 
 percent x y = round (100*(intToFloat x)/(intToFloat y))
 
 intToFloat :: Int -> Float
 intToFloat n = fromInteger (toInteger n)
 
 ------------------------------------------------
 -- Letter Frequencies for English
 
 engFreq :: [(Char,Float)]

 engFreq = [('a',0.08167),('n',0.06749),
            ('b',0.01492),('o',0.07507),
            ('c',0.02782),('p',0.01929),
            ('d',0.04253),('q',0.00095),
            ('e',0.12702),('r',0.05987),
            ('f',0.02228),('s',0.06327),
            ('g',0.02015),('t',0.09056),
            ('h',0.06094),('u',0.02758),
            ('i',0.06966),('v',0.00978),
            ('j',0.00153),('w',0.02360),
            ('k',0.00772),('x',0.00150),
            ('l',0.04025),('y',0.01974),
            ('m',0.02406),('z',0.00074)]
 ---------------------------------------------------------------------------------
 -- the message to be decoded in assignment 1
 
 mystery = "QJAWXARJFBEWXZXADBAJQJDJQFYLQVCWEVEFKQHWHRFDCXKWXNFYMWYFDMCPWAAXMWAJFVNWJAPXZWJCQAFYWXNQJJNWBQJNFYMWEAJFVFZQJACFDNHBWJCWEQMCJAFEJFTAQUWYFSAJFVPXRBWYFJNWJAQYLEWXAWJCWPWAAXMWNWYMJCXBQJPFEWAJFV"
 
 ------------------------------------------------
 
 -- Maybe helpers

 
 nothingP :: Maybe a -> Bool
 nothingP Nothing = True
 nothingP (Just _) = False
 
 fromMaybe :: Maybe a -> a
 fromMaybe (Just x)=x
 ----------------------------------------------------------------------------------
 
 -- substitution cyphers for the Enigma rotors
 

 {- the standard Enigma reflector (ReflectorB)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
 -}
 
 
 ----------------------------------------------
 
 

 ------------------------------------------------
 -- Merge and MergeSort
 
 mergesort :: (a->a -> Bool)->[a] -> [a]
 
 mergesort compfn [] = [] --check this once only
 mergesort compfn dlis =
   head (mergesortA compfn (map (\ e -> [e]) dlis))

 mergesortA :: (a->a -> Bool)->[[a]] -> [[a]]
 mergesortA compfn mlis
  |(length mlis == 1) = mlis --stopping condition
  |otherwise = mergesortA compfn(mergesortpass compfn mlis)

 mergesortpass :: (a->a -> Bool)->[[a]] -> [[a]]
 mergesortpass _ [] = []
 mergesortpass _ mlis@[_] = mlis
 mergesortpass compfn (ml1:ml2:mlt) = (my_merge compfn ml1 ml2):
                                            (mergesortpass compfn mlt)
                                            
 my_merge :: (a->a -> Bool)->[a]->[a] -> [a]
 my_merge compfn [] lis2 = lis2
 my_merge compfn lis1 [] = lis1

 my_merge compfn l1@(h1:t1) l2@(h2:t2)
  | compfn h1 h2 = (h1:my_merge compfn t1 l2)
  | otherwise =    (h2:my_merge compfn l1 t2)
 
 ------------------------------------------------
 