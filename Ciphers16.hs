module Ciphers16 where
 {- Assignment 1 of 3
    Substitution cyphers and how to break them
    Versions given for Explicit Recursion & using Mapping Fns
    2016 version
 -}

 import AssignmentHelp
 import Data.List -- needed for delete    

 
 
 -- datatypes
 
 type Cipher = String -- a substitution cypher for A,B..Z
 
 -- some examples
 r1 :: Cipher
 r1 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"  -- Enigma rotor 1
 r2 = "AJDKSIRUXBLHWTMCQGZNPYFVOE"  -- Enigma rotor 2
 
 alphabet :: Cipher
 alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 ----------------------------------------------------------------------
 -- is a Substitution code valid?
 -- i.e. all 26 letters appear once
 {- variables
    s String to validate
    c String to validate
    ha,ra first & rest of alphabet
 -}
    
    
 validateCipher :: Cipher->Bool
 
 -- EXPLICIT RECURSION
 
 validateCipher s = ((length s) == 26)&& (checkChars s alphabet)
 -- do length check only once
 
 -- check for all upper case letters
 checkChars :: String->String->Bool
 
 checkChars _ [] = True --checked all alphabet
 
 checkChars c (ha:ra) --  c is the Cipher, remaining alphabet is ha:ra
  |elem ha c = checkChars c ra -- got this one, test remainder
  |otherwise = False
 
 {- MAPPING .. i.e. sort, then will be identical to alphabet
 
 validateCipher s = (mergesort (<) s)== alphabet
 
 -}
 
 

---------------------------------------------------------- 
 -- ENCODING CHARS
 -- forward encoding
 -- with an offset
 
 -- encode individual char
 
 {- variables
    r the cipher
    n the offset
    x the char to be encoded
    q index into alphabet
 -}
  
 encode :: Cipher->Int->Char->Char -- put Char to be encoded as last arg for partial fn (later!)
 
 encode  r n x =  
  let
   p=mod ((alphaPos x) - n)  26  -- position of the given ch x after rotation
  in r!!p
 -----------------------------------------------------------------
 -- reverse encoding
 
 reverseEncode :: Cipher->Int->Char->Char
 
 reverseEncode  r n x =
  let
   p = findPos x r -- position of x in cipher
   q = mod (p+n) 26 -- position after offset
  in
   alphabet!!q -- index into alphabet

-----------------------------------------------------------------
 -- position of a given character in a rotor
 {- variables
    ch character
    (h:t) cipher
 -}
  
 findPos :: Char ->Cipher->Int
 {-
 -- version with explicit recursion
 findPos ch (h:t)
   |(ch==h) = 0
   |otherwise = 1+ (findPos ch t)

 

 -- with a comprehension
 findPos ch r = 
    head [p |(c,p) <- (zip r [0..25]), c==ch]
 
 -}   
 -- with a filter
 
 findPos ch r =
  let
   [(_,n)]=filter (\(c,p)->c==ch) (zip r [0..25])
  in
   n
   

 --------------------------------------------------
 -- ENCODE A MESSAGE

 -- with explicit recursion
 
 {- variables
    (mh:mt) the message
    s cipher
    n offset
 -}
    
 encodeMessage :: String->Cipher->Int->String
 encodeMessage [] _ _ = []
 
 encodeMessage (mh:mt) s n = (encode s n mh): (encodeMessage mt s n)
 

 
 {- 
  -- as a map, with partial fn
  
 encodeMessage m s n = map (encode s n) m
 
 -- with a lambda
 encodeMessage m s n = map (\c-> (encode s n c)) m
 -}
 
 
 -- reverseEncodeMessage
 -- same code but reverseEncode rather than encode
 
 reverseEncodeMessage :: String->Cipher->Int->String
 reverseEncodeMessage [] _ _ = []
 
 reverseEncodeMessage (mh:mt) s n = (reverseEncode s n mh): (reverseEncodeMessage mt s n)
 
 
 --------------------------------------------------
 -- BREAKING CODES
 
 -- count the letters, express as %s
 -- outputs [('A',%A),('B',%B)..]
 
 {- variables
    s message String
    (h:t) head, tail of s
    clis list of pairs - char, count
    (c,n) a pair from this
    p alphabetic posiiton of a char
 -}
 

 letterStats :: String -> [(Char,Int)]
 
 -- with explicit recursions
 -- count of chars, e.g. [('A',3), ('C',2) ..]
 -- counts of zero removed
 
 zlis :: [Int] -- set up initial counts
 zlis = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
 
 charCounts :: String->[(Char,Int)]
 
 charCounts s  = charCountsNZ (charCountsA s (zip alphabet zlis)) 
 
 -- recursive fn to increment counts

 charCountsA :: String->[(Char,Int)]-> [(Char,Int)]
 charCountsA [] clis = clis
 charCountsA (h:t) clis = 
  let 
      p = alphaPos h
      (c,n) = clis!!p
      ans = (take p clis)++((c,n+1):(drop (p+1) clis)) -- form up new clis
  in 
     (charCountsA t ans)
 
  -- remove the zero counts
  
 charCountsNZ :: [(Char,Int)]-> [(Char,Int)]
 charCountsNZ [] = []
 charCountsNZ ((c,n):t) 
  |n==0 = charCountsNZ t
  |otherwise = (c,n):(charCountsNZ t)
 
 -- with mapping fns
 {-
 letterStats m = 
  let
   counts= map (charCount m) alphabet 
   fcounts = filter (\(_,n)->n>0) counts
   total = foldr (+) 0 (map snd fcounts)
   res = map (\(c,n)-> (c, percent n total)) fcounts
  in
   mergesort (\x y ->(snd x)>(snd y)) res
 
 
 charCount :: String->Char->(Char,Int)
 
 charCount m c = (c,length (filter (== c) m))
 -}
 
 
  
  
  
 -- letterStats m = mergesort (\x y->((snd x)>(snd y))) (charCounts m)
 
 -- using group
 
 letterStats m = map (\cg@(h:_)-> (h,(percent (length cg)(length m)))) 
                     (mergesort (\g1 g2-> length g1>length g2) (group (mergesort (<) m)))
 
 --------------------------------------------------------------
 
 -- Substitute letter guesses
 -- replacing upper with lower case
 
 type Guesses = [(Char,Char)] -- guessing that first Char is coded to second
 
 {- variables
    gl a guess list
    (h:t) the encript
 -}
 
  -- explicit recursion
 partialDecode :: Guesses->String->String
 
 partialDecode _[] = []
 
 partialDecode gl (h:t) = (lCipher gl h): (partialDecode gl t)
  
 -- Substitute letter - in lower case - if it's in Guesses
 {- variables
    (x,y) pair from guess list
    t rest of guess list
 -}
 
 lCipher :: Guesses->Char->Char
 
 lCipher [] c = c
 
 lCipher ((x,y):t) c 
  |x==c = (u2Lo y)
  |otherwise = lCipher t c
 
 {-
 -- with Mapping functions
 partialDecode slis m = map (lCipher slis) m

 -- lCipher with filter
 lCipher slis c =
  let 
   res = filter (\(x,_)-> (x==c)) slis
  in 
   if (null res) then c else u2Lo (snd (head res))
 -}
 
 
  
 
 -- sample message for breaking
 plain :: String
 plain = "ITSEASYTOBREAKASUBSTITUTIONCIPHERPROVIDEDYOUHAVEALONGENOUGHMESSAGESTOPLETSMAKETHISONEALITTLEBITLONGERSTOPOKITSHOULDBETHERIGHTSORTOFSIZENOWSTOPMAYBENOTLETSINCREASETHEMESSAGELENGTHABITMORESTOP"
  
 mess :: String
 mess = encodeMessage plain r2 18
 
  
 compareSnd :: (Char,Int)->(Char,Int)->Ordering
 compareSnd (_,n1) (_,n2)
  |n1 < n2 = LT
  |n1==n2 = EQ 
  |otherwise = GT
  
  