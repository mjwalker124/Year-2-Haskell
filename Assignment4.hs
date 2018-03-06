{-******************************
    assignment3.hs
    Enigma
    Matthew Walker ACA15MJW
********************************-}

module Enigma where
 import Ciphers16
 import EnigmaAndMenu
 import Bombe_Testing
 import Data.Char -- needed for Char ordering etc
 import Data.List -- needed for delete etc

 import Debug.Trace
 
  ------------------------------------------------
 -- Create Types
  
 -------------------------------------------------
 type SteckerPair = (Char, Char)
 
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x
 
 offset_step_back :: Offsets->Offsets
 
 offset_step_back (l,m,0) = offset_step_backlm (l,m,25)
 offset_step_back (l,m,r) = (l,m,r-1)
 
 offset_step_backlm :: Offsets->Offsets
 offset_step_backlm(l,0,25)=offset_step_backl (l,25,25)
 offset_step_backlm (l,m,25)=(l,m-1,25)
 
 offset_step_backl :: Offsets->Offsets
 offset_step_backl (0,25,25)= (25,25,25)
 offset_step_backl (l,25,25)=(l-1,25,25)

 percent :: Int->Int->Int
 percent x y = round (100*(intToFloat x)/(intToFloat y))
 intToFloat :: Int -> Float
 intToFloat n = fromInteger (toInteger n)
 
 breakEnigma :: Crib -> Maybe(Offsets, Stecker)
 breakEnigma c = breakEA c (longestMenu c) [(fst(c !! head (longestMenu c)), ['A'..'Z'] !! mod ((alphaPos (fst(c !! head (longestMenu c))) + 1)) 25)] (0,0,1)

       


 compareString :: String -> String -> Int
 compareString a b = percent (length (filter (\n-> n == True) (map (\(first,seccond) -> first == seccond) $ zip a b))) (length a)
 

 breakEA :: Crib->Menu->Stecker->Offsets->Maybe(Offsets, Stecker)
 breakEA c m s o 
    | (o) == (25,25,25) && stecker == Nothing = Nothing
    | (stecker == Nothing) || testPercentage < 80 = breakEA c m s (offset_step o)
    |otherwise = Just(((offset_step_back o), (resMaybe stecker)))
    where
        stecker = findStecker c m s o
        messages = unzip c
        testedDecrypt = (enigmaEncodeMessage (fst messages) (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (resMaybe stecker)) ((offset_step_back o)))
        testPercentage = compareString testedDecrypt (snd messages)
 
 incrementStecker :: SteckerPair -> SteckerPair
 incrementStecker sp = ((fst sp), ['A'..'Z'] !! mod ((alphaPos (snd sp)) + 1) 25)
 
 showDetails :: (Int, Int, Int) -> String
 showDetails (fst, snd, trd) = "("++ show fst++","++ show snd++","++ show trd++")"
 
 findStecker :: Crib->Menu->Stecker->Offsets-> Maybe Stecker
 findStecker c m s o
     | (followMenu c m s o) /= Nothing =  (followMenu c m s o)
    | snd (head s) == (['A'..'Z'] !! mod (alphaPos ((fst (head s)))      ) 25) = (followMenu c m s o)
    |otherwise = findStecker c m [(incrementStecker (head s))] o
 
 steckerContains :: SteckerPair -> Stecker -> Bool
 steckerContains sp s
    | fst sp == char1 || fst sp == char2 || snd sp == char1 || snd sp == char2 = True
    | length (tail s) == 0 = False
    |otherwise = steckerContains sp (tail s)
    where
        char1 = fst (head s)
        char2 = snd (head s)
 
 steckerAdd :: SteckerPair -> Stecker -> Maybe Stecker
 steckerAdd sp s
    | length s == 0 = Just s
    | (fst (sp)) == (snd (sp)) = Just (s)
    | steckerContains sp s = Nothing
    | otherwise = Just ([sp] ++ s)
    
 followMenu :: Crib->Menu->Stecker->Offsets->Maybe Stecker
 followMenu c m s o
    | steckerAdd (cic, r) s == Nothing = Nothing
    | length (tail m) == 0 = steckerAdd (cic, r) s
    |otherwise = followMenu c (tail m) (resMaybe (steckerAdd (cic, r) s )) o
    where
        i = head m
        p = fst(c !! i)
        q = p
        r = enigmaEncodeA q (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB s) (iterate offset_step o !!(((head m))))
        cic = snd(c !! i)