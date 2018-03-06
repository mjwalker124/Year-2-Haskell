{-******************************
    assignment3.hs
    Enigma
    Matthew Walker ACA15MJW
********************************-}

module Bombe where
 import EnigmaAndMenu
 import Data.Char -- needed for Char ordering etc
 import Data.List -- needed for delete etc
 import GHC.Exts
 import Debug.Trace
 
  ------------------------------------------------
 -- Create Types
  
 -------------------------------------------------
 type SteckerPair = (Char, Char)
 
 --Use of Phil's code to get extract a value from a maybe
 resMaybe :: (Maybe a) -> a
 resMaybe (Just x) = x
 
 --A backwards offset step using pattern matching
 offset_step_back :: Offsets->Offsets
 
 offset_step_back (l,m,0) = offset_step_backlm (l,m,25)
 offset_step_back (l,m,r) = (l,m,r-1)
 
 offset_step_backlm :: Offsets->Offsets
 offset_step_backlm(l,0,25)=offset_step_backl (l,25,25)
 offset_step_backlm (l,m,25)=(l,m-1,25)
 
 offset_step_backl :: Offsets->Offsets
 offset_step_backl (0,25,25)= (25,25,25)
 offset_step_backl (l,25,25)=(l-1,25,25)

 --This is from the assignment help document
 percent :: Int->Int->Int
 percent x y = round (100*(intToFloat x)/(intToFloat y))
 intToFloat :: Int -> Float
 intToFloat n = fromInteger (toInteger n)
 
 --This takes in a 3 part tuple and returns the 2 part tuple having removed percentage 
 clean :: (a,Stecker,c) -> (a,Stecker)
 clean (first,seccond, _) = (first,(filter (\(st) -> ((fst st) /= (snd st))) seccond))

 --This takes in a crib, values for the enigma settings, the number of menus to use and the minimum length of the menu to use
 --This then gets the longest menus it can and runs breakEA over all of them.
 --It then gets the one with the best correctness percentage and returns that.
 breakEnigmaManyMenus :: Crib -> (Rotor, Rotor, Rotor, Reflector) -> Int -> Int -> Maybe(Offsets, Stecker)
 breakEnigmaManyMenus c ei i minimumLength
    | c == [] = Nothing
    |otherwise = Just (clean (result))
    where 
        menus = filter (\menu -> (length menu) >= minimumLength) (findMenus c)
        menusLimited = take i (reverse (sortWith (abs. length) menus))
        results = map (\menu -> resMaybe menu) (filter (\r -> r /= Nothing) (map (\menu -> (breakEA c (menu) [(fst(c !! head (menu)), ['A'..'Z'] !! mod ((alphaPos (fst(c !! head (menu))) + 1)) 25)] (0,0,1) (ei)) ) menusLimited))
        result = head (reverse (sortWith (abs . tT3) results))

 --This is the basic break enigma, it takes in a crib and gets the longest menu, it then runs breakEA on those
 breakEnigma :: Crib -> Maybe (Offsets, Stecker)
 breakEnigma c = Just (clean (resMaybe (breakEA c (longestMenu c) [(fst(c !! head (longestMenu c)), ['A'..'Z'] !! mod ((alphaPos (fst(c !! head (longestMenu c))) + 1)) 25)] (0,0,1) (rotor1, rotor2, rotor3, reflectorB) )))

 --This takes in a crib, and settings for an enigma, so that it is possible to change the rotors and the reflector. 
 breakEnigmaCustom :: Crib -> (Rotor, Rotor, Rotor, Reflector) -> Maybe (Offsets, Stecker)
 breakEnigmaCustom c ei 
    |ea /= Nothing = Just (clean (resMaybe ea))
    |otherwise = Nothing
    where
       ea = ((breakEA c (longestMenu c) [(fst(c !! head (longestMenu c)), ['A'..'Z'] !! mod ((alphaPos (fst(c !! head (longestMenu c))) + 1)) 25)] (0,0,1) (ei) ))

 

 --These are so that it is possible to get the data out of a 3 part tuple.
 fT3 :: (a,b,c) -> a
 fT3 (first,_,_) = first

 sT3 :: (a,b,c) -> b
 sT3 (_,seccond,_) = seccond

 tT3 :: (a,b,c) -> c
 tT3 (_,_,third) = third   

 --This is used so I can check how well the breakEnigma has worked.  It works by adding up all of the matching chars
 compareString :: String -> String -> Int
 compareString a b = percent (length (filter (\n-> n == True) (map (\(first,seccond) -> first == seccond) $ zip a b))) (length a)
 
 --This returns Nothing if the findStecker has found nothing and it has reached the end of the rotors
 --It then progresses the rotor offsets if findStecker is nothing or if the testPercentage is less than 80
 --If it reaches the otherwise then a decent stecker has been found
 breakEA :: Crib->Menu->Stecker->Offsets-> (Rotor, Rotor, Rotor, Reflector) -> Maybe(Offsets, Stecker, Int)
 breakEA c m s o (r1, r2, r3, ref)
    | (o) == (25,25,25) && stecker == Nothing = Nothing
    | (stecker == Nothing) || (length stecker > 11) || testPercentage < 80 = breakEA c m s (offset_step o) (r1, r2, r3, ref)
    |otherwise = Just(((offset_step_back o), (resMaybe stecker), testPercentage))
    where
        stecker = findStecker c m s o
        messages = unzip c
        testedDecrypt = (enigmaEncodeMessage (fst messages) (SteckeredEnigma r1 r2 r3 ref (resMaybe stecker)) ((offset_step_back o)))
        testPercentage = compareString testedDecrypt (snd messages)
 
 --This progresses the second part stecker pair
 incrementStecker :: SteckerPair -> SteckerPair
 incrementStecker sp = ((fst sp), ['A'..'Z'] !! mod ((alphaPos (snd sp)) + 1) 25)

 --This tries to use followMenu, if the first and second part of the first stecker are equal then follow the menu
 --Otherwise it will call findStecker with an incremented stecker head. 
 findStecker :: Crib->Menu->Stecker->Offsets-> Maybe Stecker
 findStecker c m s o
    | (followMenu c m s o) /= Nothing =  (followMenu c m s o)
    | snd (head s) == (['A'..'Z'] !! mod (alphaPos ((fst (head s)))) 25) = (followMenu c m s o)
    |otherwise = findStecker c m [(incrementStecker (head s))] o
 
 --This is passed a SteckerPair and a stecker and checks to see if a the steckerpair is inside the stecker
 steckerContains :: SteckerPair -> Stecker -> Bool
 steckerContains sp s
    | fst sp == char1 || fst sp == char2 || snd sp == char1 || snd sp == char2 = True
    | length (tail s) == 0 = False
    |otherwise = steckerContains sp (tail s)
    where
        char1 = fst (head s)
        char2 = snd (head s)
 
 --This tries to add a stecker pair to a stecker.
 steckerAdd :: SteckerPair -> Stecker -> Maybe Stecker
 steckerAdd sp s
    | length s == 0 = Just s
    | (fst (sp)) == (snd (sp)) = Just (s)
    | steckerContains sp s = Nothing
    | otherwise = Just ([sp] ++ s)

 --This tries to follow the menu to add to the stecker.
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