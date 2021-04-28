data RegEx = Epsilon                  -- matches the empty word, which is also often written as ε.
           | Dot                      -- matches any character in Σ.
           | MatchCharacter Char      -- Each character c ∈ Σ matches itself.
           | QuestionRegex RegEx      -- regex? matches the empty word or whatever is matched by regex.
           | StarRegex RegEx          -- regex* matches zero or more occurrences of the words matched by regex.
           | PlusRegex RegEx          -- regex+ matches one or more occurrences of the words matched by regex.
           | MatchRegex RegEx RegEx   -- regex regex′ matches any word ww′ when regex matches w and regex′ matches w′.
           | OrRegex RegEx RegEx      -- regex | regex′ matches any word that is matched by either regex or regex′.
           | ListReg [RegEx]          -- Hold a list of Regex's
           
-------------------------------------------------------------- Part b
-- define a function thats takes a regular expression and string, returns T if they match
accept :: RegEx -> String -> Bool
--regular expression == regex, string == w

-- Regular expressions from data type:

accept Epsilon str  | str == "" = True
                    | otherwise = False

accept Dot str | (length str) /= 0 = True
               | otherwise = False

--accept (MatchCharacter Char) Char | 
--                                  |

accept (OrRegex x y) str 
    | accept x str || accept y str = True
    | otherwise = False