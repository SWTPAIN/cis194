type Peg  = String
type Move = (Peg, Peg)

hanol :: Integer -> Peg -> Peg -> Peg -> [Move]
hanol 0 _ _ _ = []
hanol n srcPeg tempPeg destPeg = hanol (n - 1) srcPeg destPeg tempPeg
                               ++ [(srcPeg, destPeg)]
                               ++ hanol (n - 1) tempPeg srcPeg destPeg
