# picocaml
small language


# Running 

1. Get ghc
2. load in the file through ghci `ghci main.hs`
3. Use the function `parseAll`

## Example

    parseAll "raise (fun x -> x) 4 + 3;;"
    
which should spit out a (very ugly) syntax tree.

    Right (Anon (BinOpAppExp IntPlusOp (RaiseExp (AppExp (FunExp "x" (VarExp "x")) (ConstExp (IntConst 4)))) (ConstExp (IntConst 3))))
    
    
