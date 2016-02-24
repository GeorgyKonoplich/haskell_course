module DiagnosticFun where

type Range = (Int, Int)

data Diagnostic t = D Range String

instance Show (Diagnostic t) where
  show (D r s) = "Range: " ++ show r ++ ", Message: " ++ s

data Warning
data Error

warningT = undefined :: Warning
errorT   = undefined :: Error

createDiagnostic :: t -> Range -> String -> Diagnostic t
createDiagnostic _ r s = D r s

d1 = createDiagnostic warningT (1, 2) "This is warning"
d2 = D (1, 4) "This is error" :: Diagnostic Error
d3 = D (2, 40) " , Vasya!!!" :: Diagnostic Error

intersect :: Range -> Range -> Range
intersect (a, b) (c, d) = (max a c, min b d)

instance Monoid (Diagnostic t) where
    mempty = D (minBound, maxBound) ""
    mappend (D a b) (D c d) = D (intersect a c) (b ++ d)
