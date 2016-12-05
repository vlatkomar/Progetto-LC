{
module Lexer where
}

%wrapper "basic"

-- macro definition

$digit = 0-9			-- digits

-- scanner definition

tokens :-

    " "				{ \s -> Space }
    "("				{ \s -> Open }
    ")"				{ \s -> Close }
    $digit+			{ \s -> Int (read s) }
    $digit+"."+$digit+		{ \s -> Float (read s) }

{
-- Each token has type :: String -> Token

-- The token type:
data Token =
    Space |
    Open |
    Close |
    Int Int |
    Float Float
    deriving (Eq,Show)
}
