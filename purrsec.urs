con parser :: Type -> Type

val runParser : a ::: Type -> parser a -> string -> option a

val monad : monad parser
val choice : a ::: Type -> parser a -> parser a -> parser a
val try : a ::: Type -> parser a -> parser a

val satisfy : (char -> bool) -> parser char
val char : char -> parser char
