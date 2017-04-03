datatype reply a = Success of a * (list char)
                 | Failure

datatype consumed a = Consumed of reply a
                    | Empty of reply a

datatype parser a = Parser of ((list char) -> consumed a)

fun unParser [a] (Parser p) : (list char) -> consumed a = p

fun stringToList (s : string) : list char =
    let
	fun upTo acc i =
	    if eq i 0 then 0 :: acc
	    else upTo (i :: acc) (i - 1)
    in
	List.mp (fn i => strsub s i) (upTo [] (strlen s))
    end


fun runParser [a] (Parser p) (s : string) : option a = let
    fun reply [a] (r : reply a) : option a = case r of
          Success (a, _) => Some a
        | Failure => None
    in
    case p (stringToList s) of
	  Consumed r => reply r
        | Empty r => reply r
    end

val monad = mkMonad {
    Return = let
            fun return [a] (x : a) : parser a = Parser (fn input => Empty (Success (x, input)))
        in
            @@return
        end,
    Bind = let
            fun bind [a] [b] (p : parser a) (f : a -> parser b) : parser b =
                Parser (fn input => case (unParser p) input of
                      Empty (Success (x, xs))    => unParser (f x) xs
                    | Empty Failure              => Empty Failure
                    | Consumed (Success (x, xs)) => unParser (f x) xs
                    | Consumed Failure           => Consumed Failure)
        in
            @@bind
        end
}

fun choice [a] (p : parser a) (q : parser a) : parser a =
    Parser (fn input => case (unParser p) input of
          Empty Failure => (unParser q) input
        | Empty success => (case (unParser q) input of
              Empty _  => Empty success
            | consumed => consumed)
        | consumed => consumed)

fun try [a] (p : parser a) : parser a = Parser (fn input =>
    case (unParser p) input of
          Consumed Failure => Empty Failure
        | r                => r)

(* combinators *)

val succeed : parser unit = return ()
val fail [a] : parser a = Parser (fn input => Empty Failure)

fun exactly [a] (n : int) (p : parser a) : parser (list a) =
    let
        fun p' n a = if n = 0 then return a else
            r <- p;
            p' (n - 1) (r :: a)
    in p' n []
    end

fun any [a] (p : parser a) : parser (list a) =
    let
        fun p' a = choice (
            r <- p;
            p' (r :: a))
            (return a)
    in
        p' []
    end

fun many [a] (n : int) (p : parser a) : parser (list a) =
    r <- exactly n p;
    r' <- any p;
    return (List.append r r')

fun satisfy (test : char -> bool) : parser char = 
 Parser (fn input => case input of
			 [] => Empty Failure
		       | x :: xs => if test x then
					Consumed (Success (x, xs))
				    else Empty Failure)

fun char (c : char) : parser char = satisfy (fn k => eq k c)

