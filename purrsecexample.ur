structure Parse : sig
    con parser :: Type -> Type
    val runParser : a ::: Type -> parser a -> string -> option a
    val monad_parser : monad parser
    val satisfy : (char -> bool) -> parser char
    val char : char -> parser char
		       
    val choice : a ::: Type -> parser a -> parser a -> parser a
end = struct
    datatype reply a = Success of a * list char
                     | Failure

    datatype consumed a = Consumed of reply a
                        | Empty of reply a

    type parser a = list char -> consumed a

    fun stringToList (s : string) : list char =
        let
	    fun upTo acc i =
                if i < 0 then acc
	        else if i = 0 then 0 :: acc
	        else upTo (i :: acc) (i - 1)
        in
	    List.mp (String.sub s) (upTo [] (String.length s - 1))
        end

    fun runParser [a] (p : parser a) (s : string) : option a =
        let
            fun reply [a] (r : reply a) : option a =
                case r of
                    Success (a, _) => Some a
                  | Failure => None
        in
            case p (stringToList s) of
	        Consumed r => reply r
              | Empty r => reply r
        end

    fun return' [a] (x : a) : parser a = fn input => Empty (Success (x, input))

    fun bind' [a] [b] (p : parser a) (f : a -> parser b) : parser b =
     fn input => case p input of
		     Empty (Success (x, xs))    => f x xs
		   | Empty Failure              => Empty Failure
		   | Consumed rep =>
		     Consumed (case rep of
				   Failure => Failure
				 | Success (x, xs) =>
				   (case f x xs of
					Consumed rep' => rep'
				      | Empty rep' => rep'))

    val monad_parser = mkMonad {Bind = @@bind', Return = @@return'}


    fun satisfy (test : char -> bool) : parser char =
     fn input => case input of
		     [] => Empty Failure
	           | x :: xs => if test x then
				    Consumed (Success (x, xs))
			        else
                                    Empty Failure

    fun char (c : char) : parser char = satisfy (eq c)
					
    fun choice [a] (p : parser a) (q : parser a) : parser a =
	fn input => case p input of
			Empty Failure => q input
		      | Empty ok => (case q input of
					 Empty _ => Empty ok
				       | consumed => consumed)
		      | consumed => consumed

    fun try [a] (p : parser a) : parser a =
     fn input =>
	case p input of
	    Consumed Failure => Empty Failure
	  | r                => r


end

open Parse

datatype bbtag = B | U
datatype bbtagged = BBStr of list char | BBTag of bbtag * list bbtagged
con bbcode = list bbtagged

val bbid : parser bbtag =
    choice
	(_ <- char #"b" ; return B)
	(_ <- char #"u" ; return U)

(*

data BBTag = B | U deriving Show
newtype BBCode = BBCode { unBB :: [BBTagged] }
data BBTagged = BBStr String | BBTag BBTag BBCode


instance Show BBCode where
 show b = concat (map show (unBB b))
instance Show BBTagged where
 show (BBStr s) = s
 show (BBTag t b) = "[" ++ show t ++ "]" ++ show b ++ "[/" ++ show t ++ "]"


-- fghjkjhbgfvdghjhgfghj[b]asdfhgdsdfhm[/b]sfdghgfdg

test = BBCode [ BBStr "fghjkjhbgfvdghjhgfghj"
              , BBTag B (BBCode [BBStr "asdfhgdsdfhm"])
     	      , BBStr "sfdghgfdg"
       	      ]


bbid =     do char 'B' ; return B 
       <|> do char 'U' ; return U

parseTagOpen = do char '['
                  l <- bbid
                  char ']'
                  return l

parseTagClose l = do char '['
                     char '/'
                     char (head . show $ l)
                     char ']'

parseTag = do l <- try $ parseTagOpen
	      mid <- parseBB
	      parseTagClose l
	      return (BBTag l mid)

parseStr = do s <- many 1 $ satisfy (not . flip elem "[/]")
	      return (BBStr s)

parseBB = do r <- any (parseTag <|> parseStr)
	     return (BBCode r)

*)

fun main s : transaction page =
    case runParser bbid s of
	Some e => return <xml>Tests passed.</xml>
      | _ => return <xml>Tests failed.</xml>

(*

$ urweb test 
test.ur:145:11: (to 146:2) Anonymous function remains at code generation
Function:  (fn _ : {} => "Tests passed.")
test.ur:146:13: (to 147:2) Anonymous function remains at code generation
Function:  (fn _ : {} => "Tests failed.")
:0:0: (to 0:0) Anonymous function remains at code generation
Function:  (fn _ : {} => write(UNBOUND_1 {}))
*)
