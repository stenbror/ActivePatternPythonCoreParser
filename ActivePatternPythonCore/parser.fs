
namespace PythonCore

module PythonCoreParser =

    open System

    exception SyntaxError of int * string

    type Token =
        |   Name of int * int * string
        |   Number of int * int * string
        |   String of int * int * string
        |   True of int * int 
        |   False of int * int 
        |   Ellipsis of int * int 
        |   None of int * int 
        |   LeftParen of int * int 
        |   LeftBracket of int * int 
        |   LeftCurly of int * int
        |   EOF of int

        member this.getStartPosition : int * int =
            match this with
            |   Name(s, e, _) -> (s, e)
            |   Number(s, e, _) -> (s, e)
            |   String(s, e, _) -> (s, e)
            |   True(s, e) -> (s, e)
            |   False(s, e) -> (s, e)
            |   Ellipsis(s, e) -> (s, e)
            |   None(s, e) -> (s, e)
            |   LeftParen(s, e) -> (s, e)
            |   LeftBracket(s, e) -> (s, e)
            |   LeftCurly(s, e) -> (s, e)
            |   EOF(s) -> ( s, -1 )


    type TokenStream = Token list

    let getStartNextToken(stream: TokenStream) : (int * int) =
        match stream with
        |   head :: rest -> head.getStartPosition
        |   [] -> ( -1, -1 )

    type ASTNode =
        |   Name of int * int * Token
        |   Number of int * int * Token
        |   String of int * int * Token array
        |   True of int * int * Token
        |   False of int * int * Token
        |   Ellipsis of int * int * Token
        |   None of int * int * Token
        |   Empty



    let (|Atom|) (tokens: TokenStream) : (ASTNode * TokenStream) =
        match tokens with
        |   head :: rest ->
                let s, e = head.getStartPosition
                match head with
                |   Token.Name(_) ->  ( ASTNode.Name(s, e, head), rest )
                |   Token.Number(_) ->  ( ASTNode.Number(s, e, head), rest )
                |   Token.String(_) ->
                        let mutable nodes : Token list = List.Empty
                        let mutable finalRest = rest
                        let mutable finalEndPos = e
                        nodes <- head :: nodes
                        while   match finalRest with
                                |   head2 :: rest2 ->
                                        match head2 with
                                        |   Token.String(_, e2, _) ->
                                                nodes <- head2 :: nodes
                                                finalRest <- rest2
                                                finalEndPos <- e2
                                                true
                                        |   _ -> false

                                |   [] -> false
                                do ()
                        ( ASTNode.String(s, finalEndPos, List.toArray (List.rev nodes)), finalRest )
                |   Token.False(_) ->  ( ASTNode.False(s, e, head), rest )
                |   Token.True(_) ->  ( ASTNode.True(s, e, head), rest )
                |   Token.Ellipsis(_) ->  ( ASTNode.Ellipsis(s, e, head), rest )
                |   Token.None(_) ->  ( ASTNode.None(s, e, head), rest )
                |   Token.LeftParen(_) -> raise ( SyntaxError(s, "Not implemented rule!") )
                |   Token.LeftBracket(_) -> raise ( SyntaxError(s, "Not implemented rule!") )
                |   Token.LeftCurly(_) -> raise ( SyntaxError(s, "Not implemented rule!") )
                |   _ -> raise ( SyntaxError(s, "Expecting Atom literal!") )
        |   [] -> raise ( SyntaxError(-1, "Missing Token for parsing!") )
        

    let getAtom(stream: TokenStream) : (ASTNode * TokenStream) =
        match stream with
        |   Atom (a, b) -> a, b
        
