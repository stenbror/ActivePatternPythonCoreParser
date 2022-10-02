module TestExpressionPatterns

open System
open Xunit

open PythonCore.PythonCoreParser

[<Fact>]
let ``Atom pattern for name literal`` () =
    let stream : TokenStream = [ Token.Name(0, 1, "a") ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.Name(0, 1, Token.Name(0, 1, "a")), node )

[<Fact>]
let ``Atom pattern for number literal`` () =
    let stream : TokenStream = [ Token.Number(0, 1, "1") ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.Number(0, 1, Token.Number(0, 1, "1")), node )

[<Fact>]
let ``Atom pattern for single string literal`` () =
    let stream : TokenStream = [ Token.String(0, 1, "a") ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.String(0, 1, [| Token.String(0, 1, "a") |] ), node )

[<Fact>]
let ``Atom pattern for multiple string literal`` () =
    let stream : TokenStream = [ Token.String(0, 1, "a"); Token.String(2, 3, "b") ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.String(0, 3, [| Token.String(0, 1, "a"); Token.String(2, 3, "b") |] ), node )

[<Fact>]
let ``Atom pattern for True literal`` () =
    let stream : TokenStream = [ Token.True(0, 3) ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.True(0, 3, Token.True(0, 3)), node )

[<Fact>]
let ``Atom pattern for False literal`` () =
    let stream : TokenStream = [ Token.False(0, 4) ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.False(0, 4, Token.False(0, 4)), node )

[<Fact>]
let ``Atom pattern for ... literal`` () =
    let stream : TokenStream = [ Token.Ellipsis(0, 2) ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.Ellipsis(0, 2, Token.Ellipsis(0, 2)), node )

[<Fact>]
let ``Atom pattern for None literal`` () =
    let stream : TokenStream = [ Token.None(0, 3) ]
    let node, rest  = getAtom (stream)
    Assert.Equal(ASTNode.None(0, 3, Token.None(0, 3)), node )