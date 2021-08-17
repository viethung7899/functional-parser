# Functional Parsers

This is my first take of trying functional programming with Haskell

Seriously, it is very hard to grasp all the concept of this programming paradigm. However, I really enjoyed about this languages

## How to use

- Install `ghc` and `ghci` in the machine
- Clone this repository
- Run `ghci <hs_file>`
- All parsing statement start with `parse`

## What in this repository?

### [`Parser.hs`](Parser.hs)

A general parser for any type of String

### Example: parse character and string

```ghci
*Parser> parse (char 'a') "abc"
Just ('a',"bc")

*Parser> parse (string "foo") "foobar"
Just ("foo","bar")
```

### [`JsonParser.hs`](JsonParser.hs)

A parser spacialized for JSON string

The grammar of JSON can be found [here](https://www.json.org/json-en.html)

#### Example

```ghci
*JsonParser> parse jsonObject  "{ \"hello\"   : 123, \"foo\": \"bar bar\"}  gssgs"
Just (JsonObject [("hello",JsonNumber 123),("foo",JsonString "bar bar")],"  gssgs")
```

## To-do

- [x] Add `IO`
- [x] Parse floating numbers for JSON
- [X] Implement `ParserError`
- [ ] More parser such as HTML, CSS, math expression
- [ ] Add test
