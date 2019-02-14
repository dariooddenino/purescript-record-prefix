# Purescript-Record-Prefix

A blazing fast library to add prefixes to records' labels.

## Example

```purs
foo :: { bar :: Int, baz :: Boolean }
foo = { bar: 1, baz: true }

bar :: { prebar :: Int, prebaz :: Boolean }
bar = add (SProxy :: SProxy "pre") foo

baz :: { bar :: Int, baz :: Boolean }
baz = remove (SProxy :: SProxy "pre") bar
```
