# Purescript-Record-Prefix

A library to add prefixes to records' labels.

## Example

```purs
foo :: { bar :: Int, baz :: Boolean }
foo = { bar: 1, baz: true }


bar :: { prebar :: Int, prebaz :: Boolean }
bar = addPrefix (SProxy :: SProxy "pre") foo

baz :: { bar :: Int, baz :: Boolean }
baz = removePrefix (SProxy :: SProxy "pre") bar
```
