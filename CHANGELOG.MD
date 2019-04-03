## 0.3.0

### Breaking changes

* old `Decode.withName` renamed to requireName
* old `Decode.withNamespace` renamed to requireNamespace
* `Decode.withName` is now higher-order decoder
* `Decode.withNamespace` is now higher-order decoder
* `Decode.children` accepts predicate that filters children to apply decoder to
* `Decode.child` accepts predicate function
* `float`, `int`, `date`, `bool` signatures changed, now they can be used with `Decoder.map` instead of `Decoder.andThen`
* a few function parameters reorderings

### Additions

* `Decode.select` - useful for `Decode.child` and `Decode.children`
* `Decode.selectAny`
* `Element`, `Node` and other modules are now public
* `Decode.childElements` decoder
* `Decode.requireSome` helper function
* `Decode.ok` decoder
* `Decode.error` decoder