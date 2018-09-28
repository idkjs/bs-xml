# XML parser

```
open Belt;
open Xml;

let result = Xml.parse("<root><item>Item 1</item></root>");
let item1 =
  switch (result) {
  | Result.Ok(
      ElementNode({name: "root", children: [ElementNode({name: "item", children: [TextNode(item1)]})]}),
    ) =>
    Some(item1)
  | _ => None
  };
Js.log(item1); /* Item 1*/
```
