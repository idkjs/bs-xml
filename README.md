

# bs-xml

Parse XML with DOMParser

API is similar to [@glennsl/bs-json](https://github.com/glennsl/bs-json)

## Example

```reason
type line = {
  start: point,
  end_: point,
  thickness: option(int),
}
and point = {
  x: int,
  y: int,
};

module Decode = {
  let point = elem =>
    Xml.Decode.{
      x:
        elem
        |> either(
             child("x", text)->andThen(int),
             attribute("x")->andThen(int),
           ),
      y:
        elem
        |> either(
             child("y", text)->andThen(int),
             attribute("y")->andThen(int),
           ),
    };

  let line = elem =>
    Xml.Decode.{
      start: elem |> child("start", point),
      end_: elem |> child("end", point),
      thickness: elem |> optional(child("thickness", text->andThen(int))),
    };
};

let data = {|
<line>
    <start>
        <x>10</x>
        <y>20</y>
    </start>
    <end x="30">
        <y>40</y>
    </end>
</line>
|};

let parser = Xml.DomParser.make();
let result = parser->Xml.DomParser.parseXml(data);
|};
```