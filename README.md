

# bs-xml

DOM Document decoder for BuckleScript

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
             child(select("x"), text->map(int)),
             attribute("x")->map(int),
           ),
      y:
        elem
        |> either(
             child(select("y"), text->map(int)),
             attribute("y")->map(int),
           ),
    };

  let line =
    (
      elem => {
        Xml.Decode.{
          start: elem |> child(select("start"), point),
          end_: elem |> child(select("end"), point),
          thickness:
            elem |> child(select("thickness"), text)->optional->mapOptional(int),
        };
      }
    )
    |> Xml.Decode.withName("line")
    |> Xml.Decode.withNamespace(Some("geometry"));
};

let data = {|
<g:line xmlns:g="geometry">
    <start>
        <x>10</x>
        <y>20</y>
    </start>
    <end x="30">
        <y>40</y>
    </end>
</g:line>
|};

let parser = Xml.DomParser.make();
let line = parser->Xml.DomParser.parseXml(data)->Belt.Result.getExn->Decode.line;
```

## Install

```
npm i @idkjs/bs-xml
```
