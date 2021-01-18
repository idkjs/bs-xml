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
             child(select("x",_), text->map(int)),
             attribute("x",_)->map(int),
           ),
      y:
        elem
        |> either(
             child(select("y",_), text->map(int)),
             attribute("y",_)->map(int),
           ),
    };

  let line =
    (
      elem => {
        Xml.Decode.{
          start: elem |> child(select("start",_), point),
          end_: elem |> child(select("end",_), point),
          thickness:
            elem |> child(select("thickness",_), text)->optional->mapOptional(int),
        };
      }
    )
    |> Xml.Decode.withName("line",_)
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
