open Xml__;

type t = Dom.document;

include NodeLike({
  type nonrec t = t;
});

[@bs.send] [@bs.return nullable]
external querySelector: (t, string) => option(Dom.element) = "querySelector";