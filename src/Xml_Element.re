open Xml__;

type t = Dom.element;

include NodeLike({
  type nonrec t = t;
});
include ElementLike({
  type nonrec t = t;
});

