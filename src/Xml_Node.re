open Xml__;

type t = Dom.node;

include NodeLike({
  type nonrec t = t;
});