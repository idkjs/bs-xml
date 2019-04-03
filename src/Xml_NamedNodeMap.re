type t = Dom.namedNodeMap;

[@bs.send] [@bs.return nullable]
external getNamedItem: (t, string) => option(Dom.attr) = "getNamedItem";

[@bs.send] [@bs.return nullable]
external getNamedItemNS:
  (t, ~namespace: option(string), ~localName: string) => option(Dom.attr) =
  "getNamedItemNS";