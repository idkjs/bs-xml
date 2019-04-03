type t = Dom.nodeList;

external asArrayLike: t => Js.Array.array_like(Dom.node) = "%identity";

[@bs.get] external length: t => int = "length";

[@bs.send] [@bs.return nullable]
external item: (t, int) => option(Dom.node) = "item";

[@bs.send] external itemUnsafe: (Dom.nodeList, int) => Dom.node = "item";