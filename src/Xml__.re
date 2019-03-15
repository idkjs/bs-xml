open Belt;

type node;
type elem;
type doc;

type attr;

[@bs.get] external value: attr => string = "value";

type nodeList = Js.Array.array_like(node);

[@bs.get] external length: nodeList => int = "length";

[@bs.send] [@bs.return nullable]
external item: (nodeList, int) => option(node) = "item";

type namedNodeMap;

[@bs.send] [@bs.return nullable]
external getNamedItem: (namedNodeMap, string) => option(attr) =
  "getNamedItem";

[@bs.send] [@bs.return nullable]
external getNamedItemNS:
  (namedNodeMap, ~namespace: option(string), ~localName: string) =>
  option(attr) =
  "getNamedItemNS";

module NodeLike = (M: {type t;}) => {
  [@bs.get] external childNodes: M.t => nodeList = "childNodes";

  let asElement_: M.t => Js.Nullable.t(elem) = [%raw
    {|
    function(node) {
      return (node instanceof Element) ? node : null;
    }
    |}
  ];
  let asElement = self => self->asElement_->Js.Nullable.toOption;
};

module NodeTextContent = (M: {type t;}) => {
  [@bs.get] external nodeTextContent: M.t => string = "textContent";
};

module Node = {
  type t = node;

  include NodeLike({
    type nonrec t = t;
  });
  include NodeTextContent({
    type nonrec t = t;
  });
};

module ElementLike = (M: {type t;}) => {
  [@bs.get] external attributes: M.t => namedNodeMap = "attributes";

  [@bs.get] external localName: M.t => string = "localName";
  [@bs.get] [@bs.return nullable]
  external namespaceURI: M.t => option(string) = "namespaceURI";
};

/* _ to avoid name clash in 'instanceof Element' */
module Element_ = {
  type t = elem;
  include NodeLike({
    type nonrec t = t;
  });
  include ElementLike({
    type nonrec t = t;
  });
  include NodeTextContent({
    type nonrec t = t;
  });

  external asDom: t => Dom.element = "%identity";
  external fromDom: Dom.element => t = "%identity";
};

module Doc = {
  type t = doc;

  include NodeLike({
    type nonrec t = t;
  });

  [@bs.send] [@bs.return nullable]
  external querySelector: (t, string) => option(elem) = "querySelector";

  external asDom: t => Dom.document = "%identity";
};
