module NodeLike = (M: {type t;}) => {
  [@bs.get] external childNodes: M.t => Dom.nodeList = "childNodes";

  let asElement_: M.t => Js.Nullable.t(Dom.element) = [%raw
    {|
    function(node) {
      return (node instanceof Element) ? node : null;
    }
    |}
  ];
  let asElement = self => self->asElement_->Js.Nullable.toOption;

  [@bs.get] external textContent: M.t => string = "textContent";
};

module ElementLike = (M: {type t;}) => {
  [@bs.get] external attributes: M.t => Dom.namedNodeMap = "attributes";

  [@bs.get] external localName: M.t => string = "localName";

  [@bs.get] [@bs.return nullable]
  external namespaceURI: M.t => option(string) = "namespaceURI";
};