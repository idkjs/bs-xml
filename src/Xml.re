open Belt;

type node;
type elem;
type doc;

module Attr = {
  type t;
  /* [@bs.get] external name: t => string = "name"; */
  [@bs.get] external value: t => string = "value";
};

module NodeList = {
  type t;

  [@bs.get] external length: t => int = "length";
  [@bs.send] [@bs.return nullable]
  external item: (t, int) => option(node) = "item";

  [@bs.val] [@bs.scope "Array"] external toArray: t => array(node) = "from";
};

module NamedNodeMap = {
  type t;

  /* [@bs.get] external length: Attr.t => int = "length";
     [@bs.send] [@bs.return nullable]
     external item: (t, int) => option(Attr.t) = "item"; */

  [@bs.send] [@bs.return nullable]
  external getNamedItem: (t, string) => option(Attr.t) = "getNamedItem";
};

module NodeLike = (M: {type t;}) => {
  /* [@bs.get] [@bs.return nullable]
     external textContent: M.t => option(string) = "textContent"; */
  /* [@bs.get] external nodeName: M.t => string = "nodeName"; */

  [@bs.get] external childNodes: M.t => NodeList.t = "childNodes";

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
  [@bs.get] external attributes: M.t => NamedNodeMap.t = "attributes";

  [@bs.get] [@bs.return nullable]
  external namespaceURI: M.t => option(string) = "namespaceURI";
  [@bs.get] external localName: M.t => string = "localName";
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

module DomParser = {
  type t;

  [@bs.send]
  external parseFromString_: (t, string, string) => Doc.t = "parseFromString";

  let parseFromString = (self, text, type_) => {
    parseFromString_(self, text, type_)->Doc.asDom;
  };

  let parse = (self, text, type_): Result.t(Dom.element, string) => {
    let doc = self->parseFromString_(text, type_);

    switch (doc->Doc.querySelector("parsererror")) {
    | Some(errorElement) => Error(errorElement->Element_.nodeTextContent)
    | None =>
      let nodes = doc->Doc.childNodes->NodeList.toArray;
      switch (nodes->Array.keepMap(Node.asElement)->Array.get(0)) {
      | Some(root) => Ok(root->Element_.asDom)
      | None => Error("root element missing")
      };
    };
  };

  let parseXml = (self, text) => parse(self, text, "text/xml");

  let parseHtml = (self, text) => parse(self, text, "text/html");

  [@bs.new] external make: unit => t = "DOMParser";
};

module Decode = {
  type decoder('a) = Dom.element => 'a;

  exception DecodeError(string);

  let attribute = (name: string, element: Dom.element) => {
    let element = element->Element_.fromDom;
    let attrs = element->Element_.attributes;
    switch (attrs->NamedNodeMap.getNamedItem(name)) {
    | Some(attr) => attr->Attr.value
    | None => raise(DecodeError(name ++ " attribute expected"))
    };
  };

  let name = (element: Dom.element) => {
    let element = element->Element_.fromDom;
    element->Element_.localName;
  };

  let namespace = (element: Dom.element) => {
    let element = element->Element_.fromDom;
    element->Element_.namespaceURI;
  };

  let text = (element: Dom.element) => {
    let element = element->Element_.fromDom;
    element->Element_.nodeTextContent;
  };

  let optional = (decoder: decoder('a), element) =>
    try (Some(decoder(element))) {
    | DecodeError(_) => None
    };

  let find_ =
      (name: string, namespace: option(option(string)), element: Element_.t) =>
    switch (namespace) {
    | None =>
      if (element->Element_.localName == name) {
        Some(element);
      } else {
        None;
      }
    | Some(ns) =>
      if (element->Element_.localName == name
          && element->Element_.namespaceURI == ns) {
        Some(element);
      } else {
        None;
      }
    };

  let child =
      (
        name: string,
        ~namespace: option(option(string))=?,
        decoder: decoder('a),
        element,
      ) => {
    let element = element->Element_.fromDom;
    let nodes = element->Element_.childNodes;

    let found = ref(None);
    let i = ref(0);
    while ((found^)->Option.isNone && i^ < nodes->NodeList.length) {
      switch (
        nodes
        ->NodeList.item(i^)
        ->Option.flatMap(Node.asElement)
        ->Option.flatMap(find_(name, namespace))
      ) {
      | Some(item) => found := Some(item->Element_.asDom)
      | None => ()
      };

      i := i^ + 1;
    };
    switch (found^) {
    | Some(found) => found->decoder
    | None =>
      let msg =
        switch (namespace) {
        | Some(Some(ns)) => "child " ++ name ++ " (" ++ ns ++ ") expected"
        | _ => "child " ++ name ++ " expected"
        };
      raise(DecodeError(msg));
    };
  };

  let children =
      (
        name: string,
        ~namespace: option(option(string))=?,
        decoder: decoder('a),
        element,
      ) => {
    let element = element->Element_.fromDom;
    let nodes = element->Element_.childNodes->NodeList.toArray;
    nodes
    ->Array.keepMap(Node.asElement)
    ->Array.keepMap(find_(name, namespace))
    ->Array.map(e => e->Element_.asDom->decoder);
  };

  let map = (decoder: decoder('a), f: 'a => 'b, elem) => {
    decoder(elem)->f;
  };

  let andThen = (decoder: decoder('a), f: 'a => decoder('b)): decoder('b) => {
    elem => {
      let a = decoder(elem);
      f(a, elem);
    };
  };

  let either = (left: decoder('a), right: decoder('a), elem: Dom.element) =>
    try (left(elem)) {
    | DecodeError(_) => right(elem)
    };

  let withDefault = (decoder, default, elem: Dom.element) =>
    try (decoder(elem)) {
    | DecodeError(_) => default
    };

  let oneOf = (decoders: list(decoder('a)), elem: Dom.element) => {
    let arr = decoders->List.toArray;

    let result = ref(None);

    let i = ref(0);
    while ((result^)->Option.isNone && i^ < arr->Array.length) {
      let d = arr->Js.Array.unsafe_get(i^);
      let res =
        try (Some(d(elem))) {
        | DecodeError(_) => None
        };
      i := i^ + 1;
      result := res;
    };
    switch (result^) {
    | Some(result) => result
    | None => raise(DecodeError("no decoder succeeded"))
    };
  };

  let float = (str, _: Dom.element) => {
    let f = str->Js.Float.fromString;
    if (f->Js.Float.isFinite) {
      f;
    } else {
      raise(DecodeError("float expected"));
    };
  };

  let int = (str, _: Dom.element) =>
    try (int_of_string(str)) {
    | Failure("int_of_string") => raise(DecodeError("int expected"))
    };

  let date = (str, _: Dom.element) => {
    let f = str->Js.Date.parseAsFloat;
    if (f->Js.Float.isNaN) {
      raise(DecodeError("date expected"));
    } else {
      str->Js.Date.fromString;
    };
  };

  let bool = (str, _: Dom.element) =>
    try (str->bool_of_string) {
    | Invalid_argument("bool_of_string") =>
      raise(DecodeError("bool expected"))
    };
};
