open Xml__;
open Belt;

type decoder('a) = Dom.element => 'a;

exception DecodeError(string);

let attribute =
    (
      name: string,
      ~namespace: option(option(string))=?,
      element: Dom.element,
    ) => {
  let element = element->Element_.fromDom;
  let attrs = element->Element_.attributes;
  switch (namespace) {
  | Some(namespace) =>
    switch (attrs->getNamedItemNS(~namespace, ~localName=name)) {
    | Some(attr) => attr->value
    | None => raise(DecodeError(name ++ " attribute expected"))
    }
  | None =>
    switch (attrs->getNamedItem(name)) {
    | Some(attr) => attr->value
    | None => raise(DecodeError(name ++ " attribute expected"))
    }
  };
};

let name = (element: Dom.element) => {
  let element = element->Element_.fromDom;
  element->Element_.localName;
};

let withName = (element: Dom.element, name: string) => {
  let element_ = element->Element_.fromDom;
  if (element_->Element_.localName != name) {
    raise(
      DecodeError(
        name ++ " element expected, got " ++ element_->Element_.localName,
      ),
    );
  };
  element;
};

let namespace = (element: Dom.element) => {
  let element = element->Element_.fromDom;
  element->Element_.namespaceURI;
};

let withNamespace = (element: Dom.element, namespace: option(string)) => {
  let element_ = element->Element_.fromDom;
  if (element_->Element_.namespaceURI != namespace) {
    raise(
      DecodeError(
        "namespace '"
        ++ namespace->Belt.Option.getWithDefault("")
        ++ "' expected, got '"
        ++ element_->Element_.namespaceURI->Belt.Option.getWithDefault("")
        ++ "'",
      ),
    );
  };
  element;
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
  while ((found^)->Option.isNone && i^ < nodes->length) {
    switch (
      nodes
      ->item(i^)
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
  let nodes = element->Element_.childNodes->Js.Array.from;
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

let childElements = (elem: Dom.element) => {
  elem
  ->Xml__.Element_.fromDom
  ->Xml__.Element_.childNodes
  ->Js.Array.from
  ->Belt.Array.keepMap(Xml__.Element_.asElement)
  ->Belt.Array.map(Xml__.Element_.asDom);
};
