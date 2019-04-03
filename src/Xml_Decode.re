open Belt;

type decoder('a) = Dom.element => 'a;

exception DecodeError(string);

// Element properties with different names

[@bs.get] external text: Dom.element => string = "textContent";

[@bs.get] [@bs.return nullable]
external namespace: Dom.element => option(string) = "namespaceURI";

[@bs.get] external name: Dom.element => string = "localName";

let ok = (value: 'a, _: Dom.element) => value;

let error: 'a =
  (msg: string, _: Dom.element) => {
    raise(DecodeError(msg));
  };

let requireSome = opt => {
  switch (opt) {
  | Some(value) => value
  | None => raise(DecodeError("some expected, got none"))
  };
};

let attribute =
    (
      name: string,
      ~namespace: option(option(string))=?,
      element: Dom.element,
    ) => {
  open Xml_Element;
  open Xml_Attribute;
  open Xml_NamedNodeMap;

  let attrs = element->attributes;

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

let requireName = (element: Dom.element, name: string) => {
  open Xml_Element;

  if (element->localName != name) {
    raise(
      DecodeError(name ++ " element expected, got " ++ element->localName),
    );
  };
  element;
};

let withName = (name: string, decoder: decoder('a), element: Dom.element) => {
  let _: Dom.element = requireName(element, name);
  decoder(element);
};

let requireNamespace = (element: Dom.element, namespace: option(string)) => {
  open Xml_Element;

  if (element->namespaceURI != namespace) {
    raise(
      DecodeError(
        "namespace '"
        ++ namespace->Belt.Option.getWithDefault("")
        ++ "' expected, got '"
        ++ element->namespaceURI->Belt.Option.getWithDefault("")
        ++ "'",
      ),
    );
  };
  element;
};

let withNamespace =
    (namespace: option(string), decoder: decoder('a), element: Dom.element) => {
  let _: Dom.element = requireNamespace(element, namespace);
  decoder(element);
};

let optional = (decoder: decoder('a), element) =>
  try (Some(decoder(element))) {
  | DecodeError(_) => None
  };

let child = (selector: Dom.element => bool, decoder: decoder('a), element) => {
  open Xml_Element;
  open Xml_NodeList;

  let nodes = element->childNodes;

  let found = ref(None);
  let i = ref(0);
  while ((found^)->Option.isNone && i^ < nodes->length) {
    let node = nodes->itemUnsafe(i^);

    switch (node->Xml_Node.asElement) {
    | Some(e) =>
      if (selector(e)) {
        found := Some(decoder(e));
      }
    | None => ()
    };

    i := i^ + 1;
  };

  switch (found^) {
  | Some(found) => found
  | None => raise(DecodeError("child not found"))
  };
};

let selectAny = _ => true;

let select = (targetName, ~namespace as targetNamespace=?, element) =>
  if (targetName == element->name) {
    switch (targetNamespace) {
    | Some(targetNamespace) => targetNamespace == element->namespace
    | None => true
    };
  } else {
    false;
  };

let children =
    (
      selector: Dom.element => bool,
      decoder: decoder('a),
      element: Dom.element,
    ) => {
  open Xml_Element;
  open Xml_NodeList;
  let children = element->childNodes;
  let result: array('a) = [||];

  for (i in 0 to children->length - 1) {
    let node = children->itemUnsafe(i);

    switch (node->Xml_Node.asElement) {
    | Some(e) =>
      if (selector(e)) {
        result |> Js.Array.push(decoder(e)) |> ignore;
      }

    | None => ()
    };
  };

  result;
};

let map = (decoder: decoder('a), f: 'a => 'b, elem) => {
  decoder(elem)->f;
};

let mapOptional = (decoder: decoder(option('a)), f: 'a => 'b, elem) => {
  decoder(elem)->Belt.Option.map(f);
};

let andThen = (decoder: decoder('a), f: 'a => decoder('b), elem) => {
  let a = decoder(elem);
  f(a, elem);
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

let float = str => {
  let f = str->Js.Float.fromString;
  if (f->Js.Float.isFinite) {
    f;
  } else {
    raise(DecodeError("float expected"));
  };
};

let int = str =>
  try (int_of_string(str)) {
  | Failure("int_of_string") => raise(DecodeError("int expected"))
  };

let date = str => {
  let d = str->Js.Date.fromString;
  if (d->Js.Date.getTime->Js.Float.isNaN) {
    raise(DecodeError("date expected"));
  } else {
    d;
  };
};

let bool = str =>
  try (str->bool_of_string) {
  | Invalid_argument("bool_of_string") =>
    raise(DecodeError("bool expected"))
  };

let childElements = elem => {
  elem
  ->Xml_Element.childNodes
  ->Xml_NodeList.asArrayLike
  ->Js.Array.from
  ->Belt.Array.keepMap(Xml_Element.asElement);
};