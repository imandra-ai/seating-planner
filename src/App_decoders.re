module D = Decoders;

module Decode = (D: D.Decode.S) => {
  open D;

  let pairs: decoder(list((int, int))) =
    list(
      list(int)
      >>= (
        fun
        | [y1, y2] => succeed((y1, y2))
        | _ => fail("expected 2 elements for a pair")
      ),
    );
};

module Encode = (E: D.Encode.S) => {
  open E;

  let pairs: encoder(list((int, int))) =
    (xs: list((int, int))) =>
      xs
      |> list((x: (int, int)) => [fst(x), snd(x)] |> list(y => int(y)));
};
