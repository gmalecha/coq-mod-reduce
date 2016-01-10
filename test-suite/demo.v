Require Import ModularRed.ModReduce.

Definition foo (x : Prop) : Prop := x.
Definition bar (x : Prop) : Prop := x.

Declare Delta Collection delta_foo := [ foo ].
Declare Delta Collection delta_bar := [ bar ].

Ltac test :=
  match goal with
  | |- ?G =>
    let G' := eval mod_reduce beta iota zeta delta [ ? delta_foo bar ] in G in
    change G'
  end.

Goal foo (bar True).
  test.
  match goal with
  | |- True => exact I
  end.
Qed.