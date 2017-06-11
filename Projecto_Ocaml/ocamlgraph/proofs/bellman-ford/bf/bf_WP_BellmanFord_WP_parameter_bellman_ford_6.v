(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.
Require int.MinMax.

(* Why3 assumption *)
Definition unit  := unit.

Parameter qtmark : Type.

Parameter at1: forall (a:Type), a -> qtmark -> a.
Implicit Arguments at1.

Parameter old: forall (a:Type), a -> a.
Implicit Arguments old.

(* Why3 assumption *)
Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a -> b.
Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b -> (map a b).
Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (b:Type) (a:Type), b -> (map a b).
Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (b:Type) (a:Type), forall (b1:b) (a1:a),
  ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Implicit Arguments mk_ref.

(* Why3 assumption *)
Definition contents (a:Type)(v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.
Implicit Arguments contents.

Parameter set1 : forall (a:Type), Type.

Parameter mem: forall (a:Type), a -> (set1 a) -> Prop.
Implicit Arguments mem.

(* Why3 assumption *)
Definition infix_eqeq (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop :=
  forall (x:a), (mem x s1) <-> (mem x s2).
Implicit Arguments infix_eqeq.

Axiom extensionality : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop := forall (x:a),
  (mem x s1) -> (mem x s2).
Implicit Arguments subset.

Axiom subset_trans : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a))
  (s3:(set1 a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1 s3)).

Parameter empty: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments empty.
Unset Contextual Implicit.

(* Why3 assumption *)
Definition is_empty (a:Type)(s:(set1 a)): Prop := forall (x:a), ~ (mem x s).
Implicit Arguments is_empty.

Axiom empty_def1 : forall (a:Type), (is_empty (empty :(set1 a))).

Parameter add: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments add.

Axiom add_def1 : forall (a:Type), forall (x:a) (y:a), forall (s:(set1 a)),
  (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments remove.

Axiom remove_def1 : forall (a:Type), forall (x:a) (y:a) (s:(set1 a)), (mem x
  (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall (a:Type), forall (x:a) (s:(set1 a)),
  (subset (remove x s) s).

Parameter union: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments union.

Axiom union_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments inter.

Axiom inter_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments diff.

Axiom diff_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset (diff s1 s2) s1).

Parameter all: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments all.
Unset Contextual Implicit.

Axiom all_def : forall (a:Type), forall (x:a), (mem x (all :(set1 a))).

Parameter cardinal: forall (a:Type), (set1 a) -> Z.
Implicit Arguments cardinal.

Axiom cardinal_nonneg : forall (a:Type), forall (s:(set1 a)),
  (0%Z <= (cardinal s))%Z.

Axiom cardinal_empty : forall (a:Type), forall (s:(set1 a)),
  ((cardinal s) = 0%Z) <-> (is_empty s).

Axiom cardinal_add : forall (a:Type), forall (x:a), forall (s:(set1 a)),
  (~ (mem x s)) -> ((cardinal (add x s)) = (1%Z + (cardinal s))%Z).

Axiom cardinal_remove : forall (a:Type), forall (x:a), forall (s:(set1 a)),
  (mem x s) -> ((cardinal s) = (1%Z + (cardinal (remove x s)))%Z).

Axiom cardinal_subset : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset s1 s2) -> ((cardinal s1) <= (cardinal s2))%Z.

Parameter vertex : Type.

Parameter vertices: (set1 vertex).

Parameter edges: (set1 (vertex* vertex)%type).

Parameter s: vertex.

Parameter weight: vertex -> vertex -> Z.

Axiom s_in_graph : (mem s vertices).

Axiom edges_def : forall (x:vertex) (y:vertex), (mem (x, y) edges) -> ((mem x
  vertices) /\ (mem y vertices)).

(* Why3 assumption *)
Inductive path : vertex -> vertex -> Z -> Z -> Prop :=
  | path_empty : forall (v:vertex), (path v v 0%Z 0%Z)
  | path_succ : forall (v1:vertex) (v2:vertex) (v3:vertex) (n:Z) (d:Z),
      (path v1 v2 n d) -> ((mem (v2, v3) edges) -> (path v1 v3
      (n + (weight v2 v3))%Z (d + 1%Z)%Z)).

Axiom path_depth_nonneg : forall (v1:vertex) (v2:vertex) (n:Z) (d:Z),
  (path v1 v2 n d) -> (0%Z <= d)%Z.

Axiom path_in_vertices : forall (v1:vertex) (v2:vertex) (n:Z) (d:Z), (mem v1
  vertices) -> ((path v1 v2 n d) -> (mem v2 vertices)).

Axiom path_depth_empty : forall (v1:vertex) (v2:vertex) (n:Z), (path v1 v2 n
  0%Z) -> ((v1 = v2) /\ (n = 0%Z)).

Axiom path_pred_existence : forall (v1:vertex) (v3:vertex) (n:Z) (d:Z),
  (0%Z <= d)%Z -> ((path v1 v3 n (d + 1%Z)%Z) -> exists v2:vertex, (mem (v2,
  v3) edges) /\ (path v1 v2 (n - (weight v2 v3))%Z d)).

(* Why3 assumption *)
Definition shortest_path(v1:vertex) (v2:vertex) (n:Z) (d:Z): Prop := (path v1
  v2 n d) /\ forall (m:Z) (dd:Z), (m <  n)%Z -> ~ (path v1 v2 m dd).

Axiom shortest_path_empty : forall (v:vertex), (mem v vertices) ->
  ((forall (n:Z) (d:Z), (n <  0%Z)%Z -> ~ (path v v n d)) -> (shortest_path v
  v 0%Z 0%Z)).

(* Why3 assumption *)
Definition no_path(v1:vertex) (v2:vertex): Prop := forall (n:Z) (d:Z),
  ~ (path v1 v2 n d).

Axiom no_path_not_same : forall (v:vertex), ~ (no_path v v).

Axiom path_trans : forall (v1:vertex) (v2:vertex) (v3:vertex) (n1:Z) (n2:Z)
  (d1:Z) (d2:Z), (path v1 v2 n1 d1) -> ((path v2 v3 n2 d2) -> (path v1 v3
  (n1 + n2)%Z (d1 + d2)%Z)).

Axiom reach_less_than_n : forall (v1:vertex) (v2:vertex), (mem v1
  vertices) -> forall (d:Z) (n:Z), (path v1 v2 n d) -> exists dqt:Z,
  exists nqt:Z, (dqt <  (cardinal vertices))%Z /\ (path v1 v2 nqt dqt).

Axiom reach_most_n : forall (v1:vertex) (v2:vertex), ((mem v1 vertices) /\
  (mem v2 vertices)) -> ((forall (n:Z) (d:Z), ((0%Z <= d)%Z /\
  (d <  (cardinal vertices))%Z) -> ~ (path v1 v2 n d)) -> (no_path v1 v2)).

(* Why3 assumption *)
Definition negcycle: Prop := exists v:vertex, (mem v vertices) /\ exists n:Z,
  exists d:Z, (n <  0%Z)%Z /\ (path v v n d).

Axiom ignore_negcycle : ~ (negcycle ).

(* Why3 assumption *)
Inductive dist  :=
  | Finite : Z -> dist 
  | Infinite : dist .

(* Why3 assumption *)
Definition infix_plpl(x:dist) (y:dist): dist :=
  match x with
  | Infinite => Infinite
  | (Finite x1) =>
      match y with
      | Infinite => Infinite
      | (Finite y1) => (Finite (x1 + y1)%Z)
      end
  end.

(* Why3 assumption *)
Definition infix_lsls(x:dist) (y:dist): Prop :=
  match x with
  | Infinite => False
  | (Finite x1) =>
      match y with
      | Infinite => True
      | (Finite y1) => (x1 <  y1)%Z
      end
  end.

(* Why3 assumption *)
Definition infix_gtgteq(x:dist) (y:dist): Prop :=
  match x with
  | Infinite => True
  | (Finite x1) =>
      match y with
      | Infinite => False
      | (Finite y1) => (y1 <= x1)%Z
      end
  end.

Parameter min: dist -> dist -> dist.

Parameter max: dist -> dist -> dist.

Axiom Max_is_ge : forall (x:dist) (y:dist), (infix_gtgteq (max x y) x) /\
  (infix_gtgteq (max x y) y).

Axiom Max_is_some : forall (x:dist) (y:dist), ((max x y) = x) \/ ((max x
  y) = y).

Axiom Min_is_le : forall (x:dist) (y:dist), (infix_gtgteq x (min x y)) /\
  (infix_gtgteq y (min x y)).

Axiom Min_is_some : forall (x:dist) (y:dist), ((min x y) = x) \/ ((min x
  y) = y).

Axiom Max_x : forall (x:dist) (y:dist), (infix_gtgteq x y) -> ((max x
  y) = x).

Axiom Max_y : forall (x:dist) (y:dist), (infix_gtgteq y x) -> ((max x
  y) = y).

Axiom Min_x : forall (x:dist) (y:dist), (infix_gtgteq y x) -> ((min x
  y) = x).

Axiom Min_y : forall (x:dist) (y:dist), (infix_gtgteq x y) -> ((min x
  y) = y).

Axiom Max_sym : forall (x:dist) (y:dist), (infix_gtgteq x y) -> ((max x
  y) = (max y x)).

Axiom Min_sym : forall (x:dist) (y:dist), (infix_gtgteq x y) -> ((min x
  y) = (min y x)).

Parameter take: forall (a:Type), (set1 a) -> a.
Implicit Arguments take.

Axiom take_def : forall (a:Type), forall (x:(set1 a)), (~ (is_empty x)) ->
  (mem (take x) x).

(* Why3 assumption *)
Definition bag (a:Type) := (ref (set1 a)).

(* Why3 assumption *)
Definition distmap  := (map vertex dist).

(* Why3 assumption *)
Definition paths(m:(map vertex dist)) (pass:Z) (via:(set1 (vertex*
  vertex)%type)): Prop := forall (v:vertex), (mem v vertices) ->
  (match (get m
  v) with
  | (Finite n) => (exists d:Z, ((0%Z <= d)%Z /\ (d <  pass)%Z) /\ (path s v n
      d)) \/ exists u:vertex, (mem (u, v) via) /\ exists d:Z,
      ((0%Z <= d)%Z /\ (d <  pass)%Z) /\ (path s u (n - (weight u v))%Z pass)
  | Infinite => (forall (d:Z), ((0%Z <= d)%Z /\ (d <  pass)%Z) ->
      forall (n:Z), ~ (path s v n d)) /\ forall (u:vertex), (mem (u, v)
      via) -> forall (d:Z), ((0%Z <= d)%Z /\ (d <  pass)%Z) -> forall (n:Z),
      ~ (path s u n d)
  end /\ ((no_path s v) -> ((get m v) = Infinite))).

Axiom paths_next : forall (m:(map vertex dist)) (p:Z), (paths m p edges) ->
  (paths m (p + 1%Z)%Z (empty :(set1 (vertex* vertex)%type))).

(* Why3 goal *)
Theorem WP_parameter_bellman_ford : (paths (set (const Infinite:(map vertex
  dist)) s (Finite 0%Z)) 1%Z (empty :(set1 (vertex* vertex)%type))) ->
  ((((cardinal vertices) - 1%Z)%Z <  1%Z)%Z -> forall (e:(set1 (vertex*
  vertex)%type)), forall (result:bool), ((result = true) <-> (is_empty e)) ->
  ((~ (result = true)) -> ((~ (is_empty e)) -> forall (e1:(set1 (vertex*
  vertex)%type)), forall (result1:vertex) (result2:vertex), let result3 := (
  result1, result2) in (((mem result3 e) /\ (e1 = (remove result3 e))) ->
  ((~ match (get (set (const Infinite:(map vertex dist)) s (Finite 0%Z))
  result2) with
  | Infinite => False
  | (Finite x) => match (get (set (const Infinite:(map vertex dist)) s
      (Finite 0%Z))
      result1) with
      | Infinite => True
      | (Finite x1) => (x <  (x1 + (weight result1 result2))%Z)%Z
      end
  end) -> ((cardinal e1) <  (cardinal e))%Z))))).
intuition.
intuition.
subst.

destruct (get (set (const Infinite) s (Finite 0)) result2); try trivial.
destruct (get (set (const Infinite) s (Finite 0)) result1) as [] _eqn; try contradiction.

Require Import Classical.
destruct (classic (s = result1)).

rewrite Select_eq in Heqd.
subst.
admit.
(*
assert (subset es edges). admit.
assert (mem (s, result2) edges). admit.
apply edges_def in H11.
destruct H11 as [ H11 [ H12 H13 ]].
apply H8.

apply H8 in H13.

assert (z = 0)%Z. admit.


*)
assumption.

rewrite Select_neq in Heqd.
rewrite Const in Heqd.
admit.
assumption.

Qed.


