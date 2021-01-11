(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

module Error : sig
  type t

  val equal : t -> t -> bool

  val pp : Format.formatter -> t -> unit

  val print :
       fmt:Format.formatter
    -> exe:string
    -> debug:bool
    -> quiet:bool
    -> input_name:string
    -> t
    -> unit
  (** [print conf ?fmt ~input_name e] prints the error message corresponding
      to error [e] on the [fmt] formatter (stderr by default). *)
end

val parse_and_format :
     _ Migrate_ast.Traverse.fragment
  -> ?output_file:string
  -> input_name:string
  -> source:string
  -> Conf.t
  -> Conf.opts
  -> (string, Error.t) Result.t
(** [parse_and_format conf ?output_file ~input_name ~source] parses and
    formats [source] as a list of fragments. *)
