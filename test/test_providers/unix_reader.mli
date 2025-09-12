(*_********************************************************************************)
(*_  provider - Dynamic Dispatch with Traits                                      *)
(*_  SPDX-FileCopyrightText: 2024-2025 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: ISC                                                 *)
(*_********************************************************************************)

(** [Unix_reader] is a provider for {!module:Test_interfaces.Directory_reader}
    based on [Unix].

    It is meant to demonstrate how to illustrate how multiple providers may be
    implemented the same interfaces. [Eio_reader] is another provider for the
    {!module:Test_interfaces.Directory_reader} interface.

    The structure of this file is very similar to the [Eio_reader] module, thus
    is not documented in details. Refer to [Eio_reader] for more. *)

type t = unit

val make : t -> Test_interfaces.Directory_reader.tag Provider.packed
