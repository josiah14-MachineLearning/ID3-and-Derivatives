:- module id3_seq.
:- interface.
:- import_module io.
:- pred main(io :: di, io :: uo) is det.
:- implementation.
:- import_module string, list.

main(!IO) :-
  io.command_line_arguments(Args, !IO),
  io.print(Args, !IO).
