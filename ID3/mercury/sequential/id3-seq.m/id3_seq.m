:- module id3_seq.
:- interface.
:- import_module io.
:- pred main(io :: di, io :: uo) is det.
:- implementation.

% stl imports
:- import_module
  bool, int, string, list, maybe, require, integer, float, math, stream.

% mmc-get imports
:- import_module csv.

:- pred negate_f(float :: in, float :: out) is det.
negate_f(Num, NegatedNum) :-
  NegatedNum = -Num.

:- pred entropy(int :: in, list(int) :: in, float :: in, float :: out) is det.
entropy(NumRecords, ValueFreqs, LogBase, Entropy) :-
  ItemProbs = map(func(Freq) = float(Freq) / float(NumRecords), ValueFreqs)
  , negate_f(
      foldl(func(P, A) = (P * log(LogBase, P)) + A, ItemProbs, 0.0), Entropy
    ).

main(!IO) :-
  io.command_line_arguments(Args, !IO),
  io.print(Args, !IO),
  io.nl(!IO),
  entropy(7, [3, 2, 2], 2.0, Entropy),
  io.print(Entropy, !IO).
