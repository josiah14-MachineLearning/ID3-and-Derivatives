:- module id3_seq.
:- interface.
:- import_module io.
:- pred main(io :: di, io :: uo) is det.
:- implementation.

% mmc-get imports
:- import_module csv.

% stl imports
:- import_module
  bool, int, string, list, maybe, require, integer, float, math, stream.

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
  io.open_input(
    "../../../datasets/acute_diagnoses/diagnosis.data", FileOpenResult, !IO
  ),
  (
      FileOpenResult = ok(InputFileHandle),
      process_csv(InputFileHandle, !IO),
      io.close_input(InputFileHandle, !IO)
  ;
      FileOpenResult = error(_IO_Error),
      error("cannot open input file")
  ),
  entropy(7, [3, 2, 2], 2.0, Entropy),
  io.print(Entropy, !IO).

:- pred process_csv(io.text_input_stream :: in, io :: di, io :: uo) is det.
process_csv(InputFileHandle, !IO) :-
  % A CSV header descriptor (type: csv.header_desc/0) tells the CSV reader
  % whether we expect the file to have a header line and, if so, is there any
  % field width limit for the header fields?
  %
  % If you look at the acute_diagnoses dataset in the diagnosis.data file,
  % you'll see that there is not a header, so we tell the program not to expect
  % one, here.
  HeaderDesc = csv.no_header,
  RecordDesc = [
    csv.field_desc(string([]), csv.no_limit, csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace),
    csv.field_desc(string([]), csv.limited(4), csv.do_not_trim_whitespace)
  ],
  ReaderParams = csv.reader_params(
    csv.ignore_blank_lines,
    csv.no_trailing_fields,
    csv.no_comments,
    csv.no_quotation_mark_in_unquoted_field,
    ('\t')
  ),
  csv.init_reader(
    InputFileHandle, ReaderParams, HeaderDesc, RecordDesc, Reader, !IO
  ),
  stream.get(Reader, MaybeData, !IO),
  (
    MaybeData = ok(Data),
    Data = csv.csv(_StreamName, Header, Records),
    io.write_string("Header: ", !IO),
    io.write(Header, !IO),
    io.nl(!IO),
    io.write_list(Records, "\n", io.write, !IO),
    io.nl(!IO)
  ;
    MaybeData = eof
  ;
    MaybeData = error(Error),
    ErrorMsg = stream.error_message(Error),
    io.stderr_stream(Stderr, !IO),
    io.write_string(Stderr, ErrorMsg, !IO),
    io.nl(Stderr, !IO),
    io.set_exit_status(1, !IO)
  ).

