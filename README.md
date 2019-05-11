# ID3-and-Derivatives

This is a repository for "from scratch" implementations of the ID3 algorithm and algorithms derived from it in various programming languages.

## Motivation

This is not intended to be production-level code.  These algorithms are written for learning purposes to solidify the concepts I'm encountering in my studies and research into machine learning.  Feel free to fork this repository and do as you wish with it, but all code in this repository comes with no guarantees as to its functionality or its correctness, let alone its performance, resilience, and stability.  I will not be accepting pull/merge requests into this repository since the aim of this is not to produce real-world code.  However, I would consider forking this repository into a new one for the purpose of writing a library for inclusion in other machine learning projects or to be used as an example of these algorithms for students, which the community could then contribute to, if desired.

## Layout

Three algorithms will be implemented, each under their own directories: The original ID3, the C4.5, and the C5.0 (this algorithm will be built by referencing the [C implementation licensed under the GPL](https://www.rulequest.com/see5-info.html)).

Under each algorithm directory are directories for each of the languages the algorithm was implemented in.  At minimum, everything will have a Haskell implementation.  If value is perceived in it, additional implementations in Python, Rust, C, Lisp, Mercury, and/or Prolog may also be written.

Under each language directory, you may find "parallel" and a "sequential" directories.  If so, the sequential directory contains a fully single-threaded sequential implementation of the algorithm without any concurrency or parallelism.  The parallel directory, on the other hand, will contain a version of the algorithm which takes advantage of identified opportunities for parallelism and/or concurrency.

## License

Please note that this code is licensed under the "GNU AFFERO GENERAL PUBLIC LICENSE".  For the details, please see the LICENSE file in this repository.
