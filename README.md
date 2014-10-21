# Snappy Bureaucracy Experiment

Experiment integrating Haskell, Snap and Kafka.

# Development Environment

## OS X

`haskakafka` does not compile properly on OS X Mavericks and above when using
the default `c2hs` library. Specifically due to [c2hs OS X availability macros](https://github.com/haskell/c2hs/issues/85).
This can be fixed by manually downloading and installing [c2hs](https://github.com/haskell/c2hs) against the patched `acowley`'s version of [language-c](https://github.com/acowley/language-c).
This will allow you to install `haskakafka`.

