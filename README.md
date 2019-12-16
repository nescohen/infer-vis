# Infer Visualizer Tool

### Preface

This is a tool intended to help visualizing the output of infer in a
curses-style terminal interface. I found it to sometimes be really confusing to
follow the bugtrace offered by infer. This is meant to be easy to use and allow
you to quickly get an understanding of the general picture of your infer
results. Right now this is very much a work-in-progress and is lacking a lot of
functionality.  Any contributions or forks are welcome.

### Build and Installation

For build and installation, I recommend using
[stack](https://docs.haskellstack.org/en/stable/README/) if you are new to
haskell. If you are familiar with cabal then that should also work.

I am currently using GHC version 8.4.3 and cabal-install 3.0. You can install
these tools easily using [ghcup](https://www.haskell.org/ghcup/)

For now I am not providing any binary releases because I feel like there will
not be much damand. Feel free to message me if that is something you would be
interested in.

### Usage

After installing the infer-vis binary to your PATH, navigate to your project
directory (where infer has already analyzed) and run:

`infer-vis`

Optionally you may specify the directory or report file directly:

`infer-vis --infer-out="directory"`

`infer-vis --report-file="some/report.json"`

### Contributing

I am currently planning to add functionality such that infer-vis will
automatically give the bugtrace more context from the source files themselves.
However, I welcome any and all contributions. Please feel free to PM me or just
submit a PR.
