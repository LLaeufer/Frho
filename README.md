# Interpreter for Frho
This is an interpreter for the programming language Frho.

Frho is a programming language defined in an as yet unpublished manuscript by Peter Thiemann from the University of Freiburg and Taro Sekiyama from National Institute of Informatics, Japan.

Preliminary definitions for the [syntax](syntax.md), big-step [semantics](semantics.md) and [type-checker](typechecker.md) can be found in this repository. These definitions are based on the unpublished manuscript, personal communication with Peter Thiemann and got adapted my me (L. Läufer) to fit the concrete implementation of the interpreter better.

## Requirements

This interpreter can be run on a Linux or macOS machine, with the Rust compiler and Cargo installed.

## Running the interpreter

Frho programs can be executed by running

`cargo run interpret <path>` or `cargo run debug <path>` respectively.

For all options see `cargo run help`

## Running the tests

This implementation contains over 200 tests confirming the correct function of this interpreter.

These can be run via the terminal by running: `cargo test`

## Building the interpreter

Frho can be build into a standalone application by running
`cargo build --release`. The resulting executable can be found at `target/release/frho`. The application can be then run like `./frho interpret <path>`.



