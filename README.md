# Nova

A basic programming language written in Rust.

## Building

To build the project, you need to have Rust installed. You can install Rust using [rustup](https://rustup.rs/).
After installing Rust, you can build the project by running:

```bash
cargo build --release
```

## Running

To run the interpreter in REPL mode, you can use the following command:

```bash
cargo run --release
```

You can also run a specific script by providing the script file as an argument:

```bash
cargo run --release path/to/script.nv
```

Example scripts can be found in the [`examples`](examples) directory.

```bash
cargo run --release examples/control_flow.nv
```
