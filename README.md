# diy-hazelnut

A template for you to implement [Hazelnut](https://arxiv.org/pdf/1607.04180) yourself

## Setup

### Installing and Setting Up OCaml

You will need to have the OCaml toolchain installed. If you have not already it installed, I recommend the official [Get Up and Running With OCaml](https://ocaml.org/learn/tutorials/up_and_running.html) tutorial.

Once you do that, I recommend you setup an opam switch just for this project. You can think of a switch like an isolated environment to install opam packages. The packages you install in one switch won't interfere with the packages installed in another switch. This project has only been tested against OCaml 4.13.1, so we'll be setting up the switch with that version.

To create the switch:
```sh
opam switch create diy-hazelnut 4.13.1
```

To set this switch as the currently active one:
```sh
opam switch set diy-hazelnut
```

To list all the switches you have:
```sh
opam switch list
```

To learn more about switches:
```sh
man opam-switch
```

### Installing Dependencies

If you setup a switch for this project, make sure that it's active:
```sh
opam switch set diy-hazelnut
```

To install the dependencies to the currently active switch:
```sh
make deps
```

### Building, Running, etc.

All the build commands are managed by the `Makefile`. You're free to modify it to your own liking, but as it is provided, here's what the commands do:

To autoformat your code:
```sh
make fmt
```

To build the webapp:
```sh
make build
```

To autoformat and build:
```sh
make
```

To get the URL for the webapp:
```sh
make url
```

To erase the build:
```sh
make clean
```

## Implementing Hazelnut

Now it's your turn to implement Hazelnut!

First of all, it's important that you understand what Hazelnut is and how it works. Read *[Hazelnut: A Bidirectionally Typed Structure Editor Calculus][hazelnut_paper]* if you haven't already. You don't have to understand everything right at this moment, but make sure that you have a good overview of how it all works.

You'll be using the Reason programming language, which essentially just OCaml with a more JavaScript-like syntax. It's the primary language used to implement [Hazel](https://github.com/hazelgrove/hazel). If you're already familiar with OCaml, but not Reason, [this website](https://reasonml.github.io/en/try) can be used to translate between OCaml and Reason. But if you'd really just prefer to use OCaml, you can convert code from OCaml to Reason using `refmt` as follows:

```refmt <file>.re -p ml > <file>.ml; rm <file>.re```

All the code you will write should go in the [`hazelnut/hazelnut.re`](hazelnut/hazelnut.re). The starter code in there has been provided for your convenience, but you're free to modify it however you like. Modifying any other files (especially `hazelnut/hazelnut.rei`) isn't recommended, since it might cause unexpected problems.

To implement Hazelnut, you will need to complete the following functions:

| Function     | Description                                                                                                                       |
| :----------- | :-------------------------------------------------------------------------------------------------------------------------------- |
| `erase_exp`  | Performs *cursor erasure* described in [Hazelnut Part 3.2][hazelnut_paper].                                                       |
| `syn`        | Performs *synthesis* described in [Hazelnut Part 3.1][hazelnut_paper]. Returns `None` if the expression cannot sythesize a type.  |
| `syn_action` | Performs a *synthetic action* described in [Hazelnut Part 3.3][hazelnut_paper]. Returns `None` if the action cannot be performed. |

You are not just welcomed, but encouraged to write your own helper functions.

To test out your implementation while it's incomplete, you can use `raise(Unimplemented)` to fill the gaps. The webapp will warn you if it ever reaches an unimplemented part, but it won't crash!

A `typctx` is a map from variable names to types. You can insert/update values with `TypCtx.add`, and read values with `TypCtx.find`. See the [OCaml Map documentation](https://v2.ocaml.org/api/Map.Make.html) for more details.

## Using the Webapp

Once you've built the webapp with `make build`, you can open it in your browser. Run the command `make url` to get the URL.

The first thing you see will be the Hazelnut expression you're building, and the Hazelnut type that it synthesizes. Below that are the buttons that perform actions on the expression. If the button has an input field, you have to fill that in before pressing the button.

If anything unexpected happens, a warning will appear at the bottom. Here's what each warning means:

| Warning           | Meaning                                                                                                                                                |
| :---------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------- |
| Invalid action    | According to your implementation, that action cannot performed on the current expression.                                                              |
| Invalid input     | The provided input isn't valid.                                                                                                                        |
| Unimplemented     | You called upon an operation that you haven't implemented yet.                                                                                         |
| Theorem violation | Your implementation has a bug that caused it to violate a Hazelnut metatheorem. Note: This won't catch every metatheorem violation, only some of them. |

## Using the Maybe Monad

Have you ever written code that looks like this?

```reason
switch (a) {
| Some(b) =>
  switch (f(b)) {
  | Some(c) =>
    switch (g(c)) {
    | Some(d) => Some(h(d))
    | None => None
    }
  | None => None
  }
| None => None
};
```

That code is quite messy, even though it's just trying to express some simple logic: if the value is `Some(_)`, do something to it, but if it's `None`, just return `None`. Luckily there's a thing called the maybe monad. A category theorist could tell you all kinds of cool properties of monads, but for now, all you need to know is that they make it a lot easier to work with `option('a)` types.

The following is equivalent the block of code above, but much cleaner:

```reason
let* b = a;
let* c = f(b);
let+ d = g(c);
h(d);
```

Use `let*` if the expression below it evaluates to an `option('a)` type. Use `let+` if the expression below it evaluates to a `'a` type.

To use this monad syntax, uncomment `open Monad_lib.Monad;` at the top of the file.

<!-- Link aliases -->

[hazelnut_paper]: https://arxiv.org/pdf/1607.04180
