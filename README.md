# sawblade

Sawblade is a compiler backend, like qbe/LLVM. It currently works very barely but is able to output valid x86_64 assembly code.
I'm building Sawblade not just to work the way I want, but also as *fast* as I can make it, removing all loop invariants and
have analysis information easily accessible, avoiding computing stuff twice...

Sawblade is designed to cut through code like it's butter, or jump out untouched, which *may* cause injuries. 
Kinda like a sawblade when cutting through tough materials.

## Notes

- The backend produces suitable output for `nasm`, not for `as`. Future versions will consider `as`/`gcc`.

## Design Idea & Spec

The main idea is to be able to specify exactly how pieces of code interface with each other, and let it decide
the registers/flags/memory it uses for the final assembly. 

Sawblade works on blocks. For each block, you can have it return (maybe with some data), or branch to another block (maybe conditionally). Each block can be specified
as a 'function' by adding arguments to it, which enables a special `call` instruction to it. For calls and returns, ABIs can be specified, that is, you can tell the
backend what registers to use for arguments and/or returns, what registers should the block preserve, and anything that is returned through a stack-like pointer. ABIs can
be omitted for callable blocks if they are not exported, because the ABI can be inferred after binding allocation.


ABIs can be either put inline in the block definition or written separately:


```sawblade
abi C { return : int [ rax ... ]; ...  }

block "main" :: C { ... }
```

```sawblade
block "main" :: { return : int [ rax ]; } { ... }
```


Sawblade is an SSA-based language, where each 'binding' can only be defined once per block, and each binding is local to the block. 
Each sawblade statement is one of three:

  - Assignment: define one (or more) bindings with an operation:
    ```sawblade
    %a %b %c = call "myfunction";
    ```
  - Return: return those bindings as the block's result:
    ```sawblade
    %a %b %c
    ```
  - Branch: conditionally or not, to another block:
    ```sawblade
    br @loop_end
    ```
    ```sawblade
    br %flag @loop_body @loop_end
    ```


Names for blocks surrounded in double quotes `"main"` will be marked as export blocks, while  names for blocks using a binding syntax `%main` will be marked as local.

Blocks have to do one of two things:
  - return a value
  - branch to another block

If none of those are explicitly stated, returning a value is the implicit thing. So the last statement is used as the returned value, and if bindings are defined
in that statement, those bindings will be used as the return values (you can ignore part of an instruction's result):

- Here, `%ret-code` will be used as the return value
```sawblade
block %main {
  %ret-code = 0;
}
```
- Here, everything that `myfunction` returns is returned. If a different return spec is given, the backend will make sure to properly convert from one spec to another.
```sawblade
block %main {
  call "myfunction"
}
```

In a future revision, more thought might be put into unspecified block returns, so that the backend can decide if there is information that it might want
returned from a block (i.e some intermediate results used in a computation, or flags set during that computation). The future revision will also include usage
of a `never` return hint (like Rust's `!`).


Last, but not least, explicit (callable or not) labels can be specified, with an optional ABI to be followed if they're callable. If an ABI is specified in the `extern` declaration, every
call to that label will implicitly follow that ABI. If an ABI is not specified for a callable label, each call to it will be able to specify their own abi to call it with. Yes, it is unsafe, as this
project is not designed to write safe code, but rather to explore what can be done with a generator without going straight into assembly.

```sawblade
label "memcpy" ([int, int] -> [int]);
```

Work will be done to narrow down on labels once they're being implemented.

## State of the art

Now Sawblade is still a baby project, but it can compile its first sample! The
code generator needs more work to be correct (e.g correct handling of call
arguments and spills) but at least I have a working sample I can iterate on.


## Working example(s)

- [examples/test.sawblade](./examples/test.sawblade): The initial test, defines
  two blocks, one is exported, one is not, and in one of them it poses a
  restriction for the return register. It also introduces a call and operates
  with the result of the call. The program returns 26.
