# abism

Abism is a compiler backend, like qbe/LLVM, but in full amateur mode. It currently works very barely but is able to output valid x86_64 assembly code.

## Notes

- The backend produces suitable output for `nasm`, not for `as`. Future versions will consider `as`/`gcc`.

## Design Idea & Spec

The name  'Abism' is a mix between 'abi' and 'asm'. The main idea is to be able to specify exactly how pieces of code interface with each other, and let it decide
the registers/flags/memory it uses for the final assembly. 

Abism works on blocks. For each block, you can have it return (maybe with some data), or branch to another block (maybe conditionally). Each block can be specified
as a 'function' by adding arguments to it, which enables a special `call` instruction to it. For calls and returns, ABIs can be specified, that is, you can tell the
backend what registers to use for arguments and/or returns, what registers should the block preserve, and anything that is returned through a stack-like pointer. ABIs can
be omitted for callable blocks if they are not exported, because the ABI can be inferred after binding allocation.


ABIs can be either put inline in the block definition or written separately:


```abism
abi C { return : int [ rax ... ]; ...  }

block "main" :: C { ... }
```

```abism
block "main" :: { return : int [ rax ]; } { ... }
```


Abism is an SSA-based language, where each 'binding' can only be defined once per block, and each binding is local to the block. 
Each abism statement is one of three:

  - Assignment: define one (or more) bindings with an operation:
    ```abism
    %a %b %c = call "myfunction";
    ```
  - Return: return those bindings as the block's result:
    ```abism
    %a %b %c
    ```
  - Branch: conditionally or not, to another block:
    ```abism
    br @loop_end
    ```
    ```abism
    br %flag @loop_body @loop_end
    ```


Names for blocks surrounded in double quotes `"main"` will be marked as export blocks, while  names for blocks using a binding syntax `%main` will be marked as local.

Blocks have to do one of two things:
  - return a value
  - branch to another block

If none of those are explicitly stated, returning a value is the implicit thing. So the last statement is used as the returned value, and if bindings are defined
in that statement, those bindings will be used as the return values (you can ignore part of an instruction's result):

- Here, `%ret-code` will be used as the return value
```abism
block %main {
  %ret-code = 0;
}
```
- Here, everything that `myfunction` returns is returned. If a different return spec is given, the backend will make sure to properly convert from one spec to another.
```abism
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

```abism
label "memcpy" ([int, int] -> [int]);
```

Work will be done to narrow down on labels once they're being implemented.

## State of the art

Right now Abism is a baby project. It will only parse one block, expecting it to have an ABI (without checking).
It doesn't support calls or anything else besides returning and assigning constants.


## Working example(s)

- [examples/test.abism](./examples/test.abism): The initial test, defines a block to be exported which returns some constants on predefined registers.
