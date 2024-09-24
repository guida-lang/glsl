# Elm GLSL Parser

An Elm package that implements a GLSL (OpenGL Shading Language) parser, inspired by the [Haskell GLSL parser](https://hackage.haskell.org/package/language-glsl). This library allows you to parse GLSL shader code into Elm data structures, enabling further manipulation, analysis, or code generation.

## Getting Started

### Installation

To install the package, run the following:

```bash
elm install guida-lang/glsl
```

### Usage

Here’s an example of how to use the GLSL parser in Elm:

```elm
import Language.GLSL.Parser as Parser
import Language.GLSL.Syntax as Syntax

shaderSource : String
shaderSource =
    """
    void main() {
        gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
    }
    """

parseResult : Result String Syntax.TranslationUnit
parseResult =
    Parser.parse shaderSource
```

### Parsing GLSL

The `parse` function attempts to parse a GLSL shader string and returns a `Result`:

- `Ok Syntax.TranslationUnit`: If the shader was parsed successfully, it returns an abstract syntax
  tree (AST) representing the GLSL code.
- `Err String`: If parsing failed, it returns an error string describing the issue.

### Example

```elm
case parseResult of
    Ok shader ->
        -- Do something with the parsed shader
        Debug.log "Parsed successfully!" shader

    Err error ->
        Debug.log "Failed to parse shader:" error
```

## Documentation

For full API documentation and more examples, please visit the
[Elm package documentation](https://package.elm-lang.org/packages/guida-lang/glsl/1.0.0).

## Contributing

Contributions are welcome! If you have ideas for improvements or find bugs, feel free to open an
issue or submit a pull request.

### To contribute:

1. Fork the repository.
2. Create a new feature branch.
3. Commit your changes.
4. Submit a pull request.
