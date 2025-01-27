# rlox

Tree-walk Lox interpreter from [Crafting Interpreters](https://craftinginterpreters.com/) written in Rust.

## Additional features

- `break`
- conditional operator (`?:`)
    ```python
    print condition ? 'true' : 'false';
    ```

- anonymous functions
    ```js
    var anonymous = fun(args) {
        // ...
    }
    ```

- static properties and methods
    ```js
    class Math {
        // static method
        class square(n) {
            return n * n;
        }
    }
    ```

- getters
    ```js
    class Circle {
        init(radius) {
            this.radius = radius;
        }

        // getter
        area {
            return 3.141592653 * this.radius * this.radius;
        }
    }
    ```

## Tests

`tests/` contains a modified `jlox` test suite from [the official Lox implementation](https://github.com/munificent/craftinginterpreters).

`test.sh` runs every file and compares the output to the commented output (`///` for an actual comment).
