# FormulaFX
Expression (line based) calculator

Number format:
----

floating point 123.456, 123.456e78
hexadecimal 0x123456
degree (hour):minutes  12:34.56
degree (hour):minutes:seconds  12:34:56.78

When hexadecimal or other special notation is used in the expression, the result is displayed using the same notation is possible.

Supported operators:
----

- `^`
- `*` `/`
- `+` `-`

Supported functions:
----

- `exp`
- `ln`
- `sin`, `cos`, `tan`
- `asin`, `acos`, `atan`
- `sqrt`
- `log`
- `floor`, `ceil`, `round`
- `signum`
- `abs`
- `sinh`, `cosh`, `tanh`
- `hex`

Variables
----

It is possible to define variables. Variable to be defined may be written in any position in the equation, like:

- `a = 458`
- `458 = a`
- `a * 5 = 25`
- `sin(a) = 0.5`

There can be at most one undefined variable used in the equation. If there is no undefined variable, the leftmost variable
is considered to be undefined. The undefined variable may have only one occurence.