# HomoJS

Homoiconic JavaScript interpreter.

Example:

```js
eval.stmts([], [
  // let log = (msg) => {
  //   console.log(msg);
  // };
  ['let', 'log', ['fn', ['msg'], [
    [['.', 'console', 'log'], 'msg'],
  ]]],

  // log(`Hello, world!`);
  ['log', ['str', `Hello, world!`]],

  // log(['a', 'b', 1, 2, 3]);
  ['log', ["'", 'a', 'b', 1, 2, 3]],

  // let values = [1, 2, 3, 4];
  ['let', 'values', ["'", 1, 2, 3, 4]],

  // log(values.reduce((a, b) => a + b));
  ['log', [['.', 'values', 'reduce'], ['fn', ['a', 'b'], [
    ['+', 'a', 'b'],
  ]]]],
]);
```

## License

![](https://www.gnu.org/graphics/agplv3-155x51.png)

HomoJS is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Exclusion of warranty

HomoJS is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

A copy of AGPLv3 can be found in [COPYING.](COPYING)
