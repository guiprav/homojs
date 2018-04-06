let eval = require('./eval');

eval.stmts([], [
  ['let', 'log', ['fn', ['msg'], [
    [['.', 'console', 'log'], 'msg'],
  ]]],

  ['log', ['str', `Hello, world!`]],
  ['log', ["'", 'a', 'b', 1, 2, 3]],

  ['let', 'values', ["'", 1, 2, 3, 4]],

  ['log', [['.', 'values', 'reduce'], ['fn', ['a', 'b'], [
    ['+', 'a', 'b'],
  ]]]],
]);
