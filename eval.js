exports.expr = (scopes, list) => {
  if (list[0] === "'") {
    return list.slice(1);
  }

  function getScope() {
    return scopes[scopes.length - 1] || global;
  }

  function find(name) {
    for (let i = scopes.length - 1; i >= 0; --i) {
      let scope = scopes[i];
      let value = scope[name];

      if (scope[name]) {
        return { scope, name, value };
      }
    }

    return { scope: global, name, value: global[name] };
  }

  let scope = getScope();

  switch (list[0]) {
    case '+':
    case '-':
    case '*':
    case '/':
    case '%': {
      let reducer = {
        '+': (a, b) => a + b,
        '-': (a, b) => a - b,
        '*': (a, b) => a * b,
        '/': (a, b) => a / b,
        '%': (a, b) => a % b,
      }[list[0]];

      return list.slice(1)
        .map(
          x => !Array.isArray(x) ?
            scope[x] : exports.expr(scopes, x)
        )
        .reduce(reducer);
    }

    case '.': {
      let root = find(list[1]);
      let prev = global;

      let value = list.slice(2).reduce((v, k) => {
        prev = v;
        return v[k];
      }, root.value);

      if (typeof value === 'function') {
        value = value.bind(prev);
      }

      return value;
    }

    case 'let':
      return scope[list[1]] = exports.expr(
        scopes, list[2]
      );

    case 'str':
      return list[1];

    case 'fn':
      return (...args) => {
        let fnScope = {};

        for (let [i, argName] of list[1].entries()) {
          fnScope[argName] = args[i];
        }

        return exports.stmts(
          [...scopes, fnScope], list[2]
        );
      };

    default: {
      let fn = !Array.isArray(list[0]) ?
        scope[list[0]] : exports.expr(scopes, list[0]);

      return fn(...list.slice(1).map(
        x => !Array.isArray(x) ?
          scope[x] : exports.expr(scopes, x)
      ));
    }
  }
};

exports.stmts = (scopes, list) => {
  let ret;

  for (let expr of list) {
    ret = exports.expr(scopes, expr);
  }

  return ret;
};
