exports.stmts = (list, scopes) => {
  scopes = scopes || [];

  let ret;

  for (let expr of list) {
    ret = exports.expr(expr, scopes);
  }

  return ret;
};

exports.expr = (list, scopes) => {
  scopes = scopes || [];

  if (list[0] === "'") {
    return list.slice(1);
  }

  let scope = getScope(scopes);

  switch (list[0]) {
    case '%':
    case '*':
    case '**':
    case '+':
    case '-':
    case '/':

    case '&&':
    case '||':

      return evalListValues(list.slice(1), scopes)
        .reduce(reducers[list[0]]);

    case '=': {
      let target = find(
        evalValue(list[1], scopes, 'noDeref'),
        scopes
      );

      return target.scope[target.name] = evalValue(
        list[2], scopes
      );
    }

    case '#': {
      let hash = {};

      for (let pair of list.slice(1)) {
        let k = evalValue(pair[0], scopes, 'noDeref');
        let v = evalValue(pair[1], scopes);

        hash[k] = v;
      }

      return hash;
    }

    case '.': {
      let chain = evalListValues(
        list.slice(1), scopes, 'noDeref'
      );

      let root = typeof chain[0] === 'object' ?
        chain[0] : find(chain[0], scopes).value;

      let prev = global;
      let value;

      value = chain.slice(1).reduce((v, k) => {
        if (v === undefined || v === null) {
          throw new TypeError(
            `Cannot read property '${k}' of ${v}\n` +
            `While execting: ${JSON.stringify(
              list, null, 2
            )}`
          );
        }

        prev = v;
        return v[k];
      }, root);

      if (typeof value === 'function') {
        value = value.bind(prev);
      }

      return value;
    }

    case 'let': {
      let k = evalValue(list[1], scopes, 'noDeref');

      return scope[k] = exports.expr(
        list[2], scopes
      );
    }

    case 'str':
      return list[1];

    case 'fn':
      return (...args) => {
        let fnScope = {};

        for (let [i, argName] of list[1].entries()) {
          fnScope[argName] = args[i];
        }

        return exports.stmts(
          list[2], [...scopes, fnScope]
        );
      };

    default: {
      let fn = evalValue(list[0], scopes);

      if (typeof fn !== 'function') {
        throw new TypeError(
          `${fn} is not a function\n` +
          `While executing: ${JSON.stringify(
            list, null, 2
          )}`
        );
      }

      return fn(...evalListValues(list.slice(1), scopes));
    }
  }
};

function getScope(scopes) {
  scopes = scopes || [];
  return scopes[scopes.length - 1] || global;
}

function find(name, scopes) {
  scopes = scopes || [];

  for (let i = scopes.length - 1; i >= 0; --i) {
    let scope = scopes[i];
    let value = scope[name];

    if (scope[name] !== undefined) {
      return { scope, name, value };
    }
  }

  return { scope: global, name, value: global[name] };
}

function evalListValues(list, scopes, opt) {
  return list.map(x => evalValue(x, scopes, opt));
}

function evalValue(val, scopes, opt) {
  if (Array.isArray(val)) {
    return exports.expr(val, scopes);
  }

  if (opt === 'noDeref' || typeof val !== 'string') {
    return val;
  }

  return find(val, scopes).value;
}

let reducers = {
  '%': (a, b) => a % b,
  '*': (a, b) => a * b,
  '**': (a, b) => a ** b,
  '+': (a, b) => a + b,
  '-': (a, b) => a - b,
  '/': (a, b) => a / b,

  '&&': (a, b) => a && b,
  '||': (a, b) => a || b,
};
