let fs = require('fs');
let peg = require('pegjs');

let grammar = fs.readFileSync(
  `${__dirname}/grammar.pegjs`, {
    encoding: 'utf8'
  }
);

let parser = peg.generate(grammar);

module.exports = parser.parse.bind(parser);
