#!/usr/bin/env node
let eval = require('./eval');
let fs = require('fs');
let parse = require('./parse');

let srcPath = process.argv[2];

if (srcPath === '-') {
  srcPath = '/dev/stdin';
}

let program = parse(
  fs.readFileSync(srcPath, {
    encoding: 'utf8',
  })
);

eval.stmts(program);
