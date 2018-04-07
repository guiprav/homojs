window.$ = require('jquery');

window.homo = {
  eval: require('../eval'),
  parse: require('../parse'),
};

window.editor = ace.edit('editor');
editor.setTheme('ace/theme/monokai');
editor.session.setMode('ace/mode/lisp');

$('#runBtn').on('click', () => {
  let src = editor.getValue();
  let moduleScope = {};

  homo.eval.stmts(homo.parse(src), [moduleScope]);
});
