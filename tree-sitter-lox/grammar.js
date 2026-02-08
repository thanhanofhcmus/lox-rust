/**
 * @file My take on Lox language
 * @author An Thanh Nguyen <thanhan030301@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "lox",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => "hello"
  }
});
