'use strict';

exports.logAny = function(object) {
  return function() {
    console.log(object);
  }
}

exports.logObject = exports.logAny

exports.logStringify = function(object) {
  return function() {
    console.log(JSON.stringify(object, null, 2));
  }
}
