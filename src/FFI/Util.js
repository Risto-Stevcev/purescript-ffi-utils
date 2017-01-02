'use strict';

var Maybe = require('Data.Maybe');

exports['typeof'] = function(a) {
  return typeof a;
}

exports['instanceof'] = function(a) {
  return function(b) {
    if (b !== null && b !== undefined && b.prototype !== undefined) {
      return a instanceof b;
    }
    else {
      return false;
    }
  }
}

exports.stringify = function(prettyPrint) {
  return function(object) {
    return prettyPrint ? JSON.stringify(object, null, 2) : JSON.stringify(object);
  }
}

exports['require'] = function(moduleName) {
  return require(moduleName);
}

exports.parseOptions = function parseOptional(object) {
  var acc = {};

  for (var key in object) {
    var value = object[key];

    if (value.constructor === Object) {
      acc[key] = parseOptional(value);
    }
    else {
      acc[key] = value instanceof Maybe.Just ? value.value0 : value instanceof Maybe.Nothing ? null : value;
    }
  }

  return acc;
}

exports.isNullOrUndefined = function(a) {
  if (a === null || a === undefined) {
    return true;
  }
  else {
    return false;
  }
};

exports.property = function(object) {
  return function(key) {
    return object[key];
  };
};

exports.propertyPath = function(object) {
  return function(paths) {
    for (var i; i < paths.length; i++) {
      object = object[paths[i]];
    };
    return object;
  };
};

exports["property'"] = function(key) {
  try {
    return window[key];
  }
  catch (e) {
    return global[key];
  }
};

exports["propertyPath'"] = function(paths) {
  try {
    var object = window;
  }
  catch (e) {
    var object = global;
  }

  for (var i; i < paths.length; i++) {
    object = object[paths[i]];
  };

  return object;
}

exports.setProperty = function(object) {
  return function(key) {
    return function(value) {
      object[key] = value;
      return {};
    };
  };
};

exports.setPropertyPath = function(object) {
  return function(paths) {
    return function(value) {
      for (var i; i < paths.length; i++) {
        if (i < paths.length - 1) {
          object = object[paths[i]];
        }
        else {
          object[paths[i]] = value;
        }
      };
      return {};
    };
  };
};

exports['new'] = function(object) {
  return new object;
};

exports['window'] = function() {
  return window;
}

exports['global'] = function() {
  return global;
}
