'use strict';

exports['typeof'] = function (a) {
  return typeof a;
}

exports['instanceof'] = function (a) {
  return function (b) {
    if (b !== null && b !== undefined && b.prototype !== undefined) {
      return a instanceof b;
    }
    else {
      return false;
    }
  }
}

exports.stringify = function (prettyPrint) {
  return function (object) {
    return prettyPrint ? JSON.stringify(object, null, 2) : JSON.stringify(object);
  }
}

exports['require'] = function (moduleName) {
  return require(moduleName);
}

exports._parseOptions = function parseOptional(justValue, nothingValue, object) {
  var acc = {};

  for (var key in object) {
    var value = object[key];

    if (value.constructor === Object) {
      acc[key] = parseOptional(justValue, nothingValue, value);
    }
    else {
      acc[key] = value.constructor === justValue.constructor
        ? value.value0
        : value.constructor === nothingValue.constructor ? null : value;
    }
  }

  return acc;
}

exports.isNullOrUndefined = function (a) {
  if (a === null || a === undefined) {
    return true;
  }
  else {
    return false;
  }
};

exports.property = function (object) {
  return function (key) {
    return object[key];
  };
};

exports.propertyPath = function (object) {
  return function (paths) {
    for (var i = 0; i < paths.length; i++) {
      object = object[paths[i]];
    };
    return object;
  };
};

exports["property'"] = function (key) {
  try {
    return window[key];
  }
  catch (e) {
    return global[key];
  }
};

exports["propertyPath'"] = function (paths) {
  try {
    var object = window;
  }
  catch (e) {
    var object = global;
  }

  for (var i = 0; i < paths.length; i++) {
    object = object[paths[i]];
  };

  return object;
}

exports.setProperty = function (object) {
  return function (key) {
    return function (value) {
      object[key] = value;
      return {};
    };
  };
};

exports.setPropertyPath = function (object) {
  return function (paths) {
    return function (value) {
      for (var i = 0; i < paths.length; i++) {
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

exports['window'] = function () {
  return window;
}

exports['global'] = function () {
  return global;
}

exports['new'] = function (object) {
  return new object;
};

exports._new1 = function (obj, a1) {
  return new obj(a1);
}

exports._new2 = function (obj, a1, a2) {
  return new obj(a1, a2);
}

exports._new3 = function (obj, a1, a2, a3) {
  return new obj(a1, a2, a3);
}

exports._new4 = function (obj, a1, a2, a3, a4) {
  return new obj(a1, a2, a3, a4);
}

exports._new5 = function (obj, a1, a2, a3, a4, a5) {
  return new obj(a1, a2, a3, a4, a5);
}

exports._new6 = function (obj, a1, a2, a3, a4, a5, a6) {
  return new obj(a1, a2, a3, a4, a5, a6);
}

exports._new7 = function (obj, a1, a2, a3, a4, a5, a6, a7) {
  return new obj(a1, a2, a3, a4, a5, a6, a7);
}

exports._newEffect0 = exports['new'];
exports._newEffect1 = exports._new1;
exports._newEffect2 = exports._new2;
exports._newEffect3 = exports._new3;
exports._newEffect4 = exports._new4;
exports._newEffect5 = exports._new5;
exports._newEffect6 = exports._new6;
exports._newEffect7 = exports._new7;
