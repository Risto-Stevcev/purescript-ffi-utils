'use strict';

var Maybe = require('Data.Maybe');

exports.mkError = function(e) {
  if (e instanceof Error) {
    return new Maybe.Just(e);
  }
  else {
    return new Maybe.Nothing();
  }
}

exports.apply = function(fn) {
  return function(args) {
    return fn.apply(null, args);
  };
};

exports.bind = function(f) {
  return function(a) {
    return f.bind(a);
  };
};

exports._call0 = function(obj, method) {
  return obj[method]();
};

exports._call1 = function(obj, method, arg1) {
  return obj[method](arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1);
};

exports._call2 = function(obj, method, arg1, arg2) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    );
};

exports._call3 = function(obj, method, arg1, arg2, arg3) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    , arg3 instanceof Maybe.Just ? arg3.value0 : arg3 instanceof Maybe.Nothing ? null : arg3
                    );
};

exports._call4 = function(obj, method, arg1, arg2, arg3, arg4) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    , arg3 instanceof Maybe.Just ? arg3.value0 : arg3 instanceof Maybe.Nothing ? null : arg3
                    , arg4 instanceof Maybe.Just ? arg4.value0 : arg4 instanceof Maybe.Nothing ? null : arg4
                    );
};

exports._call5 = function(obj, method, arg1, arg2, arg3, arg4, arg5) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    , arg3 instanceof Maybe.Just ? arg3.value0 : arg3 instanceof Maybe.Nothing ? null : arg3
                    , arg4 instanceof Maybe.Just ? arg4.value0 : arg4 instanceof Maybe.Nothing ? null : arg4
                    , arg5 instanceof Maybe.Just ? arg5.value0 : arg5 instanceof Maybe.Nothing ? null : arg5
                    );
};

exports._call6 = function(obj, method, arg1, arg2, arg3, arg4, arg5, arg6) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    , arg3 instanceof Maybe.Just ? arg3.value0 : arg3 instanceof Maybe.Nothing ? null : arg3
                    , arg4 instanceof Maybe.Just ? arg4.value0 : arg4 instanceof Maybe.Nothing ? null : arg4
                    , arg5 instanceof Maybe.Just ? arg5.value0 : arg5 instanceof Maybe.Nothing ? null : arg5
                    , arg6 instanceof Maybe.Just ? arg6.value0 : arg6 instanceof Maybe.Nothing ? null : arg6
                    );
};

exports._call7 = function(obj, method, arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
  return obj[method]( arg1 instanceof Maybe.Just ? arg1.value0 : arg1 instanceof Maybe.Nothing ? null : arg1
                    , arg2 instanceof Maybe.Just ? arg2.value0 : arg2 instanceof Maybe.Nothing ? null : arg2
                    , arg3 instanceof Maybe.Just ? arg3.value0 : arg3 instanceof Maybe.Nothing ? null : arg3
                    , arg4 instanceof Maybe.Just ? arg4.value0 : arg4 instanceof Maybe.Nothing ? null : arg4
                    , arg5 instanceof Maybe.Just ? arg5.value0 : arg5 instanceof Maybe.Nothing ? null : arg5
                    , arg6 instanceof Maybe.Just ? arg6.value0 : arg6 instanceof Maybe.Nothing ? null : arg6
                    , arg7 instanceof Maybe.Just ? arg7.value0 : arg7 instanceof Maybe.Nothing ? null : arg7
                    );
};


exports._callEff0 = exports._call0;
exports._callEff1 = exports._call1;
exports._callEff2 = exports._call2;
exports._callEff3 = exports._call3;
exports._callEff4 = exports._call4;
exports._callEff5 = exports._call5;
exports._callEff6 = exports._call6;
exports._callEff7 = exports._call7;
