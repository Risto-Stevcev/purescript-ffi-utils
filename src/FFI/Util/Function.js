'use strict';

exports._mkError = function (Just) {
  return function (Nothing) {
    return function (e) {
      if (e instanceof Error) {
        return Just(e);
      }
      else {
        return Nothing;
      };
    };
  };
};

exports.apply = function (fn) {
  return function (args) {
    return fn.apply(null, args);
  };
};

exports.bind = function (f) {
  return function (a) {
    return f.bind(a);
  };
};

exports._call0 = function (obj, method) {
  return obj[method]();
};

exports._call1 = function (_Just, _Nothing, obj, method, arg1) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1);
};

exports._call2 = function (_Just, _Nothing, obj, method, arg1, arg2) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1
    , arg2 instanceof Just ? arg2.value0 : arg2 instanceof Nothing ? null : arg2
  );
};

exports._call3 = function (_Just, _Nothing, obj, method, arg1, arg2, arg3) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1
    , arg2 instanceof Just ? arg2.value0 : arg2 instanceof Nothing ? null : arg2
    , arg3 instanceof Just ? arg3.value0 : arg3 instanceof Nothing ? null : arg3
  );
};

exports._call4 = function (_Just, _Nothing, obj, method, arg1, arg2, arg3, arg4) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1
    , arg2 instanceof Just ? arg2.value0 : arg2 instanceof Nothing ? null : arg2
    , arg3 instanceof Just ? arg3.value0 : arg3 instanceof Nothing ? null : arg3
    , arg4 instanceof Just ? arg4.value0 : arg4 instanceof Nothing ? null : arg4
  );
};

exports._call5 = function (_Just, _Nothing, obj, method, arg1, arg2, arg3, arg4, arg5) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1
    , arg2 instanceof Just ? arg2.value0 : arg2 instanceof Nothing ? null : arg2
    , arg3 instanceof Just ? arg3.value0 : arg3 instanceof Nothing ? null : arg3
    , arg4 instanceof Just ? arg4.value0 : arg4 instanceof Nothing ? null : arg4
    , arg5 instanceof Just ? arg5.value0 : arg5 instanceof Nothing ? null : arg5
  );
};

exports._call6 = function (_Just, _Nothing, obj, method, arg1, arg2, arg3, arg4, arg5, arg6) {
  var Just = _Just().constructor;
  var Nothing = _Nothing.constructor;
  return obj[method](arg1 instanceof Just ? arg1.value0 : arg1 instanceof Nothing ? null : arg1
    , arg2 instanceof Just ? arg2.value0 : arg2 instanceof Nothing ? null : arg2
    , arg3 instanceof Just ? arg3.value0 : arg3 instanceof Nothing ? null : arg3
    , arg4 instanceof Just ? arg4.value0 : arg4 instanceof Nothing ? null : arg4
    , arg5 instanceof Just ? arg5.value0 : arg5 instanceof Nothing ? null : arg5
    , arg6 instanceof Just ? arg6.value0 : arg6 instanceof Nothing ? null : arg6
  );
};


exports._callEffect0 = exports._call0;
exports._callEffect1 = exports._call1;
exports._callEffect2 = exports._call2;
exports._callEffect3 = exports._call3;
exports._callEffect4 = exports._call4;
exports._callEffect5 = exports._call5;
exports._callEffect6 = exports._call6;
