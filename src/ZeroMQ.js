"use strict";

var zmq = require('zeromq');


exports.pair = zmq.types.pair;
exports.pub = zmq.types.pub;
exports.sub = zmq.types.sub;
exports.xpub = zmq.types.xsub;
exports.xsub = zmq.types.xsub;
exports.pull = zmq.types.pull;
exports.push = zmq.types.push;
exports.req = zmq.types.req;
exports.rep = zmq.types.rep;
exports.router = zmq.types.router;
exports.dealer = zmq.types.dealer;


exports.socketImpl = function socketImpl (from) {
  return zmq.socket(from);
};


exports.bindImpl = function bindImpl (socket,addr,onConnect) {
  socket.bind(addr,onConnect);
};


