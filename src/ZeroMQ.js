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


exports.bindSyncImpl = function bindSyncImpl (socket,addr) {
  socket.bindSync(addr);
};


exports.unbindImpl = function unbindImpl (socket,addr,onConnect) {
  socket.unbind(addr,onConnect);
};


exports.unbindSyncImpl = function unbindSyncImpl (socket,addr) {
  socket.unbindSync(addr);
};


exports.connectImpl = function connectImpl (socket,addr) {
  socket.connect(addr);
};


exports.disconnectImpl = function disconnectImpl (socket,addr) {
  socket.disconnect(addr);
};


exports.subscribeImpl = function subscribeImpl (socket,addr) {
  socket.subscribe(addr);
};


exports.unsubscribeImpl = function unsubscribeImpl (socket,addr) {
  socket.unsubscribe(addr);
};


exports.sendMore = zmq.ZMQ_SNDMORE;
exports.dontWait = zmq.ZMQ_DONTWAIT;


exports.sendImpl = function sendImpl (socket,flags,msg) {
  socket.send(msg,flags);
};
