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


exports.sendManyImpl = function sendManyImpl (socket,msgs) {
  socket.send(msgs);
};


exports.readImpl = function readImpl (socket) {
  return socket.read();
};


exports.monitorImpl = function monitorImpl (socket,interval,events) {
  socket.monitor(interval,events);
};


exports.unmonitorImpl = function unmonitorImpl (socket) {
  socket.unmonitor();
};


exports.closeImpl = function closeImpl (socket) {
  socket.close();
};


exports.receiveImpl = function receiveImpl (socket,f) {
  var f_ = function f_ () {
    socket.removeListener('message',f_);
    f(arguments);
  };
  socket.on('message',f_);
};


exports.addReceiveListenerImpl = function addReceiveListenerImpl (socket,f) {
  socket.on('message',f);
};


exports.removeAllReceiveListenersImpl = function removeAllReceiveListenersImpl (socket,e) {
  socket.removeAllListeners('message');
};


exports.addMonitorListenerImpl = function addMonitorListenerImpl (socket,e,f) {
  socket.on(e,f);
};


exports.removeAllMonitorListenersImpl = function removeAllMonitorListenersImpl (socket,e) {
  socket.removeAllListeners(e);
};
