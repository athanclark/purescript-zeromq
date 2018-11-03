"use strict";

var zmq = require('zeromq');


exports.pair = 'pair';
exports.pub = 'pub';
exports.sub = 'sub';
exports.xpub = 'xsub';
exports.xsub = 'xsub';
exports.pull = 'pull';
exports.push = 'push';
exports.req = 'req';
exports.rep = 'rep';
exports.router = 'router';
exports.dealer = 'dealer';


exports.socketImpl = function socketImpl (from) {
  return zmq.socket(from);
};


exports.zmqIdentity = zmq.ZMQ_IDENTITY;


exports.setOptionImpl = function setOptionImpl (socket,opt,val) {
  socket.setsockopt(opt,val);
};


exports.getOptionImpl = function getOptionImpl (socket,opt) {
  return socket.getsockopt(opt);
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


// exports.sendImpl = function sendImpl (socket,flags,msg) {
//   socket.send(msg,flags);
// };


exports.sendManyImpl = function sendManyImpl (socket,msgs) {
  socket.send(msgs);
};


// exports.readImpl = function readImpl (socket) {
//   return socket.read();
// };


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
    var as = Object.values(arguments);
    socket.removeListener('message',f_);
    f(as);
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


exports.proxyImpl = function proxyImpl (frontend,backend,capture) {
  zmq.proxy(frontend,backend,capture);
};
