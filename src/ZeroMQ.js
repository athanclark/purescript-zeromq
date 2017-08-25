"use strict";

var zmq = require('zeromq');


exports.registerProducerImpl = function registerProducerImpl (socket, f) {
  var sock = zmq.socket('push');
  sock.bindSync(socket);
  f(function sendImpl (message) {
    sock.send(message);
  });
};

exports.registerWorkerImpl = function registerWorkerImpl (socket, f) {
  var sock = zmq.socket('pull');
  sock.connect(socket);
  sock.on('message', function onMessageImpl (msg) {
    f(msg);
  });
};

exports.registerPublisherImpl = function registerPublisherImpl (socket, f) {
  var sock = zmq.socket('pub');
  sock.bindSync(socket);
  f(function sendImpl (channel,message) {
    sock.send([channel,message]);
  });
  sock.send(["foo","asdf"]);
};

exports.registerSubscriberImpl = function registerSubscriberImpl (socket, channel, f) {
  var sock = zmq.socket('sub');
  sock.connect(socket);
  sock.subscribe(channel);
  sock.on('message', function onMessageImpl (topic, msg) {
    f(msg);
  });
};
