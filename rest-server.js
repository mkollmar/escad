// Copyright (C) 2011, 2012, 2013, 2014, 2019 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// 

// Socket-connection (common-lisp data) to ESCAD
var net = require('net');
var escad = new net.Socket();  // to write to escad
var escad_result_list = "[]";

// HTML-REST-connection to HTML-browser
var express = require('express');
var server = express();

// required to translate lisp to JSON:
var parser = require('fast-sexpr');


// ********************************************************************
// COMMUNICATION WITH ESCAD
escad.connect(3000, '127.0.0.1', function() {
    console.log('Connect to escad via socket locally at port 3000 to speak common-lisp...');
});

// Listen to 'data' event, which comes when data from escad is recieved at socket
escad.on('data', function(data) {
    console.log('[REST-SRV] Got DATA from escad: ' + data.toString());
    //var escad_result_list = parser(data).pop();
    if (data.toString().match(/NIL/)) {
	escad_result_list = "[]";
    } else {
	escad_result_list = parser("(" + data + ")"); // gives back data-array
    }
    
    browser_response.status(200).send(escad_result_list.toString());
    console.log('[REST-SRV] Got DATA from escad: ' + data.toString());
});

// Listen to 'close' event, which comes when escad closes socket
escad.on('close', function(data) {
    console.log('escad closed socket!');
});

// Listen to 'connect' event
escad.on('connect', function(data) {
    console.log('REST-SERVER connected to escad!');
//    escad.write(make_JSON_RPC_request_string("ls", []));
});

// Listen to 'drain' event, which comes when write buffer is sent
escad.on('drain', function(data) {
    console.log('all data sent to escad!');
});

function sendRequest2escad(escad_cmd, response, request) {
    console.log("[REST-SRV] Got HTTP request from client: " + request.method + " " + request.url);
    var id = "ESCAD" + JSON.stringify(Date.now());
    ResponsesToBrowser["id"] = response;
    escad.write(escad_cmd);
}

// { "jsonrpc": "2.0", "method": "gibAus", "params": ["Hallo JSON-RPC"], "id": 1 }
function make_JSON_RPC_request_string(command, arg_array, id) {
    var json_rpc;
    if (typeof(arg_array[0]) == 'undefined') {
	json_rpc = "{\"jsonrpc\":\"2.0\",\"method\":\"" + command + "\",\"id\":\"" + id + "\"}\n";
    } else {
	json_rpc = "{\"jsonrpc\":\"2.0\",\"method\":\"" + command + "\", \"params\":" + JSON.stringify(arg_array) +
	    ",\"id\":\"" + id + "\"}\n";
    }

    console.log("[REST-SRV] Sending JSON-RPC request to escad: " + json_rpc);
    return json_rpc;
}



// ********************************************************************
// COMMUNICATION WITH BROWSER (USER)

server.use(express.static(__dirname + '/public')); // set the static files location /public/img will be /img for users
//server.use(express.logger('dev')); // log every request to the console
//server.use(express.bodyParser()); // pull information from html in POST


// Upon first load of homepage send static template-HTML file. All access after that will done via AJAX (AngularJS)
server.get('/escad', function(request, response) {
    console.log("[REST-SRV] Got HTTP request to load escad-browser-client-application: " + request.method + " " + request.url);
    response.sendfile('./public/index.html');
});
 


var EscadGetSymbols = function (req, res, next) {
  console.log('CB1')
  next()
}

var BrowserSendJSONResponse = function (req, res) {
  res.send('Hello from C!')
}

// routes which are available for browser clients (standard media type is JSON):
server.get('/symbols', [EscadGetSymbols, BrowserSendJSONResponse]);

server.get('/symbol/:id', function (request, response) {
    sendRequest2escad("(s \"" + request-params.id + "\")", response, request);
    console.log(request.method + " -> " + request.url + request.params.id);
});

server.get('/symbols-all-data', function (request, response, next) {
    sendRequest2escad("(gsdump)", response, request);
});

server.get('/relations', function (request, response) {
    sendRequest2escad("(lr)", response, request);
});

server.get('/relation/:id', function (request, response) {
    sendRequest2escad("(r \"" + request-params.id + "\")", response, request);
    console.log(request.method + " -> " + request.url + request.params.id);
});

server.get('/taxonomies', function (request, response) {
    sendRequest2escad("(lta)", response, request);
});

server.get('/taxonomy/attribute', function (request, response) {
    sendRequest2escad("(lta \"escad.attribute\")", response, request);
});

server.get('/taxonomy/relation', function (request, response) {
    sendRequest2escad("(lta \"escad.relations\")", response, request);
});

server.get('/taxonomy/symbol', function (request, response) {
    sendRequest2escad("(lta \"escad.symbol\")", response, request);
});
 
server.post('/symbol', function (request, response) {
    console.log(request.method + " -> " + request.url);
});

server.put('/symbol/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});
 
server.delete('/symbol/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});
 

// start server:
server.listen(4000);
console.log("[REST-SRV] Started REST-server: http://127.0.0.1:4000");
