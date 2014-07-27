// Connection to ESCAD
var net = require('net');
var escad = new net.Socket();  // to write to escad
var message = "";  // data recieved from escad

// Connection to BROWSER
var express = require('express');
var server = express();
var nextResponse;

// ********************************************************************
// ESCAD SIDE
escad.connect(3000, '127.0.0.1', function() {
    console.log('Connect to escad via JSON-RPC locally at port 3000...');
});

// Listen to 'data' event, which comes when data from escad is recieved at socket
escad.on('data', function(data) {
    console.log('[REST-SRV] DATA from escad: ' + escad.remoteAddress + ': ' + data.toString());
    nextResponse.status(200).send(data);
    message=data;
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

// { "jsonrpc": "2.0", "method": "gibAus", "params": ["Hallo JSON-RPC"], "id": 1 }
function make_JSON_RPC_request_string(command, arg_array) {
    var json_rpc;
    if (typeof(arg_array[0]) == 'undefined') {
	json_rpc = "{\"jsonrpc\": \"2.0\", \"method\": \"" + command + "\", \"id\": " + JSON.stringify(Date.now()) + "}\n";
    } else {
	json_rpc = "{\"jsonrpc\": \"2.0\", \"method\": \"" + command + "\", \"params\": " + JSON.stringify(arg_array) +
	    ", \"id\": " + JSON.stringify(Date.now()) + "}\n";
    }

    console.log("[REST-SRV] make_JSON_RPC_request_string:" + json_rpc);
    return json_rpc;
}



// ********************************************************************
// BROWSER SIDE


server.use(express.static(__dirname + '/public')); // set the static files location /public/img will be /img for users
//server.use(express.logger('dev')); // log every request to the console
//server.use(express.bodyParser()); // pull information from html in POST

// Upon first load of homepage send static template-HTML file. All access after that will done via AJAX (Angular)
server.get('/escad', function(request, response) {
    console.log("[REST-SRV] INITIAL SERVE:" + request.method + " -> " + request.url);
    response.sendfile('./public/index.html');
});
 

// routes:
server.get('/symbols', function (request, response) {
    console.log("[REST-SRV]" + request.method + " -> " + request.url);
    nextResponse = response;
    escad.write(make_JSON_RPC_request_string("ls", []));
//    response.status(200).send(["kl", "lllll"]);
});

server.get('/relations', function (request, response) {
    console.log("[REST-SRV]" + request.method + " -> " + request.url);
    nextResponse = response;
    escad.write(make_JSON_RPC_request_string("lr", []));
});
 
server.get('/symbols/:id', function (request, response) {
    console.log(request.method + " -> " + request.url + request.params.id);
});
 
server.post('/symbols', function (request, response) {
    console.log(request.method + " -> " + request.url);
});

server.put('/symbols/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});
 
server.delete('/symbols/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});
 

// start server:
server.listen(4000);
console.log("[REST-SRV] Started REST-server: http://127.0.0.1:4000");
