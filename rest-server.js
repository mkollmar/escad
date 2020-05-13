// Copyright (C) 2011, 2012, 2013, 2014, 2019, 2020 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// This server communicates with two parts: escad-server and a html-browser with escad-client.

// Socket-connection (via common-lisp data) to ESCAD:
var net = require('net');
var escad = new net.Socket();  // bidirectional TCP-stream to write/read to escad
//var endOfLine = require('os').EOL;

var escad_result_string = "";  // raw escad lisp data
var responseToBrowser = "";

// HTML-REST-connection (via JSON-objects) to HTML-browser:
var express = require('express');
var server = express();

// required to translate lisp to JSON:
var parser = require('fast-sexpr');

// for URL parsing:
const { URL, URLSearchParams } = require('url');


// ********************************************************************
// COMMUNICATION WITH ESCAD
escad.connect(3000, '127.0.0.1', function() {
    console.log('Connected to escad via socket locally at port 3000 to speak common-lisp...');
    //client.write('Hello, server! Love, Client.');
});

// Listen to 'data' event, triggered when data from escad is at socket:
escad.on('data', function(data) {
    if (Buffer.isBuffer(data)) {
	escad_result_string = JSON.stringify(data.toString().trim());
	console.log('[REST-SRV] we got a buffer...');
	var list = parser(data.toString().trim()).pop();
	console.log('[REST-SRV] parser:' + JSON.stringify(list));
    } else {
	escad_result_string = data.toString();
	console.log('[REST-SRV] Got DATA string from escad: ' + escad_result_string);
    }
    //var result_clean = result.replace(/\"/g, "");  // remove "
    //console.log('[REST-SRV] DATA stripped: ' + result_clean.toString());
    //if (result_clean.toString().match(/^NIL/)) {
	//escad_result_list = "[]";
    //} else {
	//escad_result_list = parser("(" + result_clean + ")").pop();
    //}

    if (escad_result_string.match("\\w+")) {
	server.emit('gotEscadData', escad_result_string);  // emit 'gotEscadData' event if we got at least one word from escad
    }
});


// Listen to 'close' event, which comes when escad closes socket:
escad.on('close', function(data) {
    console.log('escad closed socket!');
});

// Listen to 'connect' event:
escad.on('connect', function(data) {
    console.log('REST-SERVER connected to escad!');
});


// ********************************************************************
// COMMUNICATION WITH BROWSER (USER)

// set the static files location /public/img will be /img for users
server.use(express.static(__dirname + '/public'));
//server.use(express.logger('dev')); // log every request to the console
//server.use(express.bodyParser()); // pull information from html in POST

// Upon first load of homepage send static template-HTML file. All access after that will done via AJAX (AngularJS)
server.get('/escad', function(request, response) {
    console.log("[REST-SRV] Got HTTP request to load escad-browser-client-application: " + request.method + " " + request.url);
    response.sendfile('./public/index.html');
});

// Get all data of one symbol with id:
server.get('/symbol/:id', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    responseToBrowser = response;
    escad.write("(o (s \"" + request.params.id + "\"))");
});

// Get all data of one relation with id:
server.get('/relation/:id', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    responseToBrowser = response;
    escad.write("(o (r \"" + request.params.id + "\"))");
});

// Allow to communicate direct via escad lisp-commands:
server.post('/command', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    responseToBrowser = response;
    escad.write(meineURL.searchParams.get('1'));
});

// register to 'gotEscadData' event:
server.on('gotEscadData', function(data) {
    responseToBrowser.status(200).type('json').send(escad_result_string);
    console.log('server.on-DATA: ' + escad_result_string);
});

// TODO! create symbol
server.put('/symbol/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});

// TODO! delete symbol
server.delete('/symbol/:id', function (request, response) {
    console.log(request.method + " -> " + request.url);
});
 

// start server:
server.listen(4000);
console.log("[REST-SRV] Started REST-server: http://127.0.0.1:4000");
