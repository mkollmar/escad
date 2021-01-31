// Copyright (C) 2011, 2012, 2013, 2014, 2019, 2020, 2021 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// This server implements the REST-interface for escad.
// For this we communicate with two connections:
// [escad] <-> [rest-server.js] <-> [html-browser + escad-web-interface].

// Socket-connection (via common-lisp data) to ESCAD:
var net = require('net');
var escad = new net.Socket();  // bidirectional TCP-stream to write/read to escad
//var endOfLine = require('os').EOL;

var escad_result_string = "";  // incoming escad data
var responseToBrowser = "";  // handler for the next repsonse if data from escad arrives
var responseType = "text";  // currently 'json' or 'text' (lisp)
var processEscadIncome = undefined;  // if true process attributes of sym/rel

// HTML-REST-connection (via JSON-objects) to HTML-browser:
var express = require('express');
var server = express();

// required to translate lisp to JSON:
var parser = require('fast-sexpr');

// for URL parsing:
const { URL, URLSearchParams } = require('url');

// ********************************************************************
// Functions for data translating


// Translate JSON [["key", ".", "value"]["key", ".", "value"]...] to
// JSON-STRING [["key", "value"], ["key", "value"],...]
function FormatEscadAttributes(InAoA) {
    var arrayLength = InAoA.length;
    var resultString = '[';
    for (var i = 0; i < arrayLength; i++) {
	resultString = resultString + '["' + InAoA[i][0] + '", "' + InAoA[i][2] + '"]';
	if (InAoA[i+1]) { resultString = resultString + ','; }
    }
    return resultString + ']';
}


// Format
// [":ATTRIBUTES",[["url",".","https://github.com/mkollmar/escad"]],":COMMENT","Settings for escad belonging to this view.",":REF_TO","NIL",":REF_FROM","NIL",":TAXONOMY","escad.symbol._escad",":WEIGHT","NIL"]
function FormatEscadIncome(InArray) {
    var arrayLength = InArray.length;
    var resultString = '[';
    for (var i = 0; i < arrayLength; i++) {
	if (InArray[i] == ":ATTRIBUTES") {
	    resultString = resultString + '":ATTRIBUTES",' + FormatEscadAttributes(InArray[i+1]);
	} else {
	    resultString = resultString + '"' + InArray[i] + '"' + ', ' +  '"' + InArray[i+1] + '"';
	}
	if (InArray[i+2]) { resultString = resultString + ','; }
	i++;
    }
    return resultString + ']';
}

// translate JSON [["key", "value"], ["key", "value"],...] to
// escad attribute string "(("key" . "value")("key" . "value")...)"
function JSON2Attr(json) {
    return;
}


// ********************************************************************
// COMMUNICATION WITH ESCAD
escad.connect(3000, '127.0.0.1', function() {
    console.log('Connected to escad via socket locally at port 3000 to speak common-lisp...');
    //client.write('Hello, server! Love, Client.');
});

// Listen to 'data' event, triggered when data from escad is at socket:
escad.on('data', function(data) {
    console.log('[REST-SRV] escad raw: ' + JSON.stringify(data.toString().trim()));
    if (Buffer.isBuffer(data)) {
	if (responseType == 'text') {
	    escad_result_string = JSON.stringify(data.toString().trim());
	    console.log('[REST-SRV] escad.on.text(lisp): ' + escad_result_string);
	} else { // we assume request is for json
	    var JSON_list = parser(data.toString().trim()).pop();
	    if (processEscadIncome && JSON_list) { // we have to process the string?!
		var procStr = FormatEscadIncome(JSON_list); // call function ref
		processEscadIncome = undefined;
		escad_result_string = procStr;
	    } else {
		escad_result_string = JSON.stringify(JSON_list);
	    }
	    console.log('[REST-SRV] escad.on.json: ' + escad_result_string);
	}
    } else { // we got string, no buffer
	escad_result_string = data.toString();
	console.log('[REST-SRV] Got DATA string from escad: ' + escad_result_string);
    }

    if (escad_result_string) {  // avoid errors if there is a empty string
	// emit 'gotEscadData' event if we got at least one word from escad:
	if (escad_result_string.match("\\w+")) {
	    server.emit('gotEscadData', escad_result_string);
	}
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
server.get('/rest/version1/symbol/:id', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    if (request.accepts(['json', 'application/json'])) {
	responseType = 'json';
	processEscadIncome = "sym"; // set ref to function
    } else {  // TODO: give error if expected type not possible
	responseType = 'text';
	processEscadIncome = undefined;
    }
    responseToBrowser = response;
    var StringToEscad = "(o (s \"" + request.params.id + "\"))";
    console.log("[REST-SRV] write to escad: " + StringToEscad);
    escad.write(StringToEscad);
});

// Get all data of one relation with id:
server.get('/rest/version1/relation/:id', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    responseToBrowser = response;
    escad.write("(o (r \"" + request.params.id + "\"))");
});

// Allow to communicate direct via escad lisp-commands:
server.post('/rest/version1/command', function (request, response) {
    var meineURL = new URL('http://127.0.0.1:4000' + request.url);
    if (request.accepts(['json', 'application/json'])) {
	responseType = 'json';
    } else {  // TODO: give error if expected type not possible
	responseType = 'text';
    }
    responseToBrowser = response;
    escad.write(meineURL.searchParams.get('1'));
});

// register to 'gotEscadData' event to send response to browser if escad sent answer:
server.on('gotEscadData', function(data) {
    if (responseType == 'json') {
	responseToBrowser.status(200).type('json').send(data);
    } else {  // TODO: give error if expected type not possible
	responseToBrowser.status(200).type('text').send(data);
    }
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
