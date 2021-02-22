// Copyright (C) 2021 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// This are tests for the escad REST-API.
//
// USAGE:
// 1. Start escad with "./escad start net-rest"
// 2. Call this via "node rest_api.js"
// ###################################################


var Client = require('node-rest-client').Client;
var client = new Client();
var args = {
    //	data: { test: "hello" }, // data passed to REST method (only useful in POST, PUT or PATCH methods)
    //	path: { "id": 120 }, // path substitution var
    //	parameters: { arg1: "hello", arg2: "world" }, // this is serialized as URL parameters
    requestConfig: {
	//timeout: 1000, //request timeout in milliseconds
	//noDelay: true, //Enable/disable the Nagle algorithm
	keepAlive: false //Enable/disable keep-alive functionalityidle socket.
	//keepAliveDelay: 1000 //and optionally set the initial delay before the first keepalive probe is sent
	},
	//responseConfig: {
	//	timeout: 1000 //response timeout
	//},
	headers: { "Accept": "application/json" } // request headers
};

// for console colors:
const colors = {
    reset: "\x1b[0m",
    bright: "\x1b[1m",
    dim: "\x1b[2m",
    underscore: "\x1b[4m",
    blink: "\x1b[5m",
    reverse: "\x1b[7m",
    hidden: "\x1b[8m",
    
    fg: {
        black: "\x1b[30m",
        red: "\x1b[31m",
        green: "\x1b[32m",
        yellow: "\x1b[33m",
        blue: "\x1b[34m",
        magenta: "\x1b[35m",
        cyan: "\x1b[36m",
        white: "\x1b[37m",
        crimson: "\x1b[38m" // Scarlet
    },
    bg: {
        black: "\x1b[40m",
        red: "\x1b[41m",
        green: "\x1b[42m",
        yellow: "\x1b[43m",
        blue: "\x1b[44m",
        magenta: "\x1b[45m",
        cyan: "\x1b[46m",
        white: "\x1b[47m",
        crimson: "\x1b[48m"
    }
};

// catch a possible not connected error:
process.on('uncaughtException', function (err) {
    console.log(colors.fg.red, 'Hey, have you forgotten to start escad via "./escad start net-rest"? Following error occured:', colors.reset);
    console.log('<<' + err + '>>');
});


function Test01() {
    console.log("1. (symbol)... ")
    client.get("http://127.0.0.1:4000/rest/version1/symbol/_view", args, function (data, response) {
	console.log(data); // parsed response body as js object
	    /^\[":ATTRIBUTES",.+\]$/.test(data) && console.log("ok :-)") || console.log("failed :-( ", data);
    });
}

function Test02() {
    console.log("1. (symbol2)... ")
    client.get("http://127.0.0.1:4000/rest/version1/symbol/_view", function (data, response) {
	console.log(data); // parsed response body as js object
	//console.log(response); // raw response
	    /^\[":ATTRIBUTES",.+\]$/.test(data) && console.log("ok :-)") || console.log("failed :-(");
    });
}

///////////////////////////
// Test cases:

console.log(colors.fg.green, '---- Test escad rest-api ----', colors.reset);
setTimeout(Test01, 0); // wait 0ms then execute
//setTimeout(Test02, 500); // wait 500ms then execute
//Test01();
//Test02();
