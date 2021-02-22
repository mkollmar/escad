// Copyright (C) 2021 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// This are tests for the escad-lisp-API.
//
// USAGE:
// 1. Start escad with "./escad start net-lisp"
// 2. Call this via "node lisp_api.js"
// ###################################################


// Socket-connection (via common-lisp data) to ESCAD:
var net = require('net');
var escad = new net.Socket();  // bidirectional TCP-stream to write/read to escad

//var endOfLine = require('os').EOL;
var escad_result_string = "";  // incoming escad data

// [test_name1, send_test_cmd1, regex_correct_result1, test_name2, send_test_cmd2, regex_correct_result2,...]
var tests = [
    'symbol', '(o (s "_view"))', /^\(:ATTRIBUTES/,
    'symbol2', '(o (s "_view"))', /^\(:ATTRIBUTES/,
];
var testp = 0; // pointer in 'tests' array

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

// ********************************************************************
// COMMUNICATION WITH ESCAD
escad.connect(3000, '127.0.0.1', function() {
    console.log('Connecting to escad...');
});

// catch a possible not connected error:
process.on('uncaughtException', function (err) {
    console.log(colors.fg.red, 'Hey, have you forgotten to start escad via "./escad start net-lisp"? Following error occured:', colors.reset);
    console.log('<<', err, '>>');
});

// Listen to 'data' event, triggered when data from escad is at socket:
escad.on('data', function(data) {
    //console.log('raw escad-answer: ' + JSON.stringify(data.toString().trim()));

    if (Buffer.isBuffer(data)) {
	//escad_result_string = JSON.stringify(data.toString().trim());
	escad_result_string = data.toString().trim();
    } else { // we got string, no buffer
	escad_result_string = data.toString();
    }

    if (escad_result_string) {  // avoid errors if there is a empty string
	if (escad_result_string.match("\\w+")) {  // emit 'gotEscadData' event if we got at least one word
	    escad.emit('gotEscadData', escad_result_string);
	}
    }
});

function MakeNextTest() {
    process.stdout.write(testp/3 + ". (" + tests[testp] + ")... ");
    testp++;
    escad.write(tests[testp]);
}

function CheckTest(result) {
    testp++;
    //var my_regex = new RegExp(tests[testp]);
    //var my_regex = tests[testp];
    if (tests[testp].test(result) == true) {
	console.log(colors.fg.green, 'ok', colors.reset);
    }
    else {
	console.log(colors.fg.red, 'failed: <<', result, '>>', colors.reset);
    }
    testp++;
}

// register to 'gotEscadData' event to checkt test result:
escad.on('gotEscadData', function(data) {
    CheckTest(data);
    if (tests.length > (testp+2)) {  // are there any tests to do yet?
	MakeNextTest();
	//setTimeout(MakeNextTest, 500); // wait 500ms then execute
    } else {
	escad.destroy();
    }
});


// Listen to 'close' event, which comes when escad closes socket:
escad.on('close', function(data) {
    console.log('escad closed socket!');
});


// Listen to 'connect' event:
escad.on('connect', function(data) {
    //console.log('Connected to escad!');

    // MAIN: start test-chain procedure after connected to escad:
    console.log(colors.fg.green, '---- Test escad lisp-api ----', colors.reset);
    MakeNextTest();
});
