// Copyright (C) 2021, 2022 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// BUILD COMMAND:
// ./node_modules/.bin/esbuild --bundle escad-client.js --outfile=bundle.js
//
// TODO:
// use x-state (https://xstate.js.org/)
'use strict';

import cytoscape from 'cytoscape';
import contextMenus from 'cytoscape-context-menus';
//import Vue from 'vue';    // vue.runtime.esm.js
//import VueCytoscape from 'vue-cytoscape';
//Vue.use(VueCytoscape);


// register extension
cytoscape.use(contextMenus);

// import CSS as well
import 'cytoscape-context-menus/cytoscape-context-menus.css';
import './escad-client.css';

var escad = {};  // global entry to escad database
var cy = {};  // global entry to cytoscape object
var arango_authorization = "bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJwcmVmZXJyZWRfdXNlcm5hbWUiOiJyb290IiwiaXNzIjoiYXJhbmdvZGIiLCJpYXQiOjE2NDY1MzAxNDgsImV4cCI6MTY0OTEyMjE0OH0.-VX0eXUh0W8avClofFTiebdoVtIMIc6UmX2Invi6-zg";

function checkValidEscadName(name) {
    return name;
}

function displayRelation(id) {
    var rel = getRelation(id);
    if (typeof(rel) == 'object') {
	console.log("got: " + rel);
	document.getElementById('id').value = rel.id;
	document.getElementById('name').value = rel.name;
	document.getElementById('comment').value = rel.comment;
	document.getElementById('weight').value = rel.weight;
	document.getElementById('representation').value = rel.data;
	displayInfo("got relation data");
	[].forEach.call(document.getElementsByClassName('relation_input'), function (el) {
	    //console.log(el.title);
	    el.style.visibility = 'visible';
	});
    } else {
	displayError("error while displaying relation data (got no data)!");
    }
}

function displaySymbol(id) {
    var sym = getSymbol(id);
    if (typeof(sym) == 'object') {
	console.log("got: " + sym);
	document.getElementById('id').value = id;
	document.getElementById('name').value = sym.name;
	document.getElementById('comment').value = sym.comment;
	document.getElementById('weight').value = sym.weight;
	document.getElementById('representation').value = sym.representation;
	displayInfo("got symbol data");
	[].forEach.call(document.getElementsByClassName('symbol_input'), function (el) {
	    //console.log(el.title);
	    el.style.visibility = 'visible';
	});
    } else {
	displayError("error while displaying symbol data (got no data)!");
    }
}

function displayError(text) {
    document.getElementById('message').className = "error";
    document.getElementById('message').textContent = "😟" + text;
}

function displayInfo(text) {
    document.getElementById('message').className = "info";
    document.getElementById('message').textContent = "🙂" + text;
}

// IN: new or existing relation id.
// RETURN: relation-data-object or false otherwise
function getRelation(id) {
    return escad.relations.find(element => element.id == id);
}

// IN: new or existing symbol id.
// RETURN: symbol-data-object or false otherwise
function getSymbol(id) {
    return escad.symbols.find(element => element.id == id);
}

// IN: -.
// RETURN: array with taxonomy-objects or false if error
function getTaxonomy() {

}

// Connect to graph-database and read whole graph. In detail this means read all edges into variable escad.relations and all nodes into variable escad.symbols.
function getDBgraph() {
    fetch('http://127.0.0.1:8529/_db/thesis/_api/document/thesis_prozess/CAD',
	  { headers: { Authorization: arango_authorization }}).then(function (response) {
	// The API call was successful!
	if (response.ok) {
	    return response.json();
	} else {
	    return Promise.reject(response);
	}
    }).then(function (data) {
	// This is the JSON from our response
	console.log(data);
    }).catch(function (err) {
	// There was an error
	console.warn('Something went wrong.', err);
    });
    
    // simulates escad database acess
    escad = {relations: [{id: "r0", name: "R0", from: "bob", to: "jane"},
			     {id: "r1", name: "R1", from: "jane", to: "sue"}],
	     symbols: [{id: "bob", name: "Bob", x: 100, y: 280, comment: "no comment",
			representation: "DAta Bob", weight: 0},
		       {id: "jane", name: "Jane", x: 240, y: 300, comment: "no comment",
			representation: "representation Jane"},
		       {id: "sue", name: "Sue", x: 170, y: 200, comment: "no comment",
			representation: "representation Sue"}],
		 taxonomy: [{id: "has", comment: "Use for..."},
			    {id: "is_a", comment: "Use for 2..."}],
		 sym_counter: 0,
		 rel_counter: 0,
		 mode: ''
	    };
}

// IN: cytoscape-ref
// RETURN: true or false if no sucess.
function InitGraph(cy) {
    getDBgraph();

    escad.symbols.forEach(element => cy.add({group: 'nodes', data: { id: element.id, name: element.name, weight: 75 }, position: { x: element.x, y: element.y }}));
    escad.relations.forEach(element => cy.add({group: 'edges', data: { id: element.id, source: element.from, target: element.to, name: 'none'}}));
}

// RETURN: new id
function newSymbol(opts) {
    var id = opts.data.id === '' ? ("s" + escad.sym_counter++) : opts.data.id;
    var pos = opts.position === undefined ? {x: 0, y: 0} : opts.position;

    // create entry in escad:
    escad.symbols.push({id: id});

    // create entry in cytoscape:
    cy.add({ group: 'nodes', data: {id: id}, position: pos});

    setSymbol(opts);  // set remaining options
}

function saveObject() {
    if (escad.mode === 'symbol') {
	var id = document.getElementById('id').value;
	var name = document.getElementById('name').value;
	var comment = document.getElementById('comment').value;
	var weight = document.getElementById('weight').value;
	var representation = document.getElementById('representation').value;
	setSymbol({ data: {id: id, name: name, weight: weight}, group: 'nodes',
		    position: {x: cy.$('#' + id).position('x'),
			       y: cy.$('#' + id).position('y')}});
    }
}

function setMode(object) {
    if (object === 'symbol') {
	escad.mode = 'symbol'
	document.getElementById('taxonomy').className = "symbol_input";
	document.getElementById('id').className = "symbol_input";
	document.getElementById('name').className = "symbol_input";
	document.getElementById('comment').className = "symbol_input";
    } else {
	escad.mode = 'relation'
	document.getElementById('taxonomy').className = "relation_input";
	document.getElementById('id').className = "relation_input";
	document.getElementById('name').className = "relation_input";
	document.getElementById('comment').className = "relation_input";
    }
}

// IN: new or existing relation id, relation-object-data.
// RETURN: id if sucess, false otherwise
function setRelation(id, data) {

}

// set (new) symbol
// IN: cytoscape-node-object (like in cy.add()) with things to change/update.
// RETURN: id if sucess, false otherwise
function setSymbol(opt_s = { group: "nodes", position: {x: 0, y: 0} }) {
    var opts = opt_s;  // set default values if not given all
    
    if (opts.data.id === '') {  // new symbol
	opts.data.id = ("s" + escad.sym_counter++);
	// create entry in escad:
	escad.symbols.push({id: id});
    }
    
    let index = escad.symbols.findIndex(element => element.id == opts.data.id); // is element already there?

    if (index == -1) { // symbol not found
	displayError("symbol with id [" + opts.data.id + "] not in database!");
    } else {
	displayInfo("setting sym[" + index + "]...");
	// update escad:
	if (typeof opts.position !== 'undefined') {
	    //displayError("[dbg]" + opts.position.x + "|" + index + "|" + escad.symbols[index]);
	    escad.symbols[index].x = opts.position.x;
	    escad.symbols[index].y = opts.position.y;
	}
	if (typeof opts.data.name !== 'undefined') {
	    escad.symbols[index].name = opts.data.name;
	}
	//escad.symbols.splice(index, 1, symobj);

	// update cytoscape:
	var id = "#" + opts.id;
	cy.nodes(id).json(opts);
    }
}



// MAIN:
document.addEventListener('DOMContentLoaded', function(){
    cy = window.cy = cytoscape({
    container: document.getElementById('cy'),
    
    layout: {
        name: 'concentric',
        concentric: function(n){ return n.id() === 'j' ? 200 : 0; },
        levelWidth: function(nodes){ return 100; },
        minNodeSpacing: 100
    },

    style: [
        {
	    selector: 'node[name]',
	    style: {
		'background-color': 'grey',
                'content': 'data(name)'
	    }
        },
	
        {
	    selector: 'edge',
	    style: {
		'width': 3,
		'line-color': 'green',
                'curve-style': 'bezier',
                'target-arrow-shape': 'triangle',
		'target-arrow-color': 'green',
		'source-arrow-color': 'yellow'
            }
        },
	{
	    selector: ':selected',
	    css: {
		'background-color': 'SteelBlue',
		'line-color': 'SteelBlue',
		'target-arrow-color': 'SteelBlue',
		'source-arrow-color': 'SteelBlue'
	    }
	},
    ],

    });

    var contextMenu = cy.contextMenus({
	menuItems: [
        {
	    id: 'remove',
	    content: '➖ remove',
	    tooltipText: 'remove symbol or relation',
	    selector: 'node, edge',
	    onClickFunction: function (event) {
                var target = event.target || event.cyTarget;
                var removed = target.remove();
		displayInfo("removed " + removed);
                //contextMenu.showMenuItem('undo-last-remove');
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'add-relation',
	    content: '➕ add relation',
	    tooltipText: 'add relation from this symbol to the next which you now select',
	    selector: 'node',
	    onClickFunction: function (event) {
		var selectedNodes = cy.$('node:selected');
		if (selectedNodes.length === 2) {
		    cy.add([ { group: 'edges', data: { id: 'e0', source: selectedNodes[0].json().data.id, target: selectedNodes[1].json().data.id } } ]); } else {
			displayError("two symbols need to be selected! (you did " + selectedNodes.length + ")");
		    }
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'activate',
	    content: '⚙ activate symbol',
	    tooltipText: 'activate this symbol run function eventually accocciated with an expansion',
	    selector: 'node',
	    onClickFunction: function (event) {
		var selectedNodes = cy.$('node:selected');
		//activateSymbol();
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'add-node',
	    content: '➕ add symbol',
	    tooltipText: 'add symbol',
	    coreAsWell: true,
	    onClickFunction: function (event) {
                var pos = event.position; // || event.cyPosition;
		newSymbol({ data: {id: ''}, group: 'nodes', position: {x: pos.x, y: pos.y}});
	    }
        }
    ]
    });


    // handle events
    cy.on('tap', 'node', function(evt){
	var node = evt.target;
	setMode('symbol');
	console.log( 'tapped node ' + node.id() );
	//console.log(cy.$('node:selected'));
	displaySymbol(node.id());
	//[].forEach.call(document.querySelectorAll('.symbol_input'), function (el) {  el.style.visibility = 'visible';	});  // exactly ALL search items must be exist in order to get selected!
    });

    cy.on('tap', 'edge', function(evt){
	var edge = evt.target;
	setMode('relation');
	console.log( 'tapped edge ' + edge.id() );
	//console.log(cy.$('node:selected'));
	displayRelation(edge.id());
    });

    document.getElementById("save").addEventListener("click", saveObject, false);
    InitGraph(cy);
});
