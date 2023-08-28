// Copyright (C) 2021, 2022, 2023 Markus Kollmar (email: markuskollmar@onlinehome.de)
//
// BUILD COMMAND:
// ./node_modules/.bin/esbuild --bundle escad-client.js --outfile=bundle.js
//
'use strict';

import cytoscape from 'cytoscape';
import contextMenus from 'cytoscape-context-menus';
cytoscape.use(contextMenus); // register extension
import { createMachine, interpret, assign } from 'xstate'; // https://xstate.js.org/

// Actions for state-machine:
const pushNodeId = assign({ last_selected_two_nodes: (context, event) => {
    console.log("node pushed:" + event.id);
    if (context.last_selected_two_nodes.push(event.id) > 2) {
	context.last_selected_two_nodes.shift(); }
    return context.last_selected_two_nodes; } });

// user interfache
const wmui = createMachine(
    {
	id: 'wmui',
	initial: 'default',
	preserveActionOrder: true,
	context: {
	    query_running: false,
	    show_query: true,
	    last_selected_two_nodes: []
	},
	states: {
	    default: {
		on: {
		    NEWNODE: { target: 'node', actions: [ 'newNode' ] },
		    TOGGLE_QUERY: { target: 'default', actions: [ 'toggleShowQuery', 'showQuery' ] },
		    EDGE: { target: 'edge' },
		    NODE: { target: 'node', actions: [ 'pushNodeId' ] }
		}
	    },
	    query: {
		on: {
		    DEFAULT: { target: 'default' },
		    EDGE: { target: 'edge' },
		    NODE: { target: 'node', actions: [ 'pushNodeId' ]},
		    INSERT: { target: 'query', actions: [ 'insertNodeFromQuery' ] }
		}
	    },
	    edge: {
		entry: 'displayEdge',
		on: {
		    DEFAULT: { target: 'default' },
		    SAVE: { target: 'edge', actions: [ 'saveEdge' ] },
		    NODE: { target: 'node', actions: [ 'pushNodeId' ] },
		    REMOVE_EDGE: { target: 'default', actions: [removeEdge] },
		    EDGE: { target: 'edge'}
		}
	    },
	    node: {
		entry: 'displayNode',
		on: {
		    DEFAULT: { target: 'default' },
		    SAVE: { target: 'default', actions: [ 'saveNode' ] },
		    EDGE: { target: 'edge' },
		    NEWNODE: { target: 'node', actions: [ 'newInstance' ] },
		    NODE: { target: 'node', actions: [ 'pushNodeId' ] },
		    REMOVE_NODE: { target: 'default', actions: [ 'removeNode' ] },
		    NEWEDGE: { target: 'edge', actions: [ 'newEdge' ] },
		    QUERY_ELEMENT: { target: 'query', actions: [ 'listObjectNodes' ] },
		    QUERY_GRAPH: { target: 'query', actions: [ 'doQuery' ] },
		    SHOW_TREE: { target: 'node', actions: [ 'showTree' ] }
		}
	    }
	}
    },
    {
	actions: {
	    displayEdge: (context, event) => { displayEdge(event.id); },
	    displayNode: (context, event) => { displayNode(event.id); },
	    listObjectNodes: (context, event) => { listObjectNodes(event.id); },
	    toggleShowQuery: assign({ show_query: (context, event) => !context.show_query }),
	    showQuery: (context, event) => { if (context.show_query==true) { showQuery(); }
					     else { hideQuery(); } },
	    doQuery: (context, event) => { assign({ query_running: true });
					   doSearch(event.id);
					   assign({ query_running: false }); },
	    pushNodeId,
	    removeNode: (context, event) => { removeNode(event.id); },
	    removeEdge: (context, event) => { removeEdge(document.getElementById('id').value); },
	    saveNode: (context, event) => { saveNode(context.last_selected_two_nodes[1]); },
	    saveEdge: (context, event) => { saveEdge(document.getElementById('id').value); },
	    showTree: (context, event) => { showTree(event.id); },
	    insertNodeFromQuery: (context, event) => { insertNodeFromQuery(); },
	    newInstance: (context, event) => { newNode(event.position.x, event.position.y, event.semantic); },
	    newNode: (context, event) => { newNode(event.position.x, event.position.y, "unknown"); },
	    newEdge: (context, event) => { newEdge(context.last_selected_two_nodes[0], context.last_selected_two_nodes[1]); }
	},
	guards: {
	    queryIsFinished: (context, event) => { return context.query_running == false; }
	}
    });

const wmuiService = interpret(wmui).onTransition( (state) => console.log(state.value) );

var cy = {};  // global entry to cytoscape object
var authorization = ""; // currently not used

// execute this whenever the state changes:
wmuiService.subscribe( (state) => {
    if (state.matches("node")) {
	document.getElementById('id').style.display = "inline";
	document.getElementById('id').setAttribute("list", "node-type-list");
	document.getElementById('semantic').style.display = "inline";
	document.getElementById('info').style.display = "inline";
	document.getElementById('weight').style.display = "inline";
	document.getElementById('time').style.display = "none";
	document.getElementById('type').style.display = "inline";
	document.getElementById('save').style.display = "inline";
	document.getElementById('insert').style.display = "none";
	document.getElementById('query_result').style.display = "none";
	document.getElementById('toggle_query').style.display = "none";
	//[].forEach.call(document.getElementsByClassName('node'), function (el) { el.style.visibility = 'visible'; });
    } else if (state.matches("edge")) {
	document.getElementById('id').style.display = "inline";
	document.getElementById('id').setAttribute("list", "edge-type-list");
	document.getElementById('semantic').style.display = "inline";
	document.getElementById('info').style.display = "none";
	document.getElementById('weight').style.display = "inline";
	document.getElementById('time').style.display = "inline";
	document.getElementById('type').style.display = "inline";
	document.getElementById('save').style.display = "inline";
	document.getElementById('insert').style.display = "none";
	document.getElementById('query_result').style.display = "none";
	document.getElementById('toggle_query').style.display = "none";
    } else if (state.matches("default")) {
	displayInfo("Click on symbol or relation in order to see more information about it.<br />Click on background in order to get to this home-view.<br />Right-click on objects opens context dependent menue with further actions.<br />Create a search by connecting or creating symbols and relation.<br />To reset input reload this page.");
	document.getElementById('id').style.display = "none";
	document.getElementById('semantic').style.display = "none";
	document.getElementById('info').style.display = "none";
	document.getElementById('weight').style.display = "none";
	document.getElementById('time').style.display = "none";
	document.getElementById('type').style.display = "none";
	document.getElementById('save').style.display = "none";
	document.getElementById('insert').style.display = "none";
	document.getElementById('query_result').style.display = "none";
	document.getElementById('toggle_query').style.display = "inline";
    } else if (state.matches("query")) {
	displayInfo("In der Auswahlliste links erscheinen die Suchergebnisse.<br />Klicke auf ein Ergebnis um Informationen dazu zu bekommen.<br />Um das Ergebnis in den Suchgraph einzusetzen klicke <Knoten einfügen>.");
	document.getElementById('id').style.display = "none";
	document.getElementById('semantic').style.display = "none";
	document.getElementById('info').style.display = "none";
	document.getElementById('weight').style.display = "none";
	document.getElementById('time').style.display = "none";
	document.getElementById('type').style.display = "none";
	document.getElementById('save').style.display = "none";
	document.getElementById('insert').style.display = "inline";
	document.getElementById('query_result').style.display = "inline";
	document.getElementById('toggle_query').style.display = "none";
    }
} );


// display edge data in ui (no graphic):
async function displayEdge(id) {
    //const dbe = await getDBEdge(id);
    //cy.$id(id).json( {data: {semantic: dbe.semantic, weight: dbe.weight, time: dbe.time, type: dbe.type} } );

    if (cy.$id(id).data('atdb') != true) {
	displayInfo("Relation is not stored in escad yet (click <Object save> to store)!<br />Relation info:");
    }
    
    console.log("got id: " + id);
    document.getElementById('id').value = id;
    document.getElementById('semantic').value = cy.$id(id).data("semantic");
    document.getElementById('time').value = cy.$id(id).data("time");
    document.getElementById('type').value = cy.$id(id).data("type");
    document.getElementById('weight').value = cy.$id(id).data("weight");
}

// display node data in ui (no graphic):
async function displayNode(id) {
    const db_key = cy.$id(id).data('_key');
    //const dbn = await getDBNode(db_key);
    //cy.$id(id).json( {data: {semantic: dbn.semantic, weight: dbn.weight, info: dbn.info, type: dbn.type}, position: {x: dbn.x, y: dbn.y} } );

    if (cy.$id(id).data('atdb') != true) {
	displayInfo("Symbol is not stored in escad yet (click <Object save> to store)!<br />Symbol info:");
    }

    console.log("got id: " + id);
    document.getElementById('id').value = db_key;
    document.getElementById('semantic').value = cy.$id(id).data("semantic");
    document.getElementById('type').value = cy.$id(id).data("type");
    document.getElementById('weight').value = cy.$id(id).data("weight");
    document.getElementById('info').value = cy.$id(id).data("info");
}

// display error message in ui:
function displayError(text) {
    document.getElementById('message').textContent = "😟";
    document.getElementById('output').className = "error";
    document.getElementById('output').innerHTML = text;
}

// display info message in ui (you can also use html):
function displayInfo(text) {
    document.getElementById('message').textContent = "🙂";
    document.getElementById('output').className = "info";
    document.getElementById('output').innerHTML = text;
}

// show query edges
function hideQuery() {
    cy.elements('edge[type = "query"]').addClass("hidden");
    console.log("hide edge(s): " + cy.elements('edge[type = "query"]') );
}

// show query edges
function showQuery() {
    cy.elements('edge[type = "query"]').removeClass("hidden");
    console.log("show edge: " + cy.elements('edge[type = "query"]') );
}


//  RFC 3986 conformance:
function doURLencode(string) {
    return encodeURIComponent(string).replace(/[!'()*]/g, function(c) {
	return '%' + c.charCodeAt(0).toString(16);  });
}


// DB SEARCH/TREE
async function getDBsearch(target_node_id) {
    const id = doURLencode(target_node_id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/search_equal_semantic/' + id,  { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
	    displayError("Suchproblem...");
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function getDBTree(root_node_id) {
    console.log('Get tree ' + root_node_id);
    const id = doURLencode(root_node_id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/tree/' + id,  { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
	    displayError("Suchproblem...");
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


// DB GET NODE/EDGE
async function getDBClassNodes() {
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/class_nodes',
			      { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function getDBObjectNodes(semantic) {
    const semantic_enc = doURLencode(semantic);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/object_nodes/' + semantic_enc,
			      { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function getDBEdge(id) {
    const edge_id = doURLencode(id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/edge/' + edge_id,
			      { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function getDBNode(id) {
    const node_id = doURLencode(id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/node/' + node_id,
			      { headers: { Authorization: authorization }});
	if (response.status === 200) {
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


// DB SAVE EDGE/NODE
async function saveDBEdge(edge) {
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/set_edges',
				   { method: 'POST',
				     headers: { Authorization: authorization },
				     body: JSON.stringify(edge)});
	if (response.status === 200) {
	    displayInfo("Erfolgreich in Graphdatenbankserver gespeichert.");
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function saveDBNode(node) {
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/set_nodes',
				   { method: 'POST',
				     headers: { Authorization: authorization,
						'Accept': 'application/json',
						'Content-Type': 'application/json'},
				     body: JSON.stringify(node)});
	if (response.status === 200) {
	    displayInfo("Erfolgreich in Graphdatenbankserver gespeichert.");
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


// DB REMOVE EDGE/NODE
async function removeDBEdge(edge_id) {
    const id = doURLencode(edge_id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/edge/' + id,
				   { method: 'DELETE',
				     headers: { Authorization: authorization,
						'Accept': 'application/json',
						'Content-Type': 'application/json'}});
	if (response.status === 200) {
	    displayInfo("Successful removed relation in escad.");
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}


async function removeDBNode(node_id) {
    const id = doURLencode(node_id);
    try {
        let response = await fetch('http://127.0.0.1:8529/_db/thesis/thesis_api/node/' + id,
				   { method: 'DELETE',
				     headers: { Authorization: authorization,
						'Accept': 'application/json',
						'Content-Type': 'application/json'}});
	if (response.status === 200) {
	    displayInfo("Erfolgreich in Graphdatenbankserver gelöscht.");
            return await response.json();
	} else {
            return Promise.reject(response);
	}
    } catch (error) {
	displayError(error);
        console.log(error);
    }
    return false;
}



// CLIENT EDGE/NODE OPERATIONS
async function initGraph(cy) {
    let nodes = await getDBClassNodes();

    let nr = Date.now();
    nodes.forEach( (n) => { nr = nr +1; cy.add({group: 'nodes', data: { id: nr.toString(), info: n.info, _key: n._key, semantic: n.semantic, type: n.type, weight: n.weight, atdb: true }, position: { x: n.x, y: n.y }} ); } );
}


async function listObjectNodes(node_id) {
    let semantic = cy.$id(node_id).data('semantic');
    let nodes = await getDBObjectNodes(semantic);

    document.getElementById('query_result').innerHTML = "";
    nodes.forEach(function(item){
	document.getElementById('query_result').innerHTML += "<option value=\"" + item._key + "\">" + item._key + "</option>";
    });
    const db_key = cy.$id(node_id).data('_key');
    displayInfo("Folgende Knoteninstanzen für Knoten nach der Semantik von [" + db_key + "] wurden gefunden:");
}


async function doSearch(node_id) {
    const db_key = cy.$id(node_id).data('_key');
    let nn = await getDBsearch(db_key);
    let nodes = nn[0];
    
    document.getElementById('query_result').innerHTML = "";
    nodes.forEach(function(item){
	console.log("index:" + item.index);
	document.getElementById('query_result').innerHTML += "<option value=\"" + item.ll + "\">" + item.ll + " (" + item.index + ")</option>";
    });
    displayInfo("Folgende Knotenobjekte der Klasse [" + db_key + "] wurden gefunden:");
}


async function showTree(node_id) {
    const db_key = cy.$id(node_id).data('_key');
    let ne = await getDBTree(db_key);
    ne.forEach( function(o) {
	console.log("tree nobj: " + o.nodes);
	console.log("tree eobj: " + o.edges);
	doNodesUpdate(o.nodes);
	doEdgesUpdate(o.edges); } );
    displayInfo("Wurzelbaum von Knoten [" + db_key + "] wird angezeigt.");
}


function doNodesUpdate(nodes) {
    if (typeof nodes !== 'undefined') {
	const multiple = Array.isArray(nodes);
	const inputarray = multiple ? nodes : [nodes];

	let nr = Date.now();
	inputarray.forEach( function(db_key) {
	    const nid = cy.elements('node[_key = \"' + db_key + '\"]').data("id");
	    if( cy.$id(nid).inside() != true ) {
		getDBNode(db_key).then(n => {
		    if (n.type != 'class') {  // do not create a class node, because it is already there
			cy.add( {group: 'nodes',
				 data: { id: nr.toString(),
					 info: n.info,
					 _key: n._key,
					 semantic: n.semantic,
					 type: n.type,
					 weight: n.weight,
					 atdb: true },
				 position: {x: n.x + 10, y: n.y + 10} } ); }} );
		nr = nr +1;
		//cy.nodes(_key = db_key]).json(opts);
	    } } );
    } else { console.log("undefined nodes for tree"); }
}


// visualize the given edge(s) in the client if not already present
function doEdgesUpdate(edges) {
    if (typeof edges != 'undefined') {
	const multiple = Array.isArray(edges);
	const inputarray = multiple ? edges : [edges];

	inputarray.forEach( function(db_key) {
	    if( cy.$id(db_key).inside() != true ) {
		console.log("db_key edge update: " + db_key);
		getDBEdge(db_key).then(e => {
		    const db_edge = e[0];
		    cy.add({ group: 'edges',
			     data: {id: db_key,
				    source: cy.elements('node[_key = \"' + db_edge._from + '\"]').data("id"),
				    target: cy.elements('node[_key = \"' + db_edge._to + '\"]').data("id"),
				    semantic: db_edge.semantic,
				    time: Number(db_edge.time),
				    type: db_edge.type,
				    weight: Number(db_edge.weight),
				    atdb: true } } );
		} );
	    } } );
    } else { console.log("undefined edges for tree"); }
}


function newEdge(node1, node2) {
    if (typeof node1 === 'undefined' || typeof node1 === 'undefined') {
	displayError("Konnte neue Kante nicht anlegen, da Start oder Ziel fehlt!");
	return false;
    }
    const db_key1 = cy.$id(node1).data('_key');
    const db_key2 = cy.$id(node2).data('_key');
    console.log("create new edge: [" + db_key1 + "]->[" + db_key2 + "]");
    const name = Date.now().toString();
    cy.add({ group: 'edges', data: {id: name, source: node1, target: node2, _from: db_key1, _to: db_key2, semantic: "relates", time: 0, type: "query", weight: 0, atdb: false } } );
    displayInfo("Neue Kante [" + name + "] erstellt.");
}


async function saveEdge(edge_id) {
    const _from = cy.$id(edge_id).source().data('_key');
    const _to = cy.$id(edge_id).target().data('_key');

    var edge = { "_key": edge_id,
		 "_from": _from,
		 "_to": _to,
		 "type": document.getElementById('type').value,
		 "semantic": document.getElementById("semantic").value,
		 "weight": Number(document.getElementById('weight').value),
		 "time": Number(document.getElementById('time').value) };
    
    saveDBEdge(edge).then(edge_data => {
	cy.getElementById(edge_id).json({data: { source: edge_data._from, target: edge_data._to, semantic: edge_data.semantic, type: edge_data.type, weight: edge_data.weight, time: edge_data.time, atdb: true }});
	displayInfo("Kante [" + edge_id + "] gespeichert."); 
	console.log(edge_data);
	return edge_data;
    }, reason => {
	displayError("Konnte Kante nicht speichern!");
	console.error(reason);
	return false;
    });
}


function removeEdge(edge_id) {
    if (cy.$id(edge_id).data('atdb')) {
	removeDBEdge(edge_id).then(element => {
	    displayInfo("Kante [" + edge_id + "] auf DB gelöscht."); 
	}, reason => {
	    displayError("Konnte Kante nicht löschen!");
	    console.error(reason);
	    return false;
	});
    }
    cy.getElementById(edge_id).remove();
}



function insertNodeFromQuery() {
    var node_id = document.getElementById("query_result").value;
    //var qr = document.getElementById("query_result");
    //var node_id = qr.options[qr.selectedIndex].text;
    
    getDBNode(node_id).then(n => {
	cy.add({group: 'nodes', data: { id: Date.now().toString(), info: n.info, _key: n._key, semantic: n.semantic, type: n.type, weight: n.weight, atdb: true }, position: { x: n.x + 10, y: n.y + 10 }});
	displayInfo("Knoten <" + node_id + "> hinzugefuegt"); 
    });
}


function newNode(pos_x, pos_y, semantic) {
    const nid = Date.now().toString();
    const db_key = "NeuerKnoten";

    cy.add({group: 'nodes', data: { id: nid, info: "TODO", _key: db_key, semantic: semantic, type: "object", weight: 0, atdb: false }, position: { x: pos_x, y: pos_y }});

    displayInfo("Neuer Knoten [" + nid + "] erzeugt (die id kann geändert werden)."); 
}


function saveNode(node_id) {
    //var db_key = cy.$id(node_id).data('_key');
    //var db_key = cy.elements('node[_key = \"' + db_key + '\"]').data("id");
    var db_key = document.getElementById('id').value;
    var s = document.getElementById('semantic').value;
    var w = Number(document.getElementById('weight').value);
    var t = document.getElementById('type').value;
    var i = document.getElementById('info').value;
    var opts = { data: { _key: db_key, semantic: s, weight: w, info: i, type: t, atdb: true},
		 group: 'nodes',
		 position: {x: cy.$id(node_id).position('x'), y: cy.$id(node_id).position('y')}};
    var node = { _key: db_key,
		 type: t,
		 semantic: s,
		 info: i,
		 weight: w,
		 x: cy.$id(node_id).position('x'),
		 y: cy.$id(node_id).position('y') };

    saveDBNode(node).then(data => {
	cy.nodes('#' + node_id).json(opts);
	displayInfo("Knoten [" + db_key + "] gespeichert."); 
	console.log(data);
    }, reason => {
	displayError("Konnte Knoten nicht speichern!");
	console.error(reason);
	return false;
    });
}


function removeNode(node_id) {
    if (cy.$id(node_id).data('atdb') == true) {
	const db_key = cy.$id(node_id).data('_key');
	removeDBNode(db_key).then(element => {
	    displayInfo("Knoten [" + db_key + "] gelöscht."); 
	}, reason => {
	    displayError("Fehler beim Löschen des Knoten in der Datenbank!");
	    console.error(reason);
	});
    }
    cy.remove( cy.$id(node_id) );
}



// ***********************************************
// MAIN:
document.addEventListener('DOMContentLoaded', function(){
//window.onload = function() {

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
	    selector: 'node[id]',
	    style: {
		'background-color': 'grey',
                'content': 'data(_key)'
	    }
        },
	{
	    selector: '.hidden',
	    css: {
		'display': 'none'
	    }
	},
        {
	    selector: 'edge',
	    style: {
		'width': 3,
		'line-color': 'green',
                'curve-style': 'straight',
                'target-arrow-shape': 'triangle',
		'target-arrow-color': 'green',
		'source-arrow-color': 'yellow',
		'label': 'data(time)'
		//'color': 'orange'
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
    ]

    });

    var contextMenu = cy.contextMenus({
	menuItems: [
	{
	    id: 'remove_edge',
	    content: '➖ Kante loeschen',
	    tooltipText: 'Kante loeschen',
	    selector: 'edge',
	    onClickFunction: function (event) {
                var target = event.target; // || event.cyTarget;
                //var removed = target.remove();
		//displayInfo("removed " + removed);
		console.log("menue selected node: [" + event.target.id() + "]");
		wmuiService.send({ type: 'REMOVE_EDGE', id: event.target.id() });
                //contextMenu.showMenuItem('undo-last-remove');
	    },
	    hasTrailingDivider: true
        },
  	{
	    id: 'remove_node',
	    content: '➖ Knoten loeschen',
	    tooltipText: 'Knoten loeschen',
	    selector: 'node',
	    onClickFunction: function (event) {
                //var target = event.target || event.cyTarget;
                //var removed = target.remove();
		//displayInfo("removed " + removed);
		wmuiService.send({ type: 'REMOVE_NODE', id: event.target.id() });
                //contextMenu.showMenuItem('undo-last-remove');
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'add_instance',
	    content: '➕ Knoteninstanz erzeugen',
	    tooltipText: 'Knoteninstanz erzeugen',
	    selector: 'node',
	    onClickFunction: function (event) {
		var pos = event.position; // || event.cyPosition;
		wmuiService.send({ type: 'NEWNODE', semantic: cy.$id(event.target.id()).data('semantic'), position: {x: pos.x + 10, y: pos.y + 10} });
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'add-edge',
	    content: '➕ Kante hinzu',
	    tooltipText: 'Kante zu diesem Knoten oder zu einem anderen hinzufuegen.',
	    selector: 'node',
	    onClickFunction: function (event) {
		var selectedNodes = cy.$('node:selected');
		if (selectedNodes.length === 2) {
		    wmuiService.send( { type: 'NEWEDGE', source: selectedNodes[1].id(), target: selectedNodes[0].json().data.id } );
		} else if (selectedNodes.length === 1) {
		    wmuiService.send( { type: 'NEWEDGE', source: selectedNodes[0].id(), target: selectedNodes[0].json().data.id } );
		} else {
		    displayError("Ein oder zwei Knoten muessen ausgewaehlte sein! (derzeit sind es " + selectedNodes.length + ")");
		}
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'search-this',
	    //content: '⚙ element search',
	    content: '🔎 Suche Instanz(en)',
	    tooltipText: 'Suche alle Knotenobjekte dieser Klasse.',
	    selector: 'node',
	    onClickFunction: function (event) {
		//var selectedNodes = cy.$('node:selected');
		//var pos = event.position; // || event.cyPosition;
		wmuiService.send({ type: 'QUERY_ELEMENT', id: event.target.id() });
		displayInfo("Suche " + event.target.id() + ".");
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'search-graph',
	    content: '🔎 Suche Graph(en)',
	    tooltipText: 'Suche Knoten dieser Klasse die der gegebenen Graphstruktur folgen.',
	    selector: 'node',
	    onClickFunction: function (event) {
		//var selectedNodes = cy.$('node:selected');
		wmuiService.send({ type: 'QUERY_GRAPH', id: event.target.id() });
		displayInfo("Suche " + event.target.id() + ".");
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'show-tree',
	    content: '↑ Zeige Baum',
	    tooltipText: 'Zeige Baum gleicher Zeit.',
	    selector: 'node',
	    onClickFunction: function (event) {
		wmuiService.send({ type: 'SHOW_TREE', id: event.target.id() });
	    },
	    hasTrailingDivider: true
        },
	{
	    id: 'add-node',
	    content: '➕ Knoten hinzu',
	    tooltipText: 'Knoten hinzufuegen',
	    coreAsWell: true,
	    onClickFunction: function (event) {
                var pos = event.position; // || event.cyPosition;
		wmuiService.send({ type: 'NEWNODE', position: {x: pos.x, y: pos.y}  });
	    }
        }
    ]
    });


    // handle events
    cy.on('tap', 'node', function(evt){
	wmuiService.send({ type: 'NODE', id: evt.target.id() });
	const db_key = cy.$id( evt.target.id() ).data('_key');
	console.log('tap node: ' + db_key);
    });

    cy.on('tap', 'edge', function(evt){
	wmuiService.send({ type: 'EDGE', id: evt.target.id() });
	console.log('tap edge: ' + evt.target.id());
    });

    cy.on('tap', function(event){
	var evtTarget = event.target; // reference to the originator of the event (core or element)
	if( evtTarget === cy ){
	    wmuiService.send({ type: 'DEFAULT' });
	    console.log('tap on background');
	} else {
	    console.log('tap on some element');
	}
    });

    document.getElementById("save").addEventListener('click', function(e) { wmuiService.send({ type: 'SAVE' }); }, false);
    document.getElementById("insert").addEventListener('click', function(e) { wmuiService.send({ type: 'INSERT' }); }, false);
    document.getElementById("toggle_query").addEventListener('click', function(e) { wmuiService.send({ type: 'TOGGLE_QUERY' }); }, false);
    document.getElementById("id").addEventListener('change', function(e) { console.log( 'changed id'); }, false);
    initGraph(cy);
    wmuiService.start(); // Start the xstate-service
});
