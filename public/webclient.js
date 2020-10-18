// Copyright (C) 2011, 2012, 2013, 2014, 2015, 2019, 2020 Markus Kollmar (email: markuskollmar@onlinehome.de)

'use strict';

var escad = angular.module('escad', ['ngGrid', 'ui.bootstrap']);

// Symbol object constructor:
function EscadObj(name, attributes, comment, ref_to, ref_from, taxonomy, weight) {
    this.name = name;
    this.attributes = attributes;
    this.comment = comment;
    this.taxonomy = taxonomy;
    this.ref_to = ref_to;
    this.ref_from = ref_from;
    this.weight = weight;
}


// Controller for html document:
escad.controller("mainController", ['$scope', '$http', '$timeout', function($scope, $http, $timeout) {
    // on first load always execute:
    //x3dom.reload;
    $scope.Escad = {cmd_result:'loaded!'}; // make object to prevent trouble with javascript-inheritation
    $scope.Escad.file_input = 'view/language_training.svg';
    $scope.Escad.view_file = "view/language_training.svg";
    $scope.Escad.svg_url = ['view/escad_view.svg'];
    $scope.Escad.relations = [];
    $scope.Escad.symbols = [];
    $scope.Escad.objects = [];
    $scope.Escad.myObjectSelections = []; // selected escad object name
    $scope.Escad.charSet = 'UTF8';
    $scope.Escad.changesMade = 'no changes';
    $scope.Escad.cmd = '(ls)';
    $scope.Escad.message = 'Client loaded :-)';
    $scope.Escad.states = ['person', 'thing', 'country'];
    $scope.Escad.objectGrid = { data: 'Escad.objects',
			  selectedItems: $scope.Escad.myObjectSelections,
			  multiSelect: false,
			  enableColumnResize: true,
			  //pagingOptions: {
			    // pageSizes: list of available page sizes.
			    //pageSizes: [250, 500, 1000], 
			    //pageSize: currently selected page size. 
			    //pageSize: 250,
			    //totalServerItems: Total items are on the server. 
			    //totalServerItems: 0,
			    //currentPage: the uhm... current page.
			    //currentPage: 1
			  //},
			  enablePaging: true,
			  enableRowReordering: false,
			  showGroupPanel: false,
			  showColumnMenu: true,
			  afterSelectionChange: function() {
			      $scope.Escad.changesMade = "unsaved changes!";
			  },
			  showFilter: true,
			  columnDefs: [
			      {field: 'name', displayName: 'name', enableCellEdit: true},
			      {field: 'attributes', displayName: 'attributes', enableCellEdit: true},
			      {field: 'ref_to', displayName: 'ref_to', enableCellEdit: true},
			      {field: 'ref_from', displayName: 'ref_from', enableCellEdit: true},
			      {field: 'taxonomy', displayName: 'taxonomy', enableCellEdit: true},
			      {field: 'weight', displayName: 'weight', enableCellEdit: true},
			      {field: 'comment', displayName: 'comment', enableCellEdit: true}
			  ]};

    $scope.reloadEscad = function() {
	$scope.Escad.cmd = '(as "_view")';
	$scope.sendCommand();
	$timeout( function(){
            $scope.Escad.svg_url[0] = 'view/escad_view.svg';
        }, 2000 );
    };

    $scope.saveObject = function() {
	var sym_name = $scope.Escad.myObjectSelections[0].name;
	if ($scope.Escad.myObjectSelections[0].taxonomy.match("^escad\.symbol")) {
	    $http.post('/rest/version1/symbol/' + sym_name, $scope.Escad.myObjectSelections[0], {responseType: 'json'})
		.success(function(data, status) {
		    console.log('save symbol...' + ' answer: ' + data);
		    $scope.Escad.cmd_result = data.toString();
		    $scope.Escad.message = "saved symbol " + sym_name;
		})

//{"name":"_escad","attributes":[["url","https://github.com/mkollmar/escad"]],"comment":"Settings for escad belonging to this view.","taxonomy":"escad.symbol._escad","ref_to":"NIL","ref_from":"NIL","weight":"1"}

	    
		.error(function(data, status) {
		    console.log('object saving error: ' + data + status);
		    $scope.Escad.message = 'symbol saving error!';
		});
	} else { // save relation
	    // TODO!
	    console.log('CMD error: can not save relation');
	}
    };

    $scope.viewFile = function() {
        $scope.Escad.view_file = $scope.Escad.file_input;
    };

    $scope.sendCommand = function() {
	$http.post('/rest/version1/command', $scope.Escad.cmd, {params: {1: $scope.Escad.cmd}, responseType: 'text'})
	    .success(function(data, status) {
		console.log('CMD' + $scope.Escad.cmd + ' answer: ' + data);
		$scope.Escad.cmd_result = data.toString();
		$scope.Escad.message = "got escad response";
	    })
	    .error(function(data, status) {
		console.log('CMD error: ' + data + status);
		$scope.Escad.message = 'server seems not to repsond properly!';
	    });
    };

    // TODO! when submitting the add form, send the text to the node API
    $scope.createSymbol = function() {
	$http.post('/symbols', $scope.Escad.formData)
	    .success(function(data) {
		$scope.Escad.formData = {}; // clear the form so our user is ready to enter another
		$scope.Escad.symbols = data;
		console.log('create symbol, selectedL:');
	    })
	    .error(function(data) {
		console.log('createSymbol error: ' + data);
	    });
    };

    $scope.selectSymbol = function(id) {
	$http.get('/rest/version1/symbol/' + id, {responseType: 'json'})
	    .success(function(symbol) {
		console.log('got sym: ' + JSON.stringify(symbol));
		$scope.Escad.symbols = [];
		$scope.Escad.objects = [];
		var sym = new EscadObj(id, symbol[1], symbol[3], symbol[5], symbol[7], symbol[9], symbol[11]);
		$scope.Escad.symbols.push(sym);
		$scope.Escad.objects.push(sym);
	    })
	    .error(function(data) {
		console.log('selectSymbol error: ' + data);
	    });
    };

    // TODO:
    $scope.activateObject = function(id) {
	$http.delete('/symbol/' + id)
	    .success(function(data) {
		$scope.Escad.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

    // TODO:
    // when submitting the add form, send the text to the node API
    $scope.createRelation = function() {
	$http.post('/relation', $scope.formData)
	    .success(function(data) {
		$scope.Escad.formData = {}; // clear the form so our user is ready to enter another
		$scope.Escad.symbols = data;
		console.log('create symbol, selectedL:');
	    })
	    .error(function(data) {
		console.log('SError: ' + data);
	    });
    };

    // TODO:
    $scope.deleteObject = function(id) {
	$http.delete('/relation/' + id)
	    .success(function(data) {
		$scope.Escad.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

}]);
