// Copyright (C) 2011, 2012, 2013, 2014, 2015, 2019, 2020 Markus Kollmar (email: markuskollmar@onlinehome.de)

'use strict';

var escad = angular.module('escad', ['ngGrid', 'ui.bootstrap']);

// Symbol object constructor:
function EscadObj(name, attributes, comment, taxonomy, ref_to, ref_from, weight) {
    this.name = name;
    this.attributes = attributes;
    this.comment = comment;
    this.taxonomy = taxonomy;
    this.ref_to = ref_to;
    this.ref_from = ref_from;
    this.weight = weight;
}


// Controller for html doc:
escad.controller("mainController", ['$scope', '$http', '$timeout', function($scope, $http, $timeout) {
    // on first load always execute:
    //x3dom.reload;
    $scope.Escad = {cmd_result:'loaded!'}; // make object to prevent trouble with javascript-inheritation
    $scope.Escad.file_input = 'view/language_training.svg';
    $scope.Escad.view_file = "view/language_training.svg";
    $scope.Escad.svg_url = ['view/escad_view.svg'];
    $scope.Escad.relations = [];
    $scope.Escad.symbols = [];  // array of SymbolObj
    $scope.Escad.objects = [];
    $scope.Escad.mySymbolSelections = [];
    $scope.Escad.selectedSymbol = $scope.Escad.mySymbolSelections[0];
    $scope.Escad.charSet = 'UTF8';
    $scope.Escad.cmd = '(ls)';
    $scope.Escad.message = 'Client loaded :-)';
    $scope.Escad.states = ['person', 'thing', 'country'];
    $scope.Escad.objectGrid = { data: 'Escad.objects',
			  selectedItems: $scope.Escad.mySymbolSelections,
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
			      $scope.Escad.message = "table CHANGED!";
			  },
			  showFilter: true,
			  columnDefs: [
			      {field: 'name', displayName: 'Name', enableCellEdit: true},
			      {field: 'attributes', displayName: 'Attribute', enableCellEdit: true},
			      {field: 'ref_to', displayName: 'To', enableCellEdit: true},
			      {field: 'ref_from', displayName: 'From', enableCellEdit: true},
			      {field: 'taxonomy', displayName: 'Taxonomy', enableCellEdit: true},
			      {field: 'weight', displayName: 'Weight', enableCellEdit: true},
			      {field: 'comment', displayName: 'Comment', enableCellEdit: true}
			  ]};

    $scope.reloadEscad = function(symbol) {
	$scope.Escad.cmd = '(as "_view")';
	$scope.sendCommand();
	$timeout( function(){
            $scope.Escad.svg_url[0] = 'view/escad_view.svg';
        }, 2000 );
    };

    $scope.viewFile = function() {
        $scope.Escad.view_file = $scope.Escad.file_input;
    };

    $scope.sendCommand = function() {
	$http.post('/command', $scope.Escad.cmd, {params: {1: $scope.Escad.cmd}})
	    .success(function(data, status) {
		console.log('CMD' + $scope.Escad.cmd + ' answer: ' + data);
		$scope.Escad.cmd_result = data.toString();
		$scope.Escad.message = "got escad response";
		//$scope.$apply('Escad.message = "got escad response"');
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

    // TODO:
    $scope.deleteSymbol = function(id) {
	$http.delete('/symbols/' + id)
	    .success(function(data) {
		$scope.Escad.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

    $scope.selectSymbol = function(id) {
	$http.get('/symbol/' + id)
	    .success(function(symbol) {
	    console.log('GetSymbols read symbol: ' + JSON.stringify(symbol_list[0]));
	    scope.Escad.symbols = [];  // delete all old symbols
	    //let symbol = [];
	    //for (symbol of symbol_list[0]) {
		var sym = new Obj(symbol[0], symbol[1], symbol[2], symbol[3], symbol[4], symbol[5], symbol[6]);
		scope.Escad.symbols.push(sym);
	    //}
		console.log('GetSymbols read symbol: ' + JSON.stringify(symbol));
		var sym = new Obj(symbol[0], symbol[1], symbol[2], symbol[3], symbol[4], symbol[5], symbol[6]);
		$scope.Escad.objects = [];
		$scope.Escad.objects.push(sym);
	    })
	    .error(function(data) {
		console.log('selectSymbol error: ' + symbol);
	    });
    };

    // TODO:
    $scope.activateSymbol = function(id) {
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
    $scope.deleteRelation = function(id) {
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
