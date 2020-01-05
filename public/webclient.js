// Copyright (C) 2011, 2012, 2013, 2014, 2015, 2019 Markus Kollmar (email: markuskollmar@onlinehome.de)

'use strict';

var escad = angular.module('escad', ['ngGrid', 'ui.bootstrap']);

// Symbol object constructor:
function SymbolObj(name, attributes, comment, taxonomy, ref_to, ref_from, weight) {
    this.name = name;
    this.attributes = attributes;
    this.comment = comment;
    this.taxonomy = taxonomy;
    this.ref_to = ref_to;
    this.ref_from = ref_from;
    this.weight = weight;
}

// TODO:
function GetRelation(name, scope, http) {
    http.get('/symbols')
	.success(function(data) {
	    console.log('get relations from symbols: ' + JSON.stringify(scope.mySymbolSelections));
	})
	.error(function(data) {
	    console.log('GetRelations error: ' + data);
	});
}

function GetSymbols(scope, http) {
    http.get('/symbols')
	.success(function(data) {
	    var symbol_list = data;
	    console.log('GetSymbols read symbol: ' + JSON.stringify(symbol_list[0]));
	    scope.symbols = [];  // delete all old symbols
	    let symbol = [];
	    for (symbol of symbol_list[0]) {
		var sym = new SymbolObj(symbol[0], symbol[1], symbol[2], symbol[3], symbol[4], symbol[5], symbol[6]);
		scope.symbols.push(sym);
	    }
	})
	.error(function(data) {
	    console.log('GetSymbols error: ' + data);
	});
}

escad.controller('mainController', function($scope, $http, $injector) {
    $scope.formData = {};
    $scope.TEXT = "";  // Text input field
    $scope.activation_result = " ";
    $scope.relations = [];
    $scope.symbols = [];  // array of SymbolObj
    $scope.mySymbolSelections = [];
    $scope.myRelationSelections = [];
    $scope.selectedView = 'View1';  // toggles string View1 or View2
    $scope.selectedSemantic = undefined;
    $scope.status = { isopen: false };
    $scope.toggled = function(open) {console.log('Dropdown is now: ', open);};
    $scope.toggleDropdown = function($event) {
	$event.preventDefault();
	$event.stopPropagation();
	$scope.status.isopen = !$scope.status.isopen;
    };
    $scope.states = ['person', 'thing', 'country'];
    $scope.symbolGrid = { data: 'symbols',
			  selectedItems: $scope.mySymbolSelections,
			  multiSelect: true,
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
			      GetRelation(name, $scope, $http)
			      $scope.TEXT = "CHANGED!";
			  },
			  showFilter: true,
			  columnDefs: [
			      {field: 'name', displayName: 'Name', enableCellEdit: true},
			      {field: 'attributes', displayName: 'Attribute', enableCellEdit: true},
			      {field: 'ref_to', displayName: 'To', enableCellEdit: false},
			      {field: 'ref_from', displayName: 'From', enableCellEdit: false},
			      {field: 'taxonomy', displayName: 'Taxonomy', enableCellEdit: true},
			      {field: 'weight', displayName: 'Weight', enableCellEdit: true},
			      {field: 'comment', displayName: 'Comment', enableCellEdit: true}
			  ]};
    $scope.relationGrid = { data: 'relations',
			    multiSelect: true,
			    showColumnMenu: true,
			    enableColumnResize: true,
			    showFilter: true,
			    columnDefs: [
				{field: 'name', displayName: 'Name', enableCellEdit: true},
				{field: 'semantic', displayName: 'Semantik', enableCellEdit: true},
				{field: 'from', displayName: 'Start', enableCellEdit: true},
				{field: 'to', displayName: 'Target', enableCellEdit: true},
				{field: 'comment', displayName: 'Comment', enableCellEdit: true}
			    ]};

    // on first load always execute:
    GetSymbols($scope, $http);
    x3dom.reload;
    $scope.syncWithEscad = function() {
	GetSymbols($scope, $http);
    };

    // TODO:
    $scope.newView = function() {
    }

    // TODO:
    $scope.exportView = function() {
    }

    // TODO:
    $scope.loadView = function() {
    }
    
    // when submitting the add form, send the text to the node API
    $scope.createSymbol = function() {
	$http.post('/symbols', $scope.formData)
	    .success(function(data) {
		$scope.formData = {}; // clear the form so our user is ready to enter another
		$scope.symbols = data;
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
		$scope.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

    // TODO:
    $scope.editSymbol = function(id) {
	$http.delete('/symbols/' + id)
	    .success(function(data) {
		$scope.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

    // TODO:
    $scope.activateSymbol = function(id) {
	$http.delete('/symbols/' + id)
	    .success(function(data) {
		$scope.symbols = data;
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
		$scope.formData = {}; // clear the form so our user is ready to enter another
		$scope.symbols = data;
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
		$scope.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };

    // TODO:
    $scope.editRelation = function(id) {
	$http.delete('/relation/' + id)
	    .success(function(data) {
		$scope.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };
    
});
