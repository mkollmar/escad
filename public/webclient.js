// Copyright (C) 2011, 2012, 2013, 2014, 2015, 2019 Markus Kollmar (email: markuskollmar@onlinehome.de)

'use strict';

var escad = angular.module('escad', ['ngGrid', 'ui.bootstrap']);


function GetRelations(scope, http) {
    http.get('/relations')
	.success(function(data) {
	    scope.relations = data.result;
	    console.log('Got: ' + data);
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});
}

function GetSymbols(scope, http) {
    http.get('/symbols')
	.success(function(data) {
	    scope.symbols = data.result;
	    console.log('Got: ' + data);
	    //var _objs = JSON.parse(data.result)
	    //var Symbole = data.result;
	    //for (var i = 0; i < Symbole.length; i++) {
	//	scope.symbols.push({name: Symbole[i], semantic: "is_part_of"});
	  //  }
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});
}

escad.controller('mainController', function($scope, $http, $injector) {
    $scope.formData = {};
    $scope.activation_result = " ";
    $scope.Messages = "Attributes:";
    $scope.relations = [];
    $scope.symbols = [];
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
			  showColumnMenu: true,
			  showFilter: true ,
			  columnDefs: [
			      {field: 'name', displayName: 'Name', enableCellEdit: true},
			      {field: 'attributes', displayName:'Attribute', enableCellEdit: true},
			      {field:'ref_to', displayName:'To', enableCellEdit: true},
			      {field:'ref_from', displayName:'From', enableCellEdit: true},
			      {field: 'taxonomy', displayName:'Taxonomy', enableCellEdit: true},
			      {field: 'weight', displayName:'Weight', enableCellEdit: true},
			      {field: 'comment', displayName: 'Comment', enableCellEdit: true}
			  ]};
    $scope.relationGrid = { data: 'relations',
			    multiSelect: true,
			    showColumnMenu: true,
			    showFilter: true,
			    columnDefs: [{field: 'name', displayName: 'Name', enableCellEdit: true}, {field:'semantic', displayName:'Semantik', enableCellEdit: true}, {field:'from', displayName:'Start', enableCellEdit: true}, {field:'to', displayName:'Target', enableCellEdit: true}, {field:'comment', displayName:'Comment', enableCellEdit: true}] };

    // on first load always execute:
    GetRelations($scope, $http);
    GetSymbols($scope, $http);
x3dom.reload;
    $scope.syncWithEscad = function() {
	GetSymbols($scope, $http);
	GetRelations($scope, $http);
	//console.log("Refresh symbols: " + JSON.stringify(data))
    };

    $scope.newView = function() {
    }

    $scope.exportView = function() {
    }

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
		console.log('SError: ' + data);
	    });
    };

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
