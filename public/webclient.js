// Copyright (C) 2011, 2012, 2013, 2014 Markus Kollmar (email: markuskollmar@onlinehome.de)

'use strict';

var escad = angular.module('escad', ['ngGrid', 'ui.bootstrap']);

// escad.controller('SymbolListCtrl', function ($scope, $http) {
//     $http.get('/symbols').then(function(symbolResponse) {
// 	$scope.symbols = symbolResponse.data;
//     });
// });

function GetRelations(scope, http) {
    http.get('/symbols')
	.success(function(data) {
	    for (var i = 0; i < data.result.length; i++) {
		scope.relations.push({name: data.result[i], semantic: "is_part_of"});
	    }
	    console.log(data);
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});
}

function GetSymbols(scope, http) {
    http.get('/symbols/with-full-info')
	.success(function(data) {
	    scope.symbols = data;
	    console.log(data);
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});
}


function mainController($scope, $http,  $modal, $log) {
    $scope.formData = {};
    $scope.Messages = "Loaded";
    $scope.relations = [];
    $scope.symbols = [];
    $scope.radioModel = 'BrowseSymbolSemantic';
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
			  multiSelect: true,
			  showColumnMenu: true,
			  showFilter: true ,
			  columnDefs: [{field: 'name', displayName: 'Name', enableCellEdit: true}, 
				       {field:'semantic', displayName:'Semantik', enableCellEdit: true}]};
    $scope.relationGrid = { data: 'relations',
			    multiSelect: true,
			    showColumnMenu: true,
			    showFilter: true,
			    columnDefs: [{field: 'name', displayName: 'Name', enableCellEdit: true}, 
					 {field:'semantic', displayName:'Semantik', enableCellEdit: true}] };

    // on first load always execute:
    $http.get('/symbols')
	.success(function(data) {
	    for (var i = 0; i < data.result.length; i++) {
		$scope.symbols.push({name: data.result[i], semantic: "escad.setting"});
	    }
	    console.log(data);
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});

    GetRelations($scope, $http);
    GetSymbols($scope, $http);

    $scope.refreshSymbols = function() {
	$http.get('/symbolss')
	    .success(function(data) {
		$scope.symbols = data;
		console.log(data);
		console.log("Klick button!");
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
	    });
    };


    // when submitting the add form, send the text to the node API
    $scope.createSymbol = function() {
	$http.post('/symbols', $scope.formData)
	    .success(function(data) {
		$scope.formData = {}; // clear the form so our user is ready to enter another
		$scope.symbols = data;
		console.log(data);
	    })
	    .error(function(data) {
		console.log('Error: ' + data);
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


    $scope.items = ['item1', 'item2', 'item3'];

    $scope.open = function (size) {

	var modalInstance = $modal.open({
	    templateUrl: 'myModalContent.html',
	    controller: ModalInstanceCtrl,
	    size: size,
	    resolve: {
		items: function () {
		    return $scope.items;
		}
	    }
	});

	modalInstance.result.then(function (selectedItem) {
	    $scope.selected = selectedItem;
	}, function () {
	    $log.info('Modal dismissed at: ' + new Date());
	});
    };



}


// Please note that $modalInstance represents a modal window (instance) dependency.
// It is not the same as the $modal service used above.

var ModalInstanceCtrl = function ($scope, $modalInstance, items) {

    $scope.items = items;
    $scope.selected = {
	item: $scope.items[0]
    };

    $scope.ok = function () {
	$modalInstance.close($scope.selected.item);
    };

    $scope.cancel = function () {
	$modalInstance.dismiss('cancel');
    };
};
