'use strict';

var escad = angular.module('escad', []);

escad.controller('SymbolListCtrl', function ($scope, $http) {
    $http.get('/symbols').then(function(symbolResponse) {
	$scope.symbols = symbolResponse.data;
    });
});

function mainController($scope, $http) {
    $scope.formData = {};
    $scope.symbols = ["none!"];

    // when landing on the page, get all symbols and show them
    $http.get('/symbols')
	.success(function(data) {
	    $scope.symbols = data.result;
	    console.log(data);
	})
	.error(function(data) {
	    console.log('Error: ' + data);
	});

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

}