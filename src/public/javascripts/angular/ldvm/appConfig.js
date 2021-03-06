define(['./app'], function (app) {
    'use strict';

    app.config(['$routeProvider', '$locationProvider', function ($routeProvider) {

        var partialsPath = '/assets/javascripts/angular/ldvm/partials/';

        $routeProvider.when('/list', {templateUrl: partialsPath + 'list.html', controller: 'List', reloadOnSearch: false});
        $routeProvider.when('/discover', {templateUrl: partialsPath + 'discover.html', controller: 'Discover', reloadOnSearch: false});
        $routeProvider.when('/evaluate/:id', {templateUrl: partialsPath + 'evaluate.html', controller: 'Evaluate', reloadOnSearch: false});
        $routeProvider.when('/', {templateUrl: partialsPath + 'index.html', controller: 'Index', reloadOnSearch: false});
        $routeProvider.when('/validators', {templateUrl: partialsPath + 'validator/list.html', controller: 'ValidatorList', reloadOnSearch: false });
        $routeProvider.when('/validator/skos/:id?', {templateUrl: partialsPath + 'validator/skos.html', controller: 'SkosValidator', reloadOnSearch: false });
        $routeProvider.when('/validator/dataCube/:id?', {templateUrl: partialsPath + 'validator/dataCube.html', controller: 'DataCubeValidator', reloadOnSearch: false });
        $routeProvider.when('/:page', {templateUrl: partialsPath + 'list.html', controller: 'List', reloadOnSearch: false});
        $routeProvider.when('/compatibility/check/:id', {templateUrl: partialsPath + 'compatibilityCheck.html', controller: 'CompatibilityCheck', reloadOnSearch: false});
        $routeProvider.when('/compatibility/:id', {templateUrl: partialsPath + 'compatibility.html', controller: 'Compatibility', reloadOnSearch: false});
        $routeProvider.when('/detail/:id', {templateUrl: partialsPath + 'detail.html', controller: 'Detail', reloadOnSearch: false });
        $routeProvider.when('/result/:id', {templateUrl: partialsPath + 'result.html', controller: 'Result', reloadOnSearch: false });
        $routeProvider.when('/hierarchy/:type/:id', {templateUrl: partialsPath + 'visualizer/hierarchy.html', controller: 'Hierarchy', reloadOnSearch: false });
        $routeProvider.otherwise({redirectTo: '/'});
    }])
        .config(function ($provide) {
            $provide.decorator("$exceptionHandler", function ($delegate, $injector) {
                return function (exception, cause) {
                    //alert("Unexpected error.");
                    $delegate(exception, cause);
                };
            });

        })
        .factory('errorHttpInterceptor', ['$q', function ($q, $modal) {
            return {
                responseError: function responseError(rejection) {

                    //alert("Communication with server failed.");
                    return $q.reject(rejection);
                }
            };
        }])
        .config(['$httpProvider', function ($httpProvider) {
            $httpProvider.interceptors.push('errorHttpInterceptor');
        }])
        .config(['cfpLoadingBarProvider', function (cfpLoadingBarProvider) {
            cfpLoadingBarProvider.includeSpinner = false;
            cfpLoadingBarProvider.latencyThreshold = 0;
        }]);
});