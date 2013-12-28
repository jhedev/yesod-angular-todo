'use strict'
angular.module('todoServices', ['ngResource']).
    factory('Todo', ($resource) -> 
                        return $resource('/api/todo/:tid', {tid:'@id'}, 
                             query: 
                                method:'GET'
                                isArray:true
                             save: 
                                method:'PUT'
                                tid:'@id'
                             del: 
                                method:'DELETE'
                        )
            )
