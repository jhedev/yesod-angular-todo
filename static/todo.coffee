'use strict'

TodoCtrl = ($scope, Todo) ->
    $scope.todos = Todo.query()

    $scope.addTodo = () ->
        newTodo = 
            task: $scope.task
            done: false
        $scope.task = ''
        Todo.save(newTodo, (todo) ->
                                $scope.todos.push(todo)
                 )

    $scope.deleteTodo = (todo) ->
        todo.$del()
        idx = $scope.todos.indexOf(todo)
        $scope.todos.splice(idx,1)

    $scope.checkTodo = (todo) ->
        todo.$save()

    $scope.editTodo = (todo) ->
        todo.$save()

angular.module('todoApp', ['todoServices']).config(['$routeProvider', ($routeProvider) -> ])
