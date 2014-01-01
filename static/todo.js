// Generated by CoffeeScript 1.6.3
'use strict';
var TodoCtrl;

TodoCtrl = function($scope, Todo) {
  $scope.todos = Todo.query();
  $scope.addTodo = function() {
    var newTodo;
    newTodo = {
      task: $scope.task,
      done: false
    };
    $scope.task = '';
    return Todo.save(newTodo, function(todo) {
      return $scope.todos.push(todo);
    });
  };
  $scope.deleteTodo = function(todo) {
    var idx;
    todo.$del();
    idx = $scope.todos.indexOf(todo);
    return $scope.todos.splice(idx, 1);
  };
  $scope.checkTodo = function(todo) {
    return todo.$save();
  };
  return $scope.editTodo = function(todo) {
    return todo.$save();
  };
};

angular.module('todoApp', ['todoServices']).config(['$routeProvider', function($routeProvider) {}]);