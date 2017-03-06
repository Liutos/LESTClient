(function () {
  var app = new Vue({
    el: '#app',
    data: {
      message: 'Welcome to LESTClient!'
    },
    methods: {
      echoMessage: function () {
        console.log('Content of message is ' + this.message);
      }
    }
  });
})();
