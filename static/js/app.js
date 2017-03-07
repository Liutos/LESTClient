var app = new Vue({
  el: '#app',
  data: {
    message: 'Welcome to LESTClient!'
  },
  methods: {
    echoMessage: function () {
      Vue.http.post('/api/request', {
        url: this.message
      }, {
        'Content-Type': 'application/json'
      }).then(response => {
        console.log('SUCCEED');
      }, response => {
        console.log('FAIL');
      });
    }
  }
});
