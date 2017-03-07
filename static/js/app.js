var app = new Vue({
  el: '#app',
  data: {
    message: 'http://example.com',
    response: 'Here comes response'
  },
  methods: {
    echoMessage: function () {
      Vue.http.post('/api/request', {
        url: this.message
      }, {
        'Content-Type': 'application/json'
      }).then(response => {
        const body = response.body;
        this.response = body;
      }, response => {
        this.response = 'FAIL';
      });
    }
  }
});
