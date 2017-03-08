var app = new Vue({
  el: '#app',
  data: {
    method: 'GET',
    url: 'http://example.com',
    response: 'Here comes response'
  },
  methods: {
    echoMessage: function () {
      Vue.http.post('/api/request', {
        method: this.method,
        url: this.url
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
