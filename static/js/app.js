var app = new Vue({
  el: '#app',
  data: {
    method: 'GET',
    url: 'http://example.com',
    response: 'Here comes response',
    requestBody: 'Content for test'
  },
  methods: {
    echoMessage: function () {
      Vue.http.post('/api/request', {
        body: this.requestBody,
        method: this.method,
        qs: [
          {
            key: this.queryKey,
            value: this.queryValue
          },
        ],
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
