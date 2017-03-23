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
        header: [
          {
            key: this.headerKey,
            value: this.headerValue
          }
        ],
        method: this.method,
        qs: [
          {
            key: this.queryKey,
            value: this.queryValue
          },
        ],
        timeout: this.timeout,
        url: this.url
      }, {
        'Content-Type': 'application/json'
      }).then(response => {
        const body = response.body;
        const data = body.data;
        const success = body.success;
        if (success) {
          this.response = data.content;
          this.headers = data.headers;
        } else {
          this.response = body.error;
        }
      }, response => {
        this.response = 'FAIL';
      });
    }
  }
});
