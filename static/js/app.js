Vue.component('oauth', function (resolve, reject) {
  Vue.http.get('/api/client_id')
    .then(response => {
      const body = response.body;
      const data = body.data;
      const success = body.success;
      if (success) {
        const oauthUri = 'https://github.com/login/oauth/authorize?client_id=' + data['client-id'] + '&redirect_uri=' + encodeURIComponent(data['sign-in-uri']);
        resolve({
          template: '<a href="' + oauthUri + '">以GitHub帐号登录</a>'
        });
      } else {
        reject('获取client ID失败');
      }
    }, response => {
      reject('获取client ID失败');
    });
});

var token = null;

Vue.component('request-token', function (resolve, reject) {
  Vue.http.get('/api/request_token')
    .then(response => {
      const body = response.body;
      const data = body.data;
      const success = body.success;
      if (success) {
        token = data['token'];
        resolve({
          template: '<div>' + token + '</div>'
        });
      } else {
        reject('获取client ID失败');
      }
    }, response => {
      reject('获取request token失败');
    });
});

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
          this.status_code = data['status-code'];
          this.total_time = data['total-time'];
        } else {
          this.response = body.error;
        }
      }, response => {
        this.response = 'FAIL';
      });
    }
  }
});
