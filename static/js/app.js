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
        reject('获取request token失败');
      }
    }, response => {
      reject('获取request token失败');
    });
});

Vue.component('user', function (resolve, reject) {
  Vue.http.get('/api/user')
    .then(response => {
      const body = response.body;
      const data = body.data;
      const success = body.success;
      if (success) {
        const user = data.user;
        resolve({
          data: function () {
            return {
              avatar_url: user.avatar_url,
              login: user.login
            };
          },
          template: '<div><span>{{ login }}</span><img v-bind:src="avatar_url"></img></div>'
        });
      } else {
        reject('获取用户信息失败');
      }
    }, response => {
      reject('获取用户信息失败');
    });
});

var app = new Vue({
  el: '#app',
  data: {
    headerKeys: [],
    headerValues: [],
    method: 'GET',
    url: 'http://example.com',
    response: 'Here comes response',
    requestBody: 'Content for test'
  },
  methods: {
    echoMessage: function () {
      const header = this.headerKeys.map((e, i) => {
        return {
          key: e,
          value: this.headerValues[i]
        };
      });
      Vue.http.post('/api/request', {
        body: this.requestBody,
        header,
        method: this.method,
        qs: [
          {
            key: this.queryKey,
            value: this.queryValue
          },
        ],
        timeout: this.timeout,
        token,
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
          token = data.token;
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
