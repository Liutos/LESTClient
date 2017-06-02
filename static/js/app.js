Vue.component('oauth', function (resolve, reject) {
  Vue.http.get('/api/client_id')
    .then(response => {
      const body = response.body;
      const data = body.data;
      const success = body.success;
      if (success) {
        const oauthUri = 'https://github.com/login/oauth/authorize?client_id=' + data['client-id'] + '&redirect_uri=' + encodeURIComponent(data['sign-in-uri']);
        resolve({
          data: function () {
            return {
              oauthUri
            };
          },
          template: '<li><a v-bind:href="oauthUri">以GitHub帐号登录</a></li>'
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
          data: function () {
            return {
              token
            };
          },
          template: '<li><a href="#">{{ token }}</a></li>'
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
          template: '<li><a><img v-bind:alt="login" height="20" v-bind:src="avatar_url" width="20"></img></a></li>'
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
    requestHeaders: [
      {
        key: '',
        value: ''
      }
    ],
    method: 'GET',
    qs: [
      {
        key: '',
        value: ''
      }
    ],
    url: 'http://example.com',
    response: 'Here comes response',
    requestBody: 'Content for test',
    requestHistory: [
      {
        url: 'http://example.com'
      }
    ],
    requestStage: '就绪',
    userAgent: null
  },
  methods: {
    addQueryString: function () {
      this.qs.push({
        key: '',
        value: ''
      });
    },
    addRequestHeader: function () {
      this.requestHeaders.push({
        key: '',
        value: ''
      });
    },
    echoMessage: function () {
      this.requestStage = '请求中';
      Vue.http.post('/api/request', {
        body: this.requestBody,
        header: this.requestHeaders,
        method: this.method,
        qs: this.qs,
        timeout: this.timeout,
        token,
        url: this.url,
        'user-agent': this.userAgent
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
          this.ip_address = data['ip-address'];
          this.total_time = data['total-time'];
          this.requestStage = '成功';
        } else {
          this.response = body.error;
          this.requestStage = body.error;
        }
      }, response => {
        this.response = response.statusText;
        this.requestStage = '失败';
      });
    }
  }
});
