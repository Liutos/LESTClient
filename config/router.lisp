((:get "/" lestclient::home)
 (:get "/api/client_id" lestclient::get-client-id
       :requestp nil)
 (:get "/api/request_token" lestclient::get-request-token
       :requestp nil)
 (:get "/api/user" lestclient::get-user)
 (:get "/middlewares" lestclient::show-middlewares
       :requestp nil)
 (:get "/api/requests" lestclient::request-history)
 (:get "/sign_in" lestclient::sign-in)
 (:get "/sign_out" lestclient::sign-out
       :template "sign_out.html")
 (:get "/sleepy" lestclient::sleepy)
 (:post "/api/request" lestclient::api-request))
