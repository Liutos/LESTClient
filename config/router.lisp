((:get "/" lestclient::home)
 (:get "/api/client_id" lestclient::get-client-id
       :requestp nil)
 (:get "/api/request_token" lestclient::get-request-token
       :requestp nil)
 (:get "/sign_in" lestclient::sign-in)
 (:get "/sleepy" lestclient::sleepy)
 (:post "/api/request" lestclient::api-request))
