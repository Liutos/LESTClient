((:get "/" lestclient::home)
 (:get "/sign_in" lestclient::sign-in)
 (:get "/sleepy" lestclient::sleepy)
 (:post "/api/request" lestclient::api-request))
