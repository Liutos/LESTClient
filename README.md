# LESTClient

## Overview

Simulation of RESTClient that is a Firefox plugin for HTTP requests

## Startup

* Run a MongoDB listen on port 27017
* Run a Redis listen on port 8631
* Install by running the following command

```sh
ros install Liutos/LESTClient
```

* In the REPL, load this project by entering the following code

```lisp
(ql:quickload 'lestclient)
```

* Start this application by the following code

```lisp
(lestclient:start)
```

Now, you can open your web browser and access 'http://localhost:8087
