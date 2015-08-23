# LESTClient

## Overview

Simulation of RESTClient that is a Firefox plugin for HTTP requests

## Startup

* Open the Emacs editor
* Open the lestclient.asd in Emacs
* M-x and enter command slime
* Run the following Common Lisp Code

        (asdf:load-system 'lestclient)
        (in-package :lestclient)
        (lest-start)

Now, you can open your web browser and access 'http://localhost:4242'
