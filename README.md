This library requires that you have a [RabbitMQ](http://www.rabbitmq.com/download.html) server installed and running. 

Use the supplied `Main.hs` to run the example:

```
module Main where

import SnapRabbit.Example
import SnapRabbit.Server

main :: IO ()
main = run $ Settings { amqpHostname    = "127.0.0.1"
                      , amqpVirtualHost = "/"
                      , amqpLoginName   = "guest"
                      , amqpPassword    = "guest"
                      , bindAddress     = "127.0.0.1"
                      , port            = 8000
                      }
```
