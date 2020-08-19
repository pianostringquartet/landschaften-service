# landschaften-service

#### landschaften-service is a Haskell service backend for [landschaften](https://github.com/pianostringquartet/landschaften), a visual explorer for paintings and their concepts. 

It uses [Stack](https://docs.haskellstack.org/en/stable/README/) for the build tool and [Servant](https://www.servant.dev/) for the server and API.

![](Claude_Monet,_Fishing_Boats_Leaving_the_Harbor,_Le_Havre.jpg) 

## Preparing to run the app

If you've never the run the app before, you will need to initialize it.

```
stack setup 
stack init
```

After initialization, run a one-time compilation and test:
```
stack test --fast
```


## Running app locally 

#### Set ENV variables and run the app

This Haskell app uses Stack, which defaults to a pure nix environment, 
where env-vars are not available. 

To enable them, we pass the `--no-nix-pure` flag when starting the app 
(`exec landschaften-servce`).  

```
$ export PORT=8080 
$ export DB_URL="postgresql://localhost:5432/bilder" 
$ stack --no-nix-pure exec landschaften-service
```

... `PORT` and `DB_URL` above are examples; be sure to use your own :)

#### Confirm app is running properly by making a CURL request

To confirm that it works by making a CURL request.

```
http://localhost:8080/query -X POST -H "Content-Type: text/plain; charset=utf-8" --data "{\"constraints\": [ {\"column\": \"timeframe\", \"values\": [\"1501-1550\"]}, {\"column\": \"school\", \"values\": [\"French\", \"Italian\", \"Spanish\", \"German\"]}, {\"column\": \"name\", \"values\": [\"scarf\", \"deer\"]}]}"
```

#### Run tests (Hspec)

Tests are defined as specs, per [Hspec](https://hspec.github.io/writing-specs.html).  
The `hspec-discover` pragma (see `test/Spec.hs`) will discover and run any specs. 

There are several ways to run the tests.

Compile app and run tests once:
```
stack test --fast
```

Alternatively, enter the `ghci` and reload changes and rerun tests as often as you like: 
```
stack exec ghci test/Spec.hs
```
Once in the `ghci`, use `:r` to reload app changes and `:main` to run the tests.


## Recompiling during development (Steel Overseer)  

I recommend using [Steel Overseer](https://github.com/schell/steeloverseer) to recompile upon source code changes.

The app's own steel-overseer file is `.sosrc`:

```$xslt
- patterns:
  - .*\.hs$
  - ^my-thing.cabal$
  - ^stack.yaml$
  commands:
  - stack build
  - stack exec landschaften-service

```



