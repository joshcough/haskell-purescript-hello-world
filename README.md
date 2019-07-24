# haskell-purescript-hello-world

### Start in the `basic` folder. 

1. Build the Haskell code

```
stack build
```

1a. Run the Purescript bridge to create the Purescript code from Haskell types

You only need to do this if you made any changes to the Haskell code in Models.hs

```
stack ghci
> PSBridge.main
```

2. Build the Purescript code

```
cd frontend
npm install
npm run build
```

3. Start the Servant server

```
cd ..
stack ghci
> Scaffolding.Init.runApp
```

or...

```
cd ..
stack run haskell-purescript-hello-world-server
```

(I prefer the first style because it allows me to iterate on my code faster)

4. Open http://localhost:8081
