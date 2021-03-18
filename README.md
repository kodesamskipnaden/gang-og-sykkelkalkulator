# Gang- og sykkelkalkulator

Nytte- og kostnadsanalyse verktøy for gang og sykkelvei tiltak

Author: Syver Enstad, Knut Aksel Røysland, Svend Børge Vullum

Deployes kontinuerlig til: http://gang-og-sykkelkalkulator.s3-website.eu-north-1.amazonaws.com/

# Installering

```
    $ curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash
In a new terminal:
    $ nvm install v14
    $ npm install -g elm@0.18
    $ npm install -g elm-test@0.18
    $ npm install
```

# Kjøring

```
    $ npm start
```

# Kjøring av test

```
    $ npm test
```

Kontinuerlig testing:

```
    $ npm run test:watch
```

# Lisens

MIT
