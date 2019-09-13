# Web Client

Welcome to the VivaDoc web client, which is a [SPA](https://www.wikiwand.com/en/Single-page_application). This is what
is hosted at [vivadoc.io](https://www.vivadoc.io). It is built in [Elm](https://elm-lang.org/).

### Overview

Project structure forked from
[this semi-official open source elm SPA example](https://github.com/rtfeldman/elm-spa-example).

I've added WebPack thanks to [this repo](https://github.com/simonh1000/elm-webpack-starter) being a helpful example.

For styling I've went with [Bulma](https://bulma.io/), a free and open-source solution. It's been lovely.


### Contributing

Please message me at `arie@vivadoc.io` if you are interested in contributing to the web-client with `CONTRIBUTER` in
the subject. We are really looking for contributors and would love your help. If you enjoy working in Elm and on
open source projects, this could be a good fit for you, don't hesitate to email me.

To be able to develop on the project you will need to set up a GitHub app to test out your code or use our staging
GitHub app. Either way, you'll want to shoot me an email so I can help get you setup.

##### Install

`npm install`

##### Development

`npm run dev`

You will want to have the API up and running otherwise the web-client will be entirely useless. Check out our
[nodes services repository](https://github.com/vivadoc/node-services) to see how to launch the API.

##### Prod

`npm run prod`

You will need to make sure that you have set the correct fields in the [webpack config file](/webpack.config.js). It
currently has our production API, but if you want to re-launch your own version of VivaDoc make sure to change those
fields to your API URI.
