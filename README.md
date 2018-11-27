# Boston Elm Arcade [![Build Status](https://travis-ci.org/weekly-fp/boston-elm-arcade.svg?branch=master)](https://travis-ci.org/weekly-fp/boston-elm-arcade)

Games here are built by people at the Weekly Functional Programming Meetup or Elm Meetup in Boston!

Inspired by [boston-haskell-arcade](https://github.com/mitchellwrosen/boston-haskell-arcade) at the same Meetup

## Adding a game

Take a look at the `Main.elm` file. Its just a little boilerplate to add your
own game. You can use the directory structure laid out in `Snake` as a reference
if you choose.

## Developing

- Get node.js and npm for your distribution
- Run `npm install -g elm elm-format create-elm-app`
- run `elm-app start` in the project directory

### Develop With Docker

Want to avoid installing node on your machine? A Docker file is provided

- Configure and install [Docker](https://www.docker.com/get-started)
- Run `./docker/build-container.sh` to prepare the dev environment
- Run `./docker/enter-container.sh` to enter the container
- Inside the container, run `rm -r elm-stuff && elm-app start` to start a development server on port 3000
