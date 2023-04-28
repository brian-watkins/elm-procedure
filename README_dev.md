# Develop Elm-Procedure

## Installing Dependencies

```
$ npm run install
```

Note: This should install the main dependencies and the
dependencies in the `/sample` directory, which is used for
integration testing.

## Running Tests

```
$ npm run test
$ npm run test:elm // just run elm-spec tests
$ npm run test:playwright // run playwright tests
$ npm run test:playwright:watch // run playwright tests in headed mode
```

Note that the playwright tests will automatically start the app in `/sample`
via an elm-live dev server. See `playwright.config.js` for the setup.

## Publishing

Run `npm elm diff` to determine the impact of changes on the version.

1. `npx elm bump`
  - Figures out the right semantic versioning for elm-spec and updates the elm.json; does not push to git
2. Commit and push
3. `git tag 1.1.0`
  - Where 1.1.0 is your new version
4. `git push --tags`
5. `npx elm publish`
