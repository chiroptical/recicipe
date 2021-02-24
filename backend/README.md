TODO
---

- [ ] Build a unit system to represent things like grams, ounces, numbers of things
  - [x] Figure out the units library
  - [ ] Document current units
  - [x] Add tests for parseMass
  - [x] Test and implement parseLiquidVolume
  - [x] Test and implement parseDryVolume
  - [x] Test and implement parseNumber
- [ ] Organize parsing code
- [ ] Ingredients parser (convert plural to singular for all words)
  - [ ] Figure out https://hackage.haskell.org/package/countable-inflections
- [ ] Add comments to code
  - [ ] Learn more Haddock

Future us problems
---

- When the parser fails, we would like a human readable error

Installation Notes
---

```
createdb recicipe
pgcli recicipe
> CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
```
