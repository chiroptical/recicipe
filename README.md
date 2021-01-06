Description
---

A recipe website that doesn't suck

Goals
---

Develop via Test Driven Development

Software
---

- Haskell Backend
- Elm Frontend

Limitations
---

- One picture
- No descriptions

Steps
---

1. Make a sweet recipe parser

```txt
Preheat oven to [450F]

Mix [1 egg] and [1 cup cheese]
```

  - Should be able to automatically generate a list of ingredients and their total measurements
  - Should be able to automatically detect ingredients and save them to the database (for tagging)
  - Yaks:
    - A unit system (this library allows you to build a unit system: https://hackage.haskell.org/package/units)

2. Build a backend which:
  - saves recipes
  - user logins:
    - https://hackage.haskell.org/package/password
    - https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html
    - https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-63b.pdf
    - https://gitlab.com/gilmi/my-scotty-users
    - https://crackstation.net/hashing-security.htm
    - https://nitratine.net/blog/post/how-to-hash-passwords-in-python

3. Build a frontend:
  - form to input recipes
  - user logins
  - search by title, ingredient

Ideas
---

Recipes should be rated:

- :100: -- a simple and readable recipe
- :cloud: -- a recipe you would find on other websites

Cool features:

- give people points for properly annotating recipes
- ban words like "best" in title (should this be delegated to the users? i.e. :cloud:)
- random pages (by ingredient e.g.)
- saved recipes
- shopping list
- swipe right, swipe left for random recipes?
- points system for detecting "fluff"
- halving and multiplying recipes
- common substitutions for ingredients
- estimate overall cost for ingredients
- give people points for translating recipes

Stream Suggestions
---

- Haskell version of https://github.com/paragonie/paseto
