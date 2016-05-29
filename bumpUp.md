# Testing auto-deployment

```bash
./version.py
sudo elm-make Bracket.elm --output elm.js
git commit -am "bumping up to 19"
git push
```
