# Testing auto-deployment
Use the version generation script to change the repo ever so slightly.

### Suggested sequence to follow:

	./version.py
	sudo elm-make Bracket.elm --output elm.js
	git commit -am "bumping up to 19"
	git push

### Launch using Heroku
Wait for a 5-10 seconds for the auto-deployment to complete. 
Either use the following command: 

	heroku open


or simply click [HerokuApp](http://bracket.kgisl.com) to launch the app.
