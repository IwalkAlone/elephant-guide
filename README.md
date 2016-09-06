# Elephant Guide

A Magic: the Gathering deck tuning tool implementing the [Elephant Method](http://www.starcitygames.com/article/26317_The-Elephant-Method-A-Case-Study.html) (Building a 60-card post-sideboard decklist for each expected matchup, then deciding which specific cards go in the maindeck and which go in the sideboard). People usually do this with Google Sheets or similar, but my tool has special features that are hard to do in a general-purpose spreadsheet.

Still under development, but I already qualified for the Pro Tour using it! :P

Current features:

* Spreadsheet with automatic validation
* Supports different card counts while on the play/draw
* Preview your sideboard plans after filling out the spreadsheet

### Run it locally:
You'll need node.js and the Elm runtime.
```
npm install
elm-package install
npm start
```
