// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var localForage = require('localForage');
var app = Elm.Main.embed( document.getElementById( 'main' ) );


localForage.getItem('deck', function (err, value) {
    if (!err && value) {
        setTimeout(function () {
            app.ports.loadDeck.send(value);
        });
    }
    app.ports.saveDeck.subscribe(function (deck) {
        localForage.setItem('deck', deck);
    });
});
