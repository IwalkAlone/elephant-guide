// pull in desired CSS/SASS files
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var localForage = require('localForage');
Elm.Main.embed( document.getElementById( 'main' ) );
localForage.setItem('decks', {
    a: 2,
    b: 3
});
