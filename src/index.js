// pull in desired CSS/SASS files
require( './styles/material.min.css' );
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( './Main' );
var localForage = require('localForage');
var app = Elm.Main.embed( document.getElementById( 'main' ) );


localForage.getItem('deck', function (err, value) {
    if (!err && value) {
        setTimeout(function () {
            //app.ports.loadDeck.send(value);
        });
    }
    app.ports.saveDeck.subscribe(function (deck) {
        localForage.setItem('deck', deck);
    });
});

app.ports.requestTableMetrics.subscribe(function () {
  var rowBottoms = Array.prototype.map.call(document.querySelectorAll('tr'), function (tr) {
    return tr.getBoundingClientRect().bottom;
  });
  app.ports.receiveTableMetrics.send({rowBottoms: rowBottoms});
})

app.ports.focus.subscribe(function (id) {
  applyToElement(id, function (element) {
    element.focus();
  });
});

app.ports.selectText.subscribe(function (id) {
  applyToElement(id, function (element) {
    element.select();
  });
});

function applyToElement(id, callback) {
  requestAnimationFrame(function () {
    var element = document.getElementById(id);
    if (!element) {
      console.error("Could not find element by ID: " + id);
      return;
    }
    callback(element);
  });
}
