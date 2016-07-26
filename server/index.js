var express = require ('express')
var app = express()
var FS = require('q-io/fs')
var _ = require('lodash')
var bodyParser = require('body-parser')
var cors = require('express-cors')

var cards = [];
FS.read('./server/AllCards.json').then(function (data) {
    cards = JSON.parse(data);
    _.forIn(cards, (value, key) => {
        //console.log(value + " " + key);
        cards[key] = 0;
    });
}, function (error) {
    console.log(error);
});

app.use(bodyParser.json({type: () => true})); // for parsing application/json
app.use(cors({
  allowedOrigins: [
    'localhost:8080'
  ]
}))

app.get('/cards', (req, res) => {
    if (!req.query.name) {
        res.send([]);
        return;
    }
    var result = _(cards).keys().filter(name => {
        return name.toLowerCase().indexOf(req.query.name.toLowerCase()) === 0;
    }).take(5);
    res.send(result);
})

app.get('/deck', (req, res) => {
  FS.read('deck.json').then((data) => {
    res.send(data);
  })
})

app.post('/save', (req, res) => {
    console.log(req.body);
    FS.write('deck.json', JSON.stringify(req.body));
    res.send();
})

console.log('Listening on :3000')
app.listen(3000)
