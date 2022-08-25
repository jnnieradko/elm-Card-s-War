const express = require('express')
const app = express()
const fs = require("fs")
const e = require("express");
const daneGraczyJSON = require("./daneGraczy.json");

let cors = require('cors')
app.use(cors())



app.get('/dane', function (req, res) {

  /*  fs.readFile("daneGraczy.json", function (err,data){
        if (err){
            console.log(err)
        } else res.send(data)
    })*/
    daneGraczy = "RORO - dane z serwera"



    res.send(daneGraczy)

})


app.get('/daneGraczy', function (req, res) {

    let page = req.query.page
    let limit = req.query.limit

    let startIndex = (page - 1)*limit
    let endIndex = page * limit

    const daneGraczyJSON = require("./daneGraczy.json")

    let result = daneGraczyJSON.table.slice(startIndex,endIndex)

    res.send(result)

})
app.get('/', function (req, res) {

    let data = require("./daneGraczy.json")
    let daneGraczy = data.table


    let filters = req.query;
    console.log(filters)
    let filteredPlayers = daneGraczy.filter(player => {
        let isValid = true;
        for (key in filters) {
            console.log(player[key])
            //console.log(key, player, filters[key]);
            isValid = isValid && player[key] == filters[key];
        }
        return isValid;
    });
    res.send(filteredPlayers);
});



app.post('/daneGraczy', function (req, res) {

    let newData = req.body
    fs.readFile('daneGraczy.json',function (err,data){
        if (err){
            console.log(err)
        } else {
            obj = JSON.parse(data)
            console.log(obj)
            obj.table.push(newData)

            console.log(obj)

            fs.writeFile('daneGraczy.json',JSON.stringify(obj),function (err){
                if(err){
                    console.log(err)
                } else if(res.status(200)) {console.log("Ok dane zapisane")}
            })
        }
    })
    res.status(200).send("dane wysłane")
})

app.listen(8001, function (){
    console.log("działa")
})



// paginacja , jak dodac do endpointu
// czy istnieje plik do którego wysyłany jest post
// sortowanie
// search
// total
// - endpoint z listą graczy, którzy grali kiedykolwiek
// - endpoint który daje statystyke, który gracz ile razy wygrał
