'use strict';

const path = require('path');


const Piscina = require('piscina');
const EventEmitter = require('events');

const winston = require('winston');

const logger =  winston.createLogger({
    level: 'info',
    format: winston.format.simple(),
    transports: [
        new winston.transports.Console({
            stderrLevels:["info"]
        })
    ]
})

async function main(size) {
    // to hold our data. Filling up of empty objects so is easy to track.
    let array = Array.from(Array(size), _ => Array(size).fill({}))

    //the center of the spiral changes, if the number is even or odd. Odd numbers need to be adjusted a bit, so the center is still ok
    var quotient = Math.trunc(size/2);
    var reminder  = size % 2;

    let xCenter = -1
    let yCenter = quotient

    if(reminder === 0) {
        // adjust the center, by going one to the left for the x axis, so we can go 
        xCenter = quotient - 1
    } else {
        xCenter = quotient
    }

    // initialize a pool for the jobs
    const piscina = new Piscina({
        filename: path.resolve(__dirname, 'ulam-spiral-worker.js'),
        maxQueue: 1,

    });

    const bus = new EventEmitter()

    piscina.on('drain', () =>{
        bus.emit('unlocked')
    })

    let currentTask = 1
    let submittedTasks = []
    while (currentTask <= Math.pow(size, 2)) {
        //Should we wait for the next schedule?
        if (piscina.needsDrain) {
            await new Promise(resolve => bus.once('unlocked', resolve))
        }

        let task_result = piscina.run({number:currentTask, x_center: xCenter, y_center: yCenter})
            .then((result) => {                
                array[result.y][result.x] = result
            })
        currentTask += 1
        submittedTasks.push(task_result)
        
    }

    return Promise.all(submittedTasks).then(() => {
        logger.info("Task completed!!");
        piscina.destroy()
        return array
    })
}



function joinArray(array) {

    formatRow = function(row) {
        return row.map((element) => element.n_is_prime?'*':'-' ).join("")
    }

    return array.map((row) => {
        return formatRow(row)
    }).join("\n")


    
}

module.exports = {
    main,
    joinArray
}