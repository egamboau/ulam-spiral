#!/usr/bin/env node
'use strict';

const path = require('path');

const { ArgumentParser } = require('argparse');
const Piscina = require('piscina');
const EventEmitter = require('events');

async function main(size) {
    // to hold our data. As this exercise is just for numbers greater than 1, fill with 0 will give us a good way
    // to know when a cell is empty
    let array = Array.from(Array(size), _ => Array(size).fill("0" ))

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
                console.error(`Completed task for number ${result.number}, is prime: ${result.n_is_prime})`)
                array[result.y][result.x] = result.n_is_prime? "*" : "-"
            })
        currentTask += 1
        submittedTasks.push(task_result)
        
    }

    Promise.all(submittedTasks).then(() => {
        console.error("Task completed!!")
        let joined_values = array.map(array_object => array_object.join(""))
        joined_values = joined_values.join("\n")
        console.log(joined_values)
    })
}


const parser = new ArgumentParser({
    description: 'Prints Ulam spiral on screen. Logs to stderr so output can be redirected'
  });

parser.add_argument('size', { metavar: 'N', type: 'int', help: "The size of the side of the square. The spiral will be of size N squared"});

const args = parser.parse_args()

main(args.size)