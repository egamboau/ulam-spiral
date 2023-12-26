#!/usr/bin/env node
const main = require("./main")

const { ArgumentParser } = require('argparse');

const parser = new ArgumentParser({
    description: 'Prints Ulam spiral on screen. Logs to stderr so output can be redirected'
  });

parser.add_argument('size', { metavar: 'N', type: 'int', help: "The size of the side of the square. The spiral will be of size N squared"});

const args = parser.parse_args()

let result =  main(args.size)

result.then(array => console.log(JSON.stringify(array)))