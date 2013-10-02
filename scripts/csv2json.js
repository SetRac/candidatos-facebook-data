/*jshint node:true*/

'use strict';

var fs = require('fs');
var csv = require('dsv').csv;

console.log(JSON.stringify(csv.parse(fs.readFileSync(process.argv[2], {
	encoding: 'utf8'
})), null, '\t'));
