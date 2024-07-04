const {exec} = require("child_process")

const magic_transform = './untitled/src/datalog/permutations/transform.sh'


function New(arg1, arg2) {
    this.arg1 = arg1
    this.arg2 = arg2
}

New.prototype.toString = function () {
    return `New(${this.arg1}, ${this.arg2})`
}

function Assign(arg1, arg2) {
    this.arg1 = arg1
    this.arg2 = arg2
}

Assign.prototype.toString = function () {
    return `Assign(${this.arg1}, ${this.arg2})`
}

function Load(arg1, arg2, arg3) {
    this.arg1 = arg1
    this.arg2 = arg2
    this.arg3 = arg3
}

Load.prototype.toString = function () {
    return `Load(${this.arg1}, ${this.arg2}, ${this.arg3})`
}


function Store(arg1, arg2, arg3) {
    this.arg1 = arg1
    this.arg2 = arg2
    this.arg3 = arg3
}

Store.prototype.toString = function () {
    return `Store(${this.arg1}, ${this.arg2}, ${this.arg3})`
}

function PointsTo(arg1, arg2) {
    this.arg1 = arg1
    this.arg2 = arg2
}

PointsTo.prototype.toString = function () {
    return `PointsTo(${this.arg1}, ${this.arg2})`
}

function PointsToField(arg1, arg2, arg3) {
    this.arg1 = arg1
    this.arg2 = arg2
    this.arg3 = arg3
}

PointsToField.prototype.toString = function () {
    return `PointsToField(${this.arg1}, ${this.arg2}, ${this.arg3})`
}


function Rule(head, body) {
    this.head = head
    this.body = body
}

// Computes all possible permutations of input array
function permutator(inputArr) {
    var results = [];

    function permute(arr, memo) {
        var cur, memo = memo || [];

        for (var i = 0; i < arr.length; i++) {
            cur = arr.splice(i, 1);
            if (arr.length === 0) {
                results.push(memo.concat(cur));
            }
            permute(arr.slice(), memo.concat(cur));
            arr.splice(i, 0, cur[0]);
        }

        return results;
    }

    return permute(inputArr);
}

Rule.prototype.permute = function () {
    let res = []
    let permutations = permutator(this.body)
    permutations.forEach(f => {
        res.push(new Rule(this.head, f))
    })

    // console.log(res)
    return res
}

Rule.prototype.toString = function () {
    let body_str = this.body.join(", ")
    return `${this.head} :- ${body_str}.`
}

let rule1 = new Rule(new PointsTo("x", "y"), [new New("x", "y")])
let rule2 = new Rule(new PointsTo("x", "z"), [new Assign("x", "y"), new PointsTo("y", "z")])
let rule3 = new Rule(new PointsTo("x", "v"), [new Load("x", "y", "f"), new PointsTo("y", "z"), new PointsToField("z", "f", "v")])
let rule4 = new Rule(new PointsToField("t", "f", "v"), [new Store("x", "f", "y"), new PointsTo("x", "t"), new PointsTo("y", "v")])

// All possible permutations of each rule
let rule_permutations = [
    rule1.permute(),
    rule2.permute(),
    rule3.permute(),
    rule4.permute()
]

// A program is a collection of 4 rules
let programs = []

// Generates all possible permutations or programs - : )
for (let i = 0; i < rule_permutations[0].length; i++) {
    for (let j = 0; j < rule_permutations[1].length; j++) {
        for (let k = 0; k < rule_permutations[2].length; k++) {
            for (let l = 0; l < rule_permutations[3].length; l++) {
                programs.push([rule_permutations[0][i], rule_permutations[1][j], rule_permutations[2][k], rule_permutations[3][l]])
            }
        }
    }
}


// Due to timing issues with the script and Node, we just write everything to disk and perform two passes...

const fs = require('fs')
const basePath = "untitled/src/datalog/permutations/analyses/"
const templateFile = "untitled/src/datalog/permutations/template.txt"
const templateString = fs.readFileSync(templateFile, "utf-8")

const resultPath = "untitled/src/datalog/permutations/output.dl"

// Construct a program, write to analysis.dl, execute transform.sh to transform into magic sets, output in output.dl
// and collect programs into transformed_programs.
for (const [i, p] of programs.entries()) {
    // console.log(`Program ${i}`)
    let data = templateString + "\n"


    for (const rule of p) {
        data = data + rule.toString() + "\n"
        // console.log(rule.toString())
    }
    data = data + '\n.decl query(t: token)\nquery(t) :- PointsTo("x", t).'
    fs.writeFileSync(basePath + `${i}.dl`, data)
}

// Execute the transformation script
exec('sh untitled/src/datalog/permutations/transform_all.sh', (a, b, c) => {
    console.log(b)
})