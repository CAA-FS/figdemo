var thefile;
var result;

function sleepFor( sleepDuration ){
    var now = new Date().getTime();
    while(new Date().getTime() < now + sleepDuration){ /* do nothing */ } 
}

//Client-side HTML5 File Reading....
function lines(file){
    var done = [false]
    var reader = new FileReader();
    reader.onload = function(progressEvent){
        // Entire file
        // console.log(this.result);

        // By lines
        result = this.result.split('\n')
        done[0] = true
    };
    reader.readAsText(file);  
    return [done, function(){return result}];
}

function deref(itm, store){    
    var res = itm[0];
    if(res[0] == true) {
        var f = itm[1];
        store  = f();        
        return store
    } else {setTimeout(function()
                    {console.log("Loading file");
                     deref(itm,store)}, 1000);
            }
}


function timeout() {
    setTimeout(function () {
        // Do Something Here
        // Then recall the parent function to
        // create a recursive loop.
        timeout();
    }, 1000);
}

//read a line of tab-delimited text into
//an array that matches our gannt schema
function lineToRow(ln){
    return ln.split('\t')    
}


//note, we can also use localstorage
//to maintain a simple kvp....
//it might be good to jam clojure
//data or json structures there

//note - we already have ALL this
//in clojurescript.  No need to
//screw around, but for now we'll
//implement in js donkeyland.

function map(f, coll){
    var res = [];
    for(var x = 0; x < coll.length; x++){
        res[x] = f(coll[x]);
    }
    return res;
}

function filter(f, xs){
    var res = [];
    var idx = 0;
    for(var x = 0; x < xs.length; x++){
        if (f(x)) {res[idx] = xs[x];
                 idx = idx + 1;}
    }
    return res;
}

function reduce(f, init, xs){
    var res = init;
    for(var x = 0; x < xs.length; x++){
        res = f(res,x);
    }
    return res;
}

function log(xs){
    for(var x = 0; x < xs.length; x++){
        console.log(xs[x]);
    }
}

function getFile(el){
    document.getElementById(el).onchange = function(){
        var file = this.files[0]; //first selected file.
        
    }
    thefile = file;
}

function currentfile (){
    return document.getElementById('file').files[0]
}
