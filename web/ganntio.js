//a namespace for defining gannt chart stuff
//specifically, reading text into
//google charts format for gannt
//files

google.charts.load('current', {'packages':['gantt']});
google.charts.setOnLoadCallback(drawChart);

function daysToMilliseconds(days) {
    return days * 24 * 60 * 60 * 1000;
}

function orNull(x,f){
    if (x = "") {return null;}
    else {return f(x);}    
}

function getDuration(x){
   return orNull(x, function(x){
        return  daysToMilliseconds(eval(x));})
}
//these are the fields the google charts
//api expects
var ganntSchema = ['string', 'Task ID', 
                   'string', 'Task Name',
                   'date',   'Start Date',
                   'date',   'End Date',
                   'number', 'Duration',
                   'number', 'Percent Complete',
                   'string', 'Dependencies'];


//coerce a line of text into a gantt row.
function ganttRow(ln){
    var xs = ln.split('\t');
    var res = [xs[0],
               xs[1],
               new Date(xs[2]),
               new Date(xs[3]),
               getDuration(xs[4]),
               eval(xs[5]),
               xs[6]];
    return res;
}

function ganttTable(xs){
    var data = new google.visualization.DataTable();
    data.addColumn('string', 'Task ID');
    data.addColumn('string', 'Task Name');
    data.addColumn('date', 'Start Date');
    data.addColumn('date', 'End Date');
    data.addColumn('number', 'Duration');
    data.addColumn('number', 'Percent Complete');
    data.addColumn('string', 'Dependencies');
    var rows = map(ganttRow, xs);
    data.addRows(rows);
    return data;
}

//the the chart, defined by xs, into element tgt.
function drawChart(lines, tgt){
    var data = ganttTable(lines);        
    var options = {height: 275};
    var chart = new google.visualization.Gantt(document.getElementById(tgt));
    chart.draw(data, options);
}
