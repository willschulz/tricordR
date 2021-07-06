// !preview r2d3 data=c(0.3, 0.6, 0.8, 0.95, 0.40, 0.20)
//
// r2d3: https://rstudio.github.io/r2d3
//

//Set some initial values
var margin_top = options.margin_top,
    margin_bottom = options.margin_bottom,
    margin_sides = options.margin_sides,
    width = width-(2*margin_sides), height = height-(margin_bottom+margin_top),
    xmax = options.xmax,
    xmin = options.xmin,
    ymax = options.ymax,
    ymin = options.ymin;

// Make Color Scales

var sentiment_colors = d3.scaleLinear()
    .domain([-.5, .5])
    .range(['#eb4034','#4ba637']);

//Create the axes

//var mindate = new Date(2021,0,1),
//    maxdate = new Date(2021,8,31);

var mindate = new Date(xmin),
    maxdate = new Date(xmax);

x = d3.scaleTime()
    .domain([mindate, maxdate])    // values between for month of january
    .range([margin_sides, width + margin_sides]);   // map these the the chart width = total width minus padding at both sides


//x = d3.scaleLinear()
//    .range([margin, margin+width])
//    .domain([xmin, xmax]);
y = d3.scaleLinear()
    //.range([height, 0])
    .range([height + (margin_top), 0])
    .domain([ymin, ymax]);

//Append axes
svg.append("g")
  .attr("transform", "translate(" + 0 + "," + (y(0)) + ")")
  .call(d3.axisBottom(x));
//svg.append("g")
  //.attr("transform", "translate(" + x(0) + ", " + margin_sides + ")")
  //.call(d3.axisLeft(y));

//Axes labels
svg.append("text")
  .attr("transform", "translate(" + (width/2) + " ," + (height+margin_top) + ")")
  .attr("dx", "1em") .style("text-anchor", "middle")
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .style("font-size", "12pt") .text(options.xLabel);

//svg.append("text") .attr("transform", "translate(" + 0 + " ," + ((height+2*margin_top)/2) + ") rotate(-90)")
  //.attr("dy", "1em")
  //.style("text-anchor", "middle")
  //.style("font-family", "Tahoma, Geneva, sans-serif")
  //.style("font-size", "12pt")
  //.text(options.yLabel);

//Create the chart title
svg.append("text")
  .attr("x", (width/2))
  .attr("y", (margin_top/2))
  .attr("text-anchor", "middle")
  .attr("dx", "1em")
  .style("font-size", "16pt")
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .text(options.chartTitle);


//Create the chart
svg.selectAll("dot")
  .data(data)
  .enter()
  .append("circle")
  .attr("cx", function (d) { return x(d.x); } )
  .attr("cy", function (d) { return y(d.y)+margin_top; } )
  .attr("r", function (d) { return d.size; })
  .attr("text", function (d) { return d.text; })
  .attr("sentiment", function (d) { return d.sentiment; })
  .attr("screen_name", function (d) { return d.screen_name; })
  .attr("created_at", function (d) { return d.created_at; })
  //.style('fill', function(d,i) {
  //      return sentiment_colors(d.sentiment);
  //  });
  .style("fill", options.colour);

//Create the chart
//svg.selectAll("dot")
//  .data(data)
//  .enter()
//  .append("circle")
//  .attr("cx", function (d) { return x(0); } )
//  .attr("cy", function (d) { return y(0)+margin; } )
//  .attr("r", 2)
//  .style("fill", options.colour);

//On-load transition for circles
//svg.selectAll('circle')
//  .transition()
//  .delay(function(d,i){return (i*30);}) .duration(function(d,i){return (2000+(i*2));})
//  .ease(d3.easeBack)
//  .attr("cx", function (d) { return x(d.x); } )
//  .attr("cy", function (d) { return y(d.y)+margin; } )
//  .attr("r", function (d) { return d.size; });


//Create a tooltip
var Tooltip = d3.select('.wrapper')
//var Tooltip = d3.select('.html-widget')
  .append('div')
  .attr("class", "tooltip")
  //.style('transform', 'scale(0,0)')
  .style('position', 'absolute')
  //.style('position', 'relative')
  .style('background-color', '#808080')
  .style('border-radius', '5px')
  .style('padding', '5px')
  .style('opacity', 0)
  .style("font-family", "Tahoma, Geneva, sans-serif")
  .style("font-size", "12pt");

// Three function that change the tooltip when user hovers, moves and leaves each dot
var mouseover = function(d) {
Tooltip
  .style('transition', '0.4s transform cubic-bezier(0.5,0.8,0,1.7)')
  .style('transform', 'scale(1,1)')
  .style('opacity', 1)
  //.style('box-shadow', '5px 5px 5px rgba(0,0,0,0.2)')
  ;
d3.select(this)
  .style('transition', '0.4s all cubic-bezier(0.5,0.8,0,1.7)')
  .style('fill', options.hovercolour)
  .attr("r", function (d) { return d.size + 2; })
  ;
  Shiny.setInputValue('tweet_text', d3.select(this).attr("text"));
  Shiny.setInputValue('tweet_sentiment', d3.select(this).attr("sentiment"));
  Shiny.setInputValue('tweet_screen_name', d3.select(this).attr("screen_name"));
  Shiny.setInputValue('tweet_created_at', d3.select(this).attr("created_at"));
};

var mouseleave = function(d) {
Tooltip
  .style("opacity", 0)
  .style('transform', 'scale(0,0)')
  ;
d3.select(this)
  .style('transition', '0.4s all cubic-bezier(0.5,0.8,0,1.7)')
  .style('fill', options.colour)
  .attr("r", function (d) { return d.size; })
  ;
};

svg.selectAll('circle')
  .on("mouseover", mouseover)
  .on("mouseleave", mouseleave);
