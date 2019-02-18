var d3 = require('d3');

var curSimulation;
var curGraph;
var curLinks;
var curNodes;
var curLabels;
var curCircles;

function nodesChanged(graph) {
    if (!curGraph) return true;

    if (graph.nodes.length !== curGraph.nodes.length) {
        return true;
    }

    var aIds = [];
    var bIds = [];

    for (var i = 0; i < graph.nodes.length; i++) {
        aIds.push(graph.nodes[i].id);
        bIds.push(graph.nodes[i].id);
    }

    aIds.sort();
    bIds.sort();

    for (var j = 0; j < graph.nodes.length; j++) {
        if (aIds[j] !== bIds[j]) {
            return true;
        }
    }

    return false;
}

function dragstarted(d) {
    if (!d3.event.active) curSimulation.alphaTarget(0.3).restart();
    d.fx = d.x;
    d.fy = d.y;
}

function dragged(d) {
    d.fx = d3.event.x;
    d.fy = d3.event.y;
}

function dragended(d) {
    if (!d3.event.active) curSimulation.alphaTarget(0);
    d.fx = null;
    d.fy = null;
}

var color = d3.scaleOrdinal(d3.schemeCategory20);
var labelOffset = { x: 6, y: 3 };

function ticked() {
    if (curLinks) {
        curLinks
            .attr("x1", function(d) { return d.source.x; })
            .attr("y1", function(d) { return d.source.y; })
            .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; });
    }

    if (curCircles) {
        curCircles
            .attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; });
    }

    if (curLabels) {
        curLabels
            .attr("x", function(d) { return d.x + labelOffset.x; })
		        .attr("y", function(d) { return d.y + labelOffset.y; });
    }
}

module.exports = function (selector, graph) {
    // https://bl.ocks.org/colbenkharrl/21b3808492b93a21de841bc5ceac4e47#file-preview-png
    console.log(graph);

    var svg = d3.select(selector),
        width = +svg.attr("width"),
        height = +svg.attr("height");


    if (nodesChanged(graph)) {
        console.log("nodes changed");

        curSimulation = d3.forceSimulation()
            .force("link", d3.forceLink().distance(45).id(function(d) { return d.id; }))
            .force("charge", d3.forceManyBody())
            .force("center", d3.forceCenter(width / 2, height / 2));

        curSimulation
            .nodes(graph.nodes)
            .on("tick", ticked);

        if (curNodes) {
            curNodes.remove();
        }

        curNodes = svg.append("g")
            .attr("class", "nodes")
            .selectAll("g")
            .data(graph.nodes)
            .enter().append("g");

        curCircles = curNodes.append("circle")
            .attr("r", function(d) { if (d.type == 'table') { return 25; } else { return 5; } })
            .attr("fill", function(d) { return color(d.group); })
            .call(d3.drag()
                  .on("start", dragstarted)
                  .on("drag", dragged)
                  .on("end", dragended));

        curLabels = curNodes.append("text")
            .text(function(d) {
                return d.id;
            })
            .attr('x', labelOffset.x)
            .attr('y', labelOffset.y);

        curNodes.append("title")
            .text(function(d) { return d.id; });

    }

    if (curLinks) {
        curLinks.remove();
    }

    curLinks = svg.append("g")
        .attr("class", "links")
        .selectAll("line")
        .data(graph.links)
        .enter().append("line")
        .attr("stroke-width", function(d) { return 5; });

    curSimulation.force("link")
        .links(graph.links);

    curSimulation.alphaTarget(0.3).velocityDecay(0.8).restart();

    curGraph = graph;
};
