var d3 = require('d3');

var curSimulation;
var curGraph;
var curLinks;

function nodesChanged(graph) {
    /* REMOVEME */
    return true;

    if (!curGraph) return true;

    if (graph.nodes.length !== curGraph.nodes.length) {
        return true;
    }

    for (var i = 0; i < graph.nodes.length; i++) {
        if (graph.nodes[i].id !== curGraph.nodes[i].id) {
            return true;
        }
    }

    return false;
}

module.exports = function (selector, graph) {
    console.log(graph);

    var svg = d3.select(selector),
        width = +svg.attr("width"),
        height = +svg.attr("height");

    if (nodesChanged(graph)) {
        var simulation = d3.forceSimulation()
            .force("link", d3.forceLink().distance(55).id(function(d) { return d.id; }))
            .force("charge", d3.forceManyBody())
            .force("center", d3.forceCenter(width / 2, height / 2));

        function dragstarted(d) {
            if (!d3.event.active) simulation.alphaTarget(0.3).restart();
            d.fx = d.x;
            d.fy = d.y;
        }

        function dragged(d) {
            d.fx = d3.event.x;
            d.fy = d3.event.y;
        }

        function dragended(d) {
            if (!d3.event.active) simulation.alphaTarget(0);
            d.fx = null;
            d.fy = null;
        }

        var color = d3.scaleOrdinal(d3.schemeCategory20);

        var link = svg.append("g")
            .attr("class", "links")
            .selectAll("line")
            .data(graph.links)
            .enter().append("line")
            .attr("stroke-width", function(d) { return 5; });

        var node = svg.append("g")
            .attr("class", "nodes")
            .selectAll("g")
            .data(graph.nodes)
            .enter().append("g");

        var circles = node.append("circle")
            .attr("r", function(d) { if (d.type == 'table') { return 25; } else { return 5; } })
            .attr("fill", function(d) { return color(d.group); })
            .call(d3.drag()
                  .on("start", dragstarted)
                  .on("drag", dragged)
                  .on("end", dragended));

        var labelOffset = { x: 6, y: 3 };

        var labels = node.append("text")
            .text(function(d) {
                return d.id;
            })
            .attr('x', labelOffset.x)
            .attr('y', labelOffset.y);

        node.append("title")
            .text(function(d) { return d.id; });

        simulation.force("link")
            .links(graph.links);

        function ticked() {
            link
                .attr("x1", function(d) { return d.source.x; })
                .attr("y1", function(d) { return d.source.y; })
                .attr("x2", function(d) { return d.target.x; })
                .attr("y2", function(d) { return d.target.y; });


            circles
                .attr("cx", function(d) { return d.x; })
                .attr("cy", function(d) { return d.y; });

            labels
                .attr("x", function(d) { return d.x + labelOffset.x; })
		            .attr("y", function(d) { return d.y + labelOffset.y; });
        }

        simulation
            .nodes(graph.nodes)
            .on("tick", ticked);

    }

    curGraph = graph;
};
