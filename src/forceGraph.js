var d3 = require('d3');
var _ = require('lodash');

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


// module.exports = function (selector, nodes, links) {
//     // https://bl.ocks.org/colbenkharrl/21b3808492b93a21de841bc5ceac4e47#file-preview-png



//     if (nodesChanged(graph)) {
//         console.log("nodes changed");

//         curSimulation = d3.forceSimulation()
//             .force("link", d3.forceLink().distance(65).id(function(d) { return d.id; }))
//             .force("charge", d3.forceManyBody())
//             .force("center", d3.forceCenter(width / 2, height / 2));

//         curSimulation
//             .nodes(graph.nodes)
//             .on("tick", ticked);

//         curGraph = graph;
//     }

//     curGraph.links = graph.links;

//     if (curLinks) {
//         svg.select(".links").remove();
//     }

//     curLinks = svg.insert("g", ".nodes")
//         .attr("class", "links")
//         .selectAll("line")
//         .data(curGraph.links)
//         .enter().append("line")
//         .attr("stroke-width", function(d) { return 5; });

//     if (curLabels) {
//         curLabels.remove();
//     }

//     if (curNodes) {
//         curNodes.remove();
//     }

//     curNodes = svg.append("g")
//         .attr("class", "nodes")
//         .selectAll("g")
//         .data(curGraph.nodes)
//         .enter().append("g");

//     curCircles = curNodes.append("circle")
//         .attr("r", function(d) { if (d.type == 'table') { return 25; } else { return 5; } })
//         .attr("fill", function(d) { return color(d.group); })
//         .call(d3.drag()
//               .on("start", dragstarted)
//               .on("drag", dragged)
//               .on("end", dragended));

//     curNodes.append("title")
//         .text(function(d) { return d.id; });

//     curLabels = curNodes.append("text")
//         .text(function(d) {
//             return d.id;
//         })
//         .attr('x', labelOffset.x)
//         .attr('y', labelOffset.y)
//         .style('font-weight', 'bold')
//         .style('font-size', '1em')
//         .style('pointer-events', 'none');

//     curSimulation.force("link")
//         .links(curGraph.links);

//     curSimulation.alphaTarget(0.9).velocityDecay(0.9).restart();

// };

var simulation;
var nodeData;
var nodes;

var color = d3.scaleOrdinal(d3.schemeCategory20);
var labelOffset = { x: 6, y: 3 };

function ticked() {
    // if (curLinks) {
    //     curLinks
    //         .attr("x1", function(d) { return d.source.x; })
    //         .attr("y1", function(d) { return d.source.y; })
    //         .attr("x2", function(d) { return d.target.x; })
    //         .attr("y2", function(d) { return d.target.y; });
    // }

    if (nodes) {
        nodes
            .attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; });
    }

    // if (curLabels) {
    //     curLabels
    //         .attr("x", function(d) { return d.x + labelOffset.x; })
		//         .attr("y", function(d) { return d.y + labelOffset.y; });
    // }
}

function updateNodeData (nodeData) {

}

export function updateNodes(selector, newNodeData) {

    var svg = d3.select(selector),
        width = +svg.attr("width"),
        height = +svg.attr("height");

    nodes = svg.selectAll("circle.node")
        .data(nodeData, function (d) { return d.ref.type + ' ' + d.ref.id; })
        .enter()
        .append('circle')
        .attr('class', 'node')
        .attr('r', function (d) { return d.ref.type == 'table' ? 25 : 5; })
        .attr('fill', function (d) { return color(d.table); });

    nodes
        .exit()
        .remove();

    if (!simulation) {
        simulation = d3.forceSimulation()
            .force("charge", d3.forceManyBody())
            .force("center", d3.forceCenter(width / 2, height / 2));

        simulation.on("tick", ticked);
    }

    simulation.nodes(nodeData);
};

export function updateLinks(selector, links) {
};
