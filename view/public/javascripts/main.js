
// let nodes = [
//   [1, 2, 1],
//   [2, 19, 1],
//   [3, 3, 3],
//   [4, 9, 5],
//   [5, 16, 5],
//   [6, 3, 8],
//   [7, 8, 12],
//   [8, 16, 12]
// ]

// let edges = [
//   [1, 1, 2],
//   [2, 1, 3],
//   [3, 2, 5],
//   [4, 3, 4],
//   [5, 3, 6],
//   [6, 4, 5],
//   [7, 4, 7],
//   [8, 5, 8],
//   [9, 6, 7],
//   [10, 7, 8],
//   [11, 4, 6],
// ]

let nodes = [
  [0, 263.35423368818044, -41.66044206649194],
  [1, 238.60036976480356, 45.9301533546878],
  [2, 360.9318400625076, 21.176289431310977],
  [3, 254.29981085411475, 249.67349487786697],
  [4, 399.01470763693357, 320.1267998905551],
  [5, 391.39813412204836, 223.01548757576876],
  [6, 374.2608437135567, 122.09588850353987],
  [7, 337.6158254583112, -100.68888680685225],
  [8, 592.7007805965549, -114.48418451744215],
  [9, 430.9188510156549, -39.75629868777065],
  [10, 579.6646264266258, 0.6608449271561767],
  [11, 715.1475438849548, 87.38629950182124],
  [12, 729.757100047713, -62.241253736093114],
  [13, 851.1191130234571, 58.82759610948176],
  [14, 682.6181804926973, 229.43095243282818],
  [15, 901.739214937918, 224.74437550517135],
  [16, 787.6016375995532, 329.4186640829],
  [17, 907.1746963026433, 375.5806910625377],
  [18, 788.6284402031968, 461.49970397547213],
  [19, 657.0853235616883, 403.0615594941273],
  [20, 552.7840272538181, 322.0309432692764]
]

let edges = [
  [1, 1, 0],
  [2, 2, 0],
  [3, 2, 1],
  [4, 3, 1],
  [5, 4, 3],
  [6, 5, 3],
  [7, 5, 4],
  [8, 6, 2],
  [9, 6, 5],
  [10, 7, 0],
  [11, 8, 7],
  [12, 9, 7],
  [13, 9, 8],
  [14, 10, 6],
  [15, 10, 9],
  [16, 11, 10],
  [17, 12, 8],
  [18, 12, 10],
  [19, 13, 11],
  [20, 13, 12],
  [21, 14, 11],
  [22, 15, 13],
  [23, 16, 14],
  [24, 16, 15],
  [25, 17, 15],
  [26, 17, 16],
  [27, 18, 16],
  [28, 18, 17],
  [29, 19, 16],
  [30, 19, 18],
  [31, 20, 4],
  [32, 20, 14],
  [33, 20, 19]
]

let svgWidth = 1100;
let svgHeight = 1100;

let viewWidth = 800;
let viewHeight = 800;

// let domainX = [-120, -140]
// let domainY = [32, 37]

let domainX = [100, 1000]
let domainY = [-200, 500]

var xScale = d3.scale.linear()
                   .domain(domainX)
                   .range([0, viewWidth]);

var xScaleReverse = d3.scale.linear()
                   .domain([0, viewWidth])
                   // .range(domainX.slice().reverse());
                   .range(domainX);

var yScale = d3.scale.linear()
                   .domain(domainY)
                   .range([viewHeight, 0]);

var yScaleReverse = d3.scale.linear()
                   .domain([viewHeight, 0])
                   // .range(domainY.slice().reverse());
                   .range(domainY);

let svg = d3.select('body')
          .append('svg')
          .attr('width', svgWidth)
          .attr('height', svgHeight);

// Grid
let svgGridX = svg.selectAll('gridline')
                .data([0, 5, 10, 15, 20])
                .enter()
                .append('line');

svgGridX.attr('x1', function(d) { return xScale(d); }) 
      .attr('y1', function(d) { return yScale(0); })
      .attr('x2', function(d) { return xScale(d); })
      .attr('y2', function(d) { return yScale(20); })
      .attr('class', 'gridline');

let svgGridY = svg.selectAll('gridline')
                .data([0 , 5, 10, 15, 20])
                .enter()
                .append('line');

svgGridY.attr('x1', function(d) { return xScale(0); })
      .attr('y1', function(d) { return yScale(d); })
      .attr('x2', function(d) { return xScale(20); })
      .attr('y2', function(d) { return yScale(d); })
      .attr('class', 'gridline')

// edges
let edgesEnter = svg.selectAll('edges')
                .data(edges)
                .enter();

function findNode(id) {
for(let i = 0; i < nodes.length; i++) {
  if (nodes[i][0] == id) return nodes[i];
}

return null;
}

edgesEnter.append('line')
  .attr('x1', function(d) { return xScale(findNode(d[1])[1]); })
  .attr('y1', function(d) { return yScale(findNode(d[1])[2]); })
  .attr('x2', function(d) { return xScale(findNode(d[2])[1]); })
  .attr('y2', function(d) { return yScale(findNode(d[2])[2]); })
  .attr('class', 'edges')

edgesEnter.append('text')
  .attr('x', function(d){
        x = findMiddleEdge(findNode(d[1]), findNode(d[2]))[0]
        return xScale(x); 
  })
  .attr('y', function(d){
        y = findMiddleEdge(findNode(d[1]), findNode(d[2]))[1]
        return yScale(y);
  })
  .attr('class', 'edge-label')
  .text(function(a, i) { return "e" + a[0]; });

// objects
function findLocation(edgeId, pos){
let edge;
for (let i = 0; i < edges.length; i++) {
  if (edges[i][0] == edgeId) {
    edge = edges[i]
  }
}

let node1 = findNode(edge[1])
let node2 = findNode(edge[2])

let x = (node2[1] - node1[1])*pos + node1[1]
let y = (node2[2] - node1[2])*pos + node1[2]

return [x, y]
}

// nodes
let nodesEnter = svg.selectAll('nodes')
                .data(nodes)
                .enter()
                

nodesEnter.append('rect')
      .attr('x', function(d){ return xScale(d[1]) - 7.5; })
      .attr('y', function(d){ return yScale(d[2]) - 7.5; })
      .attr('class', 'nodes')
      .attr('width', 15)
      .attr('height', 15);

nodesEnter.append('text').attr('x', function(d){ return xScale(d[1]); })
  .attr('y', function(d){ return yScale(d[2]) + 3; })
  .attr('class', 'node-label')
  .attr('text-anchor', 'middle')
  .text(function(a, i) { return "n" + a[0]; });
                     

// edge label
function findMiddleEdge(node1, node2) {
  x = (node2[1] + node1[1]) / 2;
  y = (node2[2] + node1[2]) / 2;

  return [x, y];
}

let objects = [
// [1, 1, 0.5],
  [2, 2, 0.5],
  [3, 2, 0.6],
  [4, 3, 0.5]
]

let objectsEnter = svg.selectAll('objects')
  .data(objects)
  .enter()

objectsEnter
  .append("circle")
  .attr('cx', function(d){ return xScale(findLocation(d[1], d[2])[0]); })
  .attr('cy', function(d){ return yScale(findLocation(d[1], d[2])[1]); })
  .attr("r", 8)
  .attr("class", "objects")

objectsEnter.append("text")
  .attr('dx', function(d){ return xScale(findLocation(d[1], d[2])[0]); })
  .attr('dy', function(d){ return yScale(findLocation(d[1], d[2])[1]) + 3; })
  .attr('text-anchor', 'middle')
  .attr('class', 'object-label')
.append("tspan")
  .text(function(d) { return "O"; })
.append("tspan")
  .attr("baseline-shift", "sub")
  .text(function(d) { return d[0]; });

// query point
function getNode(i) {
  return nodes[i].map(x => x * 10);
}

// turning point
function getEdge(edges, edgeId) {
  for (let i = 0; i < edges.length; i++) {
    if (edges[i][0] == edgeId)
      return edges[i]
  }

  return null
}

function getNode(nodes, nodeId) {
  for (let i = 0; i < nodes.length; i++) {
    if (nodes[i][0] == nodeId)
      return nodes[i]
  }

  return null
}

function getEdgeLength(edgeId) {
  let edge = getEdge(edges, edgeId)
  let nodeSId = edge[1]
  let nodeEId = edge[2]
  let nodeS = getNode(nodes, nodeSId)
  let nodeE = getNode(nodes, nodeEId)

  let startX = nodeS[1]
  let startY = nodeS[2]

  let selisihX = nodeE[1] - nodeS[1]
  let selisihY = nodeE[2] - nodeS[2]

  return Math.sqrt(selisihX*selisihX + selisihY*selisihY)
}

function findTPCoordinatesByNodeId(nodeSId, nodeEId, TP) {
  let nodeS = getNode(nodes, nodeSId)
  let nodeE = getNode(nodes, nodeEId)

  let startX = nodeS[1]
  let startY = nodeS[2]

  let selisihX = nodeE[1] - nodeS[1]
  let selisihY = nodeE[2] - nodeS[2]

  let length = Math.sqrt(selisihX*selisihX + selisihY*selisihY)

  return TP.map(function(tp) {
    let ratio = tp / length
    
    let posX = startX + selisihX * ratio
    let x = xScale(posX) - 2
    
    let posY = startY + selisihY * ratio
    let y = yScale(posY) - 2
    
    return {
      x: x,
      y: y
    }
  })
}

function findTPCoordinatesByEdgeId(edgeId, TP) {
  let edge = getEdge(edges, edgeId)
  let nodeSId = edge[1]
  let nodeEId = edge[2]

  return findTPCoordinatesByNodeId(nodeSId, nodeEId, TP)
}

function addTurningPointMarker(edgeId, TP) {
  let tps = findTPCoordinatesByEdgeId(edgeId, TP)

  let turningPointEnter = svg.selectAll('turning-point')
    .data(tps)
    .enter()

  turningPointEnter.append('rect')
    .attr('x', d => d.x)
    .attr('y', d => d.y)
    .attr('width', 5)
    .attr('height', 5)
    .attr('fill', 'blue')
    .attr('class', 'turning-point turning-point-' + edgeId)
}

function insertToGrid(SP) {
  let edgeId = SP.edge
  let SPs = SP.SP
  let TP = SP.TP

  addTurningPointMarker(edgeId, TP)
}

function removeEdgeData(edgeId) {
  svg.selectAll('.turning-point-' + edgeId).remove()
}

var SP1 = {
  "edge": 8,
  "SP": [{
    "s": 0.0,
    "e": 1.1,
    "sp": [1, 2, 3]
  }, {
    "s": 1.1,
    "e": 2.1,
    "sp": [1, 2]
  }, {
    "s": 2.1,
    "e": 4.1,
    "sp": [4]
  }],
  "TP": [1.1, 4.1]
  }

var SP2 = {
  "edge": 6,
  "SP": [{
    "s": 0.0,
    "e": 2.1,
    "sp": [4]
  }, {
    "s": 2.1,
    "e": 3.2,
    "sp": [1]
  }, {
    "s": 2.1,
    "e": 5.1,
    "sp": [1, 2]
  }, {
    "s": 5.1,
    "e": 7,
    "sp": [1, 2, 3]
  }],
  "TP": [2.1, 3.2, 5.1]
}


var listOfSP = [
  SP1,
  SP2
]

// insertToGrid(SP1)
// insertToGrid(SP2)

// TRAVERSE OBJECT

function getSP(listOfSP, edgeId) {
  let SP = listOfSP.filter(sp => sp.edge == edgeId)

  if (SP) {
    return SP
  } else {
    null
  }
}

function getNodeLength(nodeSId, nodeEId) {
  let nodeS = getNode(nodes, nodeSId)
  let nodeE = getNode(nodes, nodeEId)

  let startX = nodeS[1]
  let startY = nodeS[2]

  let selisihX = nodeE[1] - nodeS[1]
  let selisihY = nodeE[2] - nodeS[2]

  let length = Math.sqrt(selisihX*selisihX + selisihY*selisihY)

  return length
}

function getEdgeByNodeId(nodeSId, nodeEId) {
  return edges.find(e => {
    return (
      (e[1] == nodeSId && e[2] == nodeEId) ||
      (e[2] == nodeSId && e[1] == nodeEId)
    );
  });
}

svg.append("circle")
  .attr("cx", 10)
  .attr("cy", 10)
  .attr("r", 5)
  .attr('id', 'query');

function traverseQueryPoint(selector, nodeIds) {
  let node = getNode(nodes, nodeIds[0])

  let svgq = svg.select(selector)
      .attr('cx', xScale(node[1]))
      .attr('cy', yScale(node[2]))

  let SPs = []
  let TPs = []
  let reverses = []

  for (let i = 1; i < nodeIds.length; i++) {
    let nodeId = nodeIds[i]
    let nodeIdPrev = nodeIds[i-1]
    let node = getNode(nodes, nodeId)
    let nodePrev = getNode(nodes, nodeIdPrev)
    let length = getNodeLength(nodeIdPrev, nodeId)

    let nodePrevID = nodePrev[0]
    let nodePrevX = nodePrev[1]
    let nodePrevY = nodePrev[2]

    let nodeNextID = node[0]
    let nodeNextX = node[1]
    let nodeNextY = node[2]

    let edge = getEdgeByNodeId(nodeIdPrev, nodeId)
    let edgeId = edge[0]

    let tick = 0
    let currentIndex = i

    svgq = svgq.transition()
      .ease(d3.easeLinear)
      .duration(length * 50)
      .attr('cx', xScale(node[1]))
      .attr('cy', yScale(node[2]))
      .tween("side-effects", function() {
        return function() {
          if (tick % 50 == 0) {
            let realX = d3.select(this).attr('cx')
            let realY = d3.select(this).attr('cy')

            let currentX = xScaleReverse(realX)
            let currentY = yScaleReverse(realY)

            let diffX = Math.abs(currentX) - Math.abs(nodePrevX)
            let diffY = Math.abs(currentY) - Math.abs(nodePrevY)

            let deltaX = Math.abs(nodeNextX) - Math.abs(nodePrevX)
            let deltaY = Math.abs(nodeNextY) - Math.abs(nodePrevY)

            let diffXY = Math.sqrt(diffX * diffX + diffY * diffY)
            let deltaXY = Math.sqrt(deltaX * deltaX + deltaY * deltaY)

            let currentPosition = diffXY / deltaXY

            let position = 0
            if (nodePrevID == edge[1]) {
              position = currentPosition
            } else {
              position = 1 - currentPosition
            }

            let realLength = length * position

            let maybeSP = getSP(listOfSP, edgeId)

            let isSPFound = false
            if (maybeSP.length > 0) {
              let SP = maybeSP[0].SP

              for (let i = 0; i < SP.length; i++) {
                let dStart = SP[i].s
                let dEnd = SP[i].e

                if (dStart < realLength && realLength < dEnd) {
                  isSPFound = true
                  console.log(SP[i].sp)
                  break
                }
              }

              if (!isSPFound) {
                console.log([])
              }
            } else {
              console.log([])
            }
          }

          tick++
        }
      })
  }

  return svgq
}

let toTraverse = [20, 19, 18, 17, 16, 14, 11]



function getData() {
  $.ajax({
    url: 'http://localhost:8080/data',
  })
  .done(function(result) {
    let data = result.data
    listOfSP = result.data

    for (var i = 0 ; i < data.length; i++) {
      // console.log("update edge " + data[i].edge)
      removeEdgeData(data[i].edge)
      insertToGrid(data[i])
    }
  })
  .fail(function() {
    console.log('failed')
  })

  setTimeout(function(){
    getData()
  }, 500);
}


function getObjects() {
  $.ajax({
    url: 'http://localhost:8080/objects',
  })
  .done(function(result) {
    let objects = result.data

    svg.selectAll('.objects')
      .remove()

    svg.selectAll('.object-label')
      .remove()

    let objectsEnter = svg.selectAll('objects')
      .data(objects)
      .enter()

    objectsEnter
      .append("circle")
      .attr('cx', function(d){ return xScale(findLocation(d[1], d[2])[0]); })
      .attr('cy', function(d){ return yScale(findLocation(d[1], d[2])[1]); })
      .attr("r", 8)
      .attr("class", "objects")

    objectsEnter.append("text")
      .attr('dx', function(d){ return xScale(findLocation(d[1], d[2])[0]); })
      .attr('dy', function(d){ return yScale(findLocation(d[1], d[2])[1]) + 3; })
      .attr('text-anchor', 'middle')
      .attr('class', 'object-label')
      .append("tspan")
        .text(function(d) { return "O"; })
      .append("tspan")
        .attr("baseline-shift", "sub")
        .text(function(d) { return d[0]; });
  })
  .fail(function() {
    console.log('failed')
  })

  setTimeout(function(){
    getObjects()
  }, 500);
}

getObjects()

getData()

function findSP() {
  let srcID = document.getElementById('src').value
  let dstID = document.getElementById('dst').value

  let data = {
    src: srcID,
    dst: dstID
  }

  $.ajax({
    url: 'http://localhost:8080/getPath?src=' + srcID + '&dst='+ dstID,
  })
  .done(function(path) {
    traverseQueryPoint('#query', path)
  })
  .fail(function() {
    console.log('failed')
  })
}