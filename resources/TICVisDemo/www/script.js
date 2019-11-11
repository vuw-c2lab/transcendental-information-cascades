function getAllIndexes(arr, val) {
    var indexes = [], i;
    for(i = 0; i < arr.length; i++)
        if (arr[i] === val)
            indexes.push(i);
    return indexes;
}

function removeNode(node) {
  let { nodes, links } = Graph.graphData();
  links = links.filter(l => l.source.id !== node && l.target.id !== node); // Remove links attached to node
  tmp = links.filter(l => l.source === node || l.target === node);
  console.log(node);
  console.log(links);
  nodes.splice(node.id, 1); // Remove node
  nodes.forEach((n, idx) => { n.id = idx; }); // Reset node ids to array index
  Graph.graphData({ nodes, links });
}

Shiny.addCustomMessageHandler("replace-labels", function(nodes_links) {
  var pres = document.getElementsByTagName("pre");
  //console.log(nodes_links[0]);
  var stateMapA = {};
  
  for (var i=0; i<pres.length; i++) {
    var str = pres[i].innerHTML;
    str = str.substring(0, str.length - 3);
    var nextIds = str.split(" → ");
    
    if(nextIds.length == 2){
      
      if (typeof stateMapA[nodes_links[0].names.indexOf(nextIds[0]) + "_" + nodes_links[0].names.indexOf(nextIds[1])] == 'undefined'){
        stateMapA[nodes_links[0].names.indexOf(nextIds[0]) + "_" + nodes_links[0].names.indexOf(nextIds[1])] = {cnt: 0};
      }
      //console.log(stateMapA);
      var hits = 0;
      
      for(var m=0;m<nodes_links[1].source.length;m++){
        if(nodes_links[0].names.indexOf(nextIds[0]) == nodes_links[1].source[m] && nodes_links[0].names.indexOf(nextIds[1]) == nodes_links[1].target[m]){
          if(hits == stateMapA[nodes_links[0].names.indexOf(nextIds[0]) + "_" + nodes_links[0].names.indexOf(nextIds[1])].cnt){
            theToken = nodes_links[1].token[m];
            pres[i].innerHTML = theToken;
            stateMapA[nodes_links[0].names.indexOf(nextIds[0]) + "_" + nodes_links[0].names.indexOf(nextIds[1])].cnt = stateMapA[nodes_links[0].names.indexOf(nextIds[0]) + "_" + nodes_links[0].names.indexOf(nextIds[1])].cnt + 1;
            break;
          } else {
            hits++;
          }
        }
      }
    }
  }
});

Shiny.addCustomMessageHandler("update-graph", function(nodes_links) {
  //Graph.jsonUrl('graph.json');
  
  if(nodes_links[0].node_id !== undefined){
    //console.log(nodes_links[0].node_id);
    removeNode(nodes_links[0].node_id);
  }
});