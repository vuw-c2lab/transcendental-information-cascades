    const Graph = ForceGraph3D({ controlType: 'orbit' })
      (document.getElementById('force'))
        .jsonUrl('graph.json')
        .dagMode('lr')
        .dagLevelDistance(100)
        .cameraPosition({ x: -8000, y: -2000, z: 10000 })
        .linkDirectionalArrowLength(1.5)
        .linkDirectionalArrowRelPos(1)
        .linkCurvature(0.02)
        .linkColor('#ffffff')
        .linkOpacity(0.3)
        .linkDirectionalParticles(2)
        .linkDirectionalParticleWidth(0.9)
        .linkDirectionalParticleSpeed(0.006)
        .onNodeDragEnd(node => {
          node.fx = node.x;
          node.fy = node.y;
          node.fz = node.z;
        })
        .nodeLabel('group')
        .onNodeClick(node => {
          // Aim at node from outside it
          const distance = 40;
          const distRatio = 1 + distance/Math.hypot(node.x, node.y, node.z);
          Graph.cameraPosition(
            { x: node.x * distRatio, y: node.y * distRatio, z: node.z * distRatio }, // new position
            node, // lookAt ({ x, y, z })
            3000  // ms transition duration
          );
        })
        .linkThreeObjectExtend(true)
        .linkThreeObject(link => {
          // extend link with text sprite
          //const sprite = new SpriteText(`${link.source} > ${link.target}`);
          
          const sprite = new SpriteText(`${link.value}`);
          sprite.color = '#ffffff';
          sprite.textHeight = 4;
          return sprite;
        })
        .linkPositionUpdate((sprite, { start, end }) => {
          const middlePos = Object.assign(...['x', 'y', 'z'].map(c => ({
            [c]: start[c] + (end[c] - start[c]) / 2 // calc middle point
          })));
          // Position sprite
          Object.assign(sprite.position, middlePos);
        });
    // Spread nodes a little wider
    Graph.d3Force('charge').strength(-5000);
    
    Graph.backgroundColor('#0a0f0f');
    //Graph.backgroundColor('#ffffff');
    Graph.showNavInfo(false);
    Graph.width(1050);
    Graph.height(1050);
    //Graph.d3Force('link').distance(100);