    const Graph = ForceGraph3D({ controlType: 'orbit' })
      (document.getElementById('force'))
        .jsonUrl('graph.json')
        .linkDirectionalArrowLength(3.5)
        .linkDirectionalArrowRelPos(1)
        .linkCurvature(0.25)
        .nodeLabel('id')
        .nodeAutoColorBy('group')
        .linkThreeObjectExtend(true)
        .linkThreeObject(link => {
          // extend link with text sprite
          //const sprite = new SpriteText(`${link.source} > ${link.target}`);
          
          const sprite = new SpriteText(`${link.value}`);
          sprite.color = 'lightgrey';
          sprite.textHeight = 1.5;
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
    Graph.d3Force('charge').strength(-120);