// layout for 2D projections in a single panel
const expression_projection_layout_2D = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4
  },
  xaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: []
  },
  yaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false,
    range: []
  },
  hoverlabel: {
    font: {
      size: 11
    },
    bgcolor: 'lightgrey',
    align: 'left'
  },
  shapes: []
};

// layout for 2D projections with multiple panels
const expression_projection_layout_2D_multi_panel = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4
  },
  hoverlabel: {
    font: {
      size: 11
    },
    bgcolor: 'lightgrey',
    align: 'left'
  },
  shapes: []
};

// layout for 3D projections
const expression_projection_layout_3D = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4
  },
  scene: {
    xaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: []
    },
    yaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: []
    },
    zaxis: {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false
    },
  },
  hoverlabel: {
    font: {
      size: 11
    },
    bgcolor: 'lightgrey',
    align: 'left'
  }
};

// default structure of input data
const expression_projection_default_params = {
  data: {
    x: [],
    y: [],
    z: [],
    color: [],
    size: '',
    opacity: '',
    line: {},
    x_range: [],
    y_range: [],
    reset_axes: false
  },
  hover: {
    hoverinfo: '',
    text: []
  },
  color: {
    scale: '',
    range: [0, 1]
  },
  trajectory: []
}

// update 2D projection with single panel
shinyjs.expressionProjectionUpdatePlot2D = function(params) {
  params = shinyjs.getParams(params, expression_projection_default_params);
  const data = [];
  data.push(
    {
      x: params.data.x,
      y: params.data.y,
      mode: 'markers',
      type: 'scattergl',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color,
        colorscale: params.color.scale,
        reversescale: true,
        cauto: false,
        cmin: params.color.range[0],
        cmax: params.color.range[1],
        colorbar: {
          title: {
            text: 'Expression',
            ticks: 'outside',
            outlinewidth: 1,
            outlinecolor: 'black'
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  const layout_here = Object.assign(expression_projection_layout_2D);
  if (params.data.reset_axes) {
    layout_here.xaxis['autorange'] = true;
    layout_here.yaxis['autorange'] = true;
  } else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  }
  layout_here.shapes = params.trajectory;
  Plotly.react('expression_projection', data, layout_here);
}

// update 2D projection with multiple panels
shinyjs.expressionProjectionUpdatePlot2DMultiPanel = function(params) {
  params = shinyjs.getParams(params, expression_projection_default_params);
  if (Array.isArray(params.data.color)) {
    return null;
  }
  const layout_here = Object.assign(expression_projection_layout_2D_multi_panel);
  layout_here.shapes = params.trajectory;
  const number_of_genes = Object.keys(params.data.color).length;
  let n_rows = 1;
  let n_cols = 1;
  if (number_of_genes == 2) {
    n_rows = 1;
    n_cols = 2;
  } else if (number_of_genes <= 4) {
    n_rows = 2;
    n_cols = 2;
  } else if (number_of_genes <= 6) {
    n_rows = 2;
    n_cols = 3;
  } else if (number_of_genes <= 9) {
    n_rows = 3;
    n_cols = 3;
  }
  layout_here.grid = {rows: n_rows, columns: n_cols, pattern: 'independent'};
  layout_here.annotations = [];
  const data = [];
  Object.keys(params.data.color).forEach(function(gene, index) {
    const x_axis = index===0 ? 'xaxis' : `xaxis${index+1}`;
    const y_axis = index===0 ? 'yaxis' : `yaxis${index+1}`;
    const x_anchor = `x${index+1}`;
    const y_anchor = `y${index+1}`;
    // create trace and add to data array
    data.push(
      {
        x: params.data.x,
        y: params.data.y,
        xaxis: x_anchor,
        yaxis: y_anchor,
        mode: 'markers',
        type: 'scattergl',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[gene],
          colorscale: params.color.scale,
          reversescale: true,
          cauto: false,
          cmin: params.color.range[0],
          cmax: params.color.range[1]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text,
        showlegend: false
      }
    );
    // add colorbar only to first trace
    if (index===0) {
      console.log('add colorbar');
      data[index].marker.colorbar = {
        title: {
          text: 'Expression',
          ticks: 'outside',
          outlinewidth: 1,
          outlinecolor: 'black'
        }
      }
    };
    console.log(data);
    // add X/Y axis attributes to layout
    layout_here[x_axis] = {
      title: gene,
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      anchor: x_anchor
    }
    layout_here[y_axis] = {
      autorange: true,
      mirror: true,
      showline: true,
      zeroline: false,
      range: [],
      anchor: y_anchor
    }
    if (params.data.reset_axes) {
      layout_here[x_axis]['autorange'] = true;
      layout_here[y_axis]['autorange'] = true;
    } else {
      layout_here[x_axis]['autorange'] = false;
      layout_here[x_axis]['range'] = params.data.x_range;
      layout_here[y_axis]['autorange'] = false;
      layout_here[y_axis]['range'] = params.data.y_range;
    }
  });
  // update plot
  Plotly.react('expression_projection', data, layout_here);
}

// update 3D projection
shinyjs.expressionProjectionUpdatePlot3D = function(params) {
  params = shinyjs.getParams(params, expression_projection_default_params);
  const data = [];
  data.push(
    {
      x: params.data.x,
      y: params.data.y,
      z: params.data.z,
      mode: 'markers',
      type: 'scatter3d',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color,
        colorscale: params.color.scale,
        reversescale: true,
        cauto: false,
        cmin: params.color.range[0],
        cmax: params.color.range[1],
        colorbar: {
          title: {
            text: 'Expression',
            ticks: 'outside',
            outlinewidth: 1,
            outlinecolor: 'black'
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  Plotly.react('expression_projection', data, expression_projection_layout_3D);
}
