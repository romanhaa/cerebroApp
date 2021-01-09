const layout = {
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
  zaxis: {
    autorange: true,
    mirror: true,
    showline: true,
    zeroline: false
  },
  hoverlabel: {
    font: {
      size: 11
    },
    align: 'left'
  }
};

const defaultParams = {
  meta: {
    color_type: '',
    traces: [],
    color_variable: ''
  },
  data: {
    x: [],
    y: [],
    z: [],
    color: [],
    size: '',
    opacity: '',
    line: {},
    x_range: [],
    y_range: []
  },
  hover: {
    hoverinfo: '',
    text: []
  },
  group_centers: {
    group: [],
    x: [],
    y: [],
    z: []
  }
}

shinyjs.updatePlot2DContinuous = function(params) {
  params = shinyjs.getParams(params, defaultParams);
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
        colorscale: "YlGnBu",
        reversescale: true,
        colorbar: {
          title: {
            text: params.meta.color_variable
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  let layout_here = Object.assign(layout);
  layout_here.xaxis["autorange"] = false;
  layout_here.xaxis["range"] = params.data.x_range;
  layout_here.yaxis["autorange"] = false;
  layout_here.yaxis["range"] = params.data.y_range;
  Plotly.react('overview_projection', data, layout_here);
}

shinyjs.updatePlot3DContinuous = function(params) {
  params = shinyjs.getParams(params, defaultParams);
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
        colorscale: "YlGnBu",
        reversescale: true,
        colorbar: {
          title: {
            text: params.meta.color_variable
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  Plotly.react('overview_projection', data, layout);
}


shinyjs.updatePlot2DCategorical = function(params) {
  params = shinyjs.getParams(params, defaultParams);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    data.push(
      {
        x: params.data.x[i],
        y: params.data.y[i],
        name: params.meta.traces[i],
        mode: 'markers',
        type: 'scattergl',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[i]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text[i],
        hoverlabel: {
          bgcolor: params.data.color[i]
        },
        showlegend: true
      }
    );
  }
  if (params.group_centers.group.length >= 1) {
    data.push(
      {
        x: params.group_centers.x,
        y: params.group_centers.y,
        text: params.group_centers.group,
        type: 'scattergl',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: {
          color: '#000000',
          size: 16
        },
        hoverinfo: 'skip',
        inherit: false
      }
    );
  }
  let layout_here = Object.assign(layout);
  layout_here.xaxis["autorange"] = false;
  layout_here.xaxis["range"] = params.data.x_range;
  layout_here.yaxis["autorange"] = false;
  layout_here.yaxis["range"] = params.data.y_range;
  Plotly.react('overview_projection', data, layout_here);
}


shinyjs.updatePlot3DCategorical = function(params) {
  params = shinyjs.getParams(params, defaultParams);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    data.push(
      {
        x: params.data.x[i],
        y: params.data.y[i],
        z: params.data.z[i],
        name: params.meta.traces[i],
        mode: 'markers',
        type: 'scatter3d',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[i]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text[i],
        hoverlabel: {
          bgcolor: params.data.color[i]
        },
        showlegend: true
      }
    );
  }
  if (params.group_centers.group.length >= 1) {
    data.push(
      {
        x: params.group_centers.x,
        y: params.group_centers.y,
        z: params.group_centers.z,
        text: params.group_centers.group,
        type: 'scatter3d',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: {
          color: '#000000',
          size: 16
        },
        hoverinfo: 'skip',
        inherit: false
      }
    );
  }
  Plotly.react('overview_projection', data, layout);
}

