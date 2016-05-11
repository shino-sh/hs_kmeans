#! /usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import plotly.plotly as py
from plotly.graph_objs import *

vectors = {}
for line in sys.stdin.readlines():
    vx, vy, label = line.strip().split()
    vectors.setdefault(label, [])
    vectors[label].append([float(vx), float(vy)])

trace = []
for key in sorted(vectors.keys()):
    trace.append(Scatter(
        x = [v[0] for v in vectors[key]],
        y = [v[1] for v in vectors[key]],
        mode = 'markers',
        name = 'cluster ' + key
    ))

data = Data(trace)
layout = Layout(title = 'k-means')
fig = Figure(data = data, layout = layout)
py.image.save_as(fig, filename = 'k-means.png')
