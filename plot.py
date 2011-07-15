#!/usr/bin/python

import matplotlib.pyplot as pl
import numpy as np
from photon_tools.bin_photons import bin_photons

jiffy = 1/128e6 # seconds
width = 1e-3 # seconds

spans = np.genfromtxt('spans')
points = np.genfromtxt('points')
ts = np.fromfile('2011-07-04-run_001.strobe1.times', dtype='u8')
dts = ts[1:] - ts[:-1]
bins = bin_photons(ts, width / jiffy)

pl.subplot(211)
pl.xlabel(r'Time (s)')
pl.ylabel(r'Rate')
pl.plot(jiffy*bins['start_t'], bins['count']/width, '+')
for (a,b) in spans:
        pl.axvspan(jiffy*ts[a], jiffy*ts[b], color='g', alpha=0.3)
pl.xlim(jiffy*ts[0], jiffy*ts[0] + 2)

pl.subplot(212)
pl.semilogy(jiffy*ts[:len(points)], points[:,2], '+')
pl.axhline(2, color='g')
pl.ylabel(r'$\beta$')
pl.xlim(jiffy*ts[0], jiffy*ts[0] + 2)

pl.show()
