#!/usr/bin/python

import matplotlib.pyplot as pl
import numpy as np

spans = np.genfromtxt('spans')[:100]
dts = np.genfromtxt('points')

pl.subplot(211)
pl.xlabel(r'Point #')
pl.ylabel(r'Rate')
pl.plot(dts[:,0], 1./dts[:,1], '+')
for (a,b) in spans:
        pl.axvspan(a, b, color='g', alpha=0.3)
pl.xlim(xmax=np.max(dts[:,0]))

pl.subplot(212)
pl.semilogy(dts[:,2], '+')
pl.axhline(2, color='g')
pl.ylabel(r'$\beta$')

pl.show()
