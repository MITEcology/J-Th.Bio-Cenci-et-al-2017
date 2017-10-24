import numpy as np
import pandas as pd
from rpy2.robjects import pandas2ri
pandas2ri.activate()
from rpy2.robjects import r                       
from sklearn import decomposition
from sklearn import cluster, datasets
from sklearn.neighbors import kneighbors_graph
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import MeanShift, estimate_bandwidth
from sklearn.datasets.samples_generator import make_blobs

#### Here I import a large list of matrices with nsamples row and nfeatures columns
#### Then for each matrix I compute the expected bandwidth

r['load']("Vectors.RData")
M = np.array(r['Vectors'])
f = open('Bandwidth.txt', 'w')
for i in range(0,np.shape(M)[0]):
    print np.shape(M[i])
    if np.shape(M[i])[0] < 100000:
        N_Samp = np.shape(M[i])[0]
    else:
        N_Samp = 100000
    # Here I have to convert the matrix into an array because the estimate_bandwidth take that as input
    A = np.array(M[i])
    bandwidth = estimate_bandwidth(A, quantile = 0.5, n_samples = N_Samp)
    print i, bandwidth
    f.write('%i %f\n' % (i, bandwidth))

