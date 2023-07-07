from functools import partial
import numpy as np

def weightedMean(values, weights=None):
    tmp = values*weights
    return np.sum(tmp, axis=0)/np.sum(weights, axis=0)

def weightedStd(values, weights=None):
    mean = weightedMean(values, weights)
    variance = np.average((values - mean)**2, weights=weights, axis=0)
    return np.sqrt(variance)

def weightedMedian(values, weights=None):
    if weights is None:
        return np.median(values, axis=0)
    iarr = np.argsort(values, axis=0)
    carr = np.cumsum(weights, axis=0)
    med = []
    for i, c, v in zip(iarr.T, carr.T, values.T):
        idm = i[np.searchsorted(c, 0.5*c[-1])]
        med.append(v[idm])
    return np.array(med)

def MAD(values, weights=None):
    median = weightedMedian(values, weights)
    absDev = np.abs(values - median)
    return weightedMedian(absDev, weights)/0.683

def greater(values, mean, stdRange, std, uncert):
    return np.abs(values - mean) > stdRange*std + uncert

def greaterlegacy(values, mean, stdRange, std, uncert):
    # how v2 estimate distance
    return np.square(values - mean) > stdRange*stdRange*(std*std + uncert*uncert)

def meanAndStdSequencial(values, weights, uncert, stdRange, returnMean=True):
    # the runs rejected by previous observables won't count in the calculation of mean and std in the next observable
    # result depends on the order of observables, which is not good
    # needed to simulate the behavior of v2
    weights = np.copy(weights)
    ans = []
    for i in range(values.shape[1]):
        mean = weightedMean(values[:, i], weights[:, i])
        std = weightedStd(values[:, i], weights[:, i])
        id = greaterlegacy(values[:, i], mean, stdRange, std, uncert[:, i])
        # ignore comparison results if weight is 0 to begin with
        id[weights[:, i] == 0] = False
        if i + 1 < values.shape[1]:
            weights[id, i + 1:] = 0
        ans.append(mean if returnMean else std)
    return np.array(ans)


def outlierSegment(runs, values, uncert, stdRange=3, weights=None, meanAlg=weightedMean, stdAlg=weightedStd, greater=greater):
    mean = meanAlg(values, weights)
    std = stdAlg(values, weights)
    idRejectedReason = greater(values, mean, stdRange, std, uncert)
    # if weight is zero, it's not being compared
    idRejectedReason = idRejectedReason & (weights > 0)
    idRejected = np.any(idRejectedReason, axis=1)
    return runs[idRejected], idRejectedReason[idRejected], mean, stdRange*std


def outlierDetector(runs, values, uncert, idSegments, useMAD, weights, legacy=False, seqRej=False, quadRange=False, **kwargs):
    runsRejected = np.array([])
    idRejected = []
    stdRange = []
    mean = []
    if useMAD:
        meanAlg = weightedMedian
        stdAlg = MAD
    else:
        meanAlg = weightedMean
        stdAlg = weightedStd

    if quadRange:
        gt = greaterlegacy
    else:
        gt = greater

    for idEdge, (lowEdge, upEdge) in enumerate(zip([0] + idSegments, idSegments + [runs.shape[0]])):
        if lowEdge == upEdge:
            meanSeg = np.zeros(values.shape[1])
            stdRangeSet = np.zeros(values.shape[1])
            runsRejectedSeg = np.array([])
        else:
            if seqRej:
                sr = kwargs['stdRange']
                meanAlg = partial(meanAndStdSequencial, uncert=uncert[lowEdge:upEdge], stdRange=sr, returnMean=True)
                stdAlg = partial(meanAndStdSequencial, uncert=uncert[lowEdge:upEdge], stdRange=sr, returnMean=False)

            runsRejectedSeg, idRejectedSeg, meanSeg, stdRangeSeg = outlierSegment(runs[lowEdge:upEdge], values[lowEdge:upEdge], 
                                                                                  uncert[lowEdge:upEdge], weights=weights[lowEdge:upEdge], 
                                                                                  meanAlg=meanAlg, stdAlg=stdAlg, greater=gt, **kwargs)
        stdRange.append(stdRangeSeg)
        mean.append(meanSeg)
        if runsRejectedSeg.shape[0] > 0:
            runsRejected = np.append(runsRejected, runsRejectedSeg)
            idRejected.append(idRejectedSeg)
    if runsRejected.shape[0] > 0:
        idRejected = np.vstack(idRejected)
    return runsRejected, idRejected, np.vstack(mean), np.vstack(stdRange)


if __name__ == '__main__':
    runs = np.arange(0, 3e3)
    values = np.array([[1,1]]*runs.shape[0])
    uncert = np.zeros(values.shape)
    print(outlierDetector(runs, values, uncert, [0, 1000, 2000, runs.shape[0]-1]))
    values[1, 1] = 1e8
    values[1000, 0] = 1e8
    print(outlierDetector(runs, values, uncert, [0, 1000, 2000, runs.shape[0]-1]))
    print(outlierDetector(runs, values, uncert, [0, 2000, runs.shape[0]-1]))


