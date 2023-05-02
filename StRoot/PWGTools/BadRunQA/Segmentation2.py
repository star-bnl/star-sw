# Segmentation script (change point detection) developed by Kent-BNL group for BES-II data
# author (c) C.Y. Tsang, Y. Hu, P. Tribedy  July 7, 2022
# last edit on July 7, 2022
import matplotlib.pyplot as plt
import ruptures as rpt
import pandas as pd
import numpy as np
import sys
import argparse

def readData(filename='runInfo14.txt'):
    runs = pd.read_table(filename, sep=' ')
    # normalize and recenter data
    errKeyWord = '_Err'
    x_err = runs.filter(like=errKeyWord)

    featuresErr = x_err.columns
    features = [name.replace(errKeyWord, '') for name in featuresErr]

    print('Features [' + ', '.join(features) + '] are read.')

    x = runs[features].values
    x_normal = (x - x.mean(axis=0)) / x.std(axis=0)
    x_err = x_err.values
    x_err_normal = x_err/x.std(axis=0)

    return runs, x_normal, x_err_normal, features

def kernseg(X, n_bkps_max, min_size):
    from scipy.special import betaln
    from sklearn.linear_model import LinearRegression

    segmentations_values_tot = []
    adjusted_costs_tot = []

    algo = rpt.KernelCPD(kernel="rbf", min_size=min_size)
    algo.fit(X).predict(n_bkps=n_bkps_max)

    segmentations_values = [[len(X)]] + list(algo.segmentations_dict.values())
    costs = [algo.cost.sum_of_costs(est) for est in segmentations_values]

    # https://stackoverflow.com/questions/21767690/python-log-n-choose-k
    log_nchoosek = [
        -betaln(1 + n_bkps_max - k, 1 + k) - np.log(n_bkps_max + 1)
        for k in range(0, n_bkps_max + 1)
    ]
    X_lm = np.array([log_nchoosek, range(0, n_bkps_max + 1)]).T
    lm = LinearRegression().fit(
        X_lm[int(0.6 * n_bkps_max) :, :], costs[int(0.6 * n_bkps_max) :]
    )
    adjusted_costs = costs - 2 * X_lm.dot(lm.coef_)

    return segmentations_values[np.argmin(adjusted_costs)]

def segmentation(pen, min_size, signal, gamma, useNormal=False, useJMLR=False):
    # use fancy change point calculation technique
    #c = rpt.costs.CostRbf(gamma=2).fit(signal)
    #algo = rpt.Pelt(custom_cost=c, min_size=min_size, jump=1)
    if useJMLR:
        # determine pen with heuristic from https://github.com/deepcharles/ruptures/issues/223
        return kernseg(signal, int(0.5*signal.shape[0]/min_size), min_size)
    if useNormal:
        model = 'normal'
        kwarg = {}
        pen = np.log(signal.shape[0])*signal.shape[1]
    else:
        model = 'rbf'
        kwarg={'gamma': gamma}
        pen = pen
    algo = rpt.Pelt(model=model, min_size=min_size, jump=1, params=kwarg).fit(signal)
    return algo.predict(pen=pen)

def getWeightedMeanStd(data, err, useAveErr):
    ave = np.average(data, axis=0, weights=np.reciprocal(err*err))
    if useAveErr:
        std = np.average(err, axis=0)
    else:
        std = np.sqrt(np.average((data - ave)**2, weights=np.reciprocal(err*err), axis=0)/np.sqrt(data.shape[0]))
    return ave, std

def mergeID(signal, signal_err, result, stdRange=5, useAveErr=False, minSize=0):
    # merge segments according to Yu's suggestions
    means, stds = [], []
    for idStart, idEnd in zip([0] + result[:-1], result):
        m, e = getWeightedMeanStd(signal[idStart:idEnd], signal_err[idStart:idEnd], useAveErr)
        means.append(m)
        stds.append(e)
    merged = [0]
    for i, (id_, m1, m2, e1, e2, idStart, idEnd) in enumerate(zip(result, means[:-1], means[1:], stds[:-1], stds[1:], [0] + result[:-1], result)):
        dist = 0.5*np.sqrt(e1*e1 + e2*e2)
        if not np.all(np.less(np.fabs(m1 - m2), stdRange*dist)):
            merged.append(id_)
        else:
            means[i], stds[i] = getWeightedMeanStd(signal[merged[-1]:idEnd], signal_err[merged[-1]:idEnd], useAveErr)
    # include the end point for segmentation array for consistency with rpt predictions
    merged.append(result[-1])
    return merged[1:]

def plotSegmentationAndRejection(signal, segmentArray, features, idNum=None): 
    fig, axes = rpt.display(signal, segmentArray)
    for ax, title in zip(axes, features):
        ax.set_ylabel(title)
    # idNum is numerical array of GOOD events
    # it calculates bad id automatically
    if idNum is not None:
        badId = badIdFromGoodId(signal.shape[0], idNum)
        for ax, value in zip(axes, signal.T):
            ax.scatter(badId, value[badId], color='r')
    return fig, axes



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Use hybrid ML technique to detect outliers. Rapture is used for data segmentation and distance to mean value on each segment is used as conditions to reject outliers', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-i', '--input', help='Filename to run data', required=True)
    parser.add_argument('-p', '--pen', type=float, help='Penality to segmentation (lower value creates more segmentations).', default=2)
    parser.add_argument('-m', '--minSize', type=int, help='Minimum number of events required for a segment.', default=10)
    parser.add_argument('-mr', '--mergeRange', type=float, help='Standard deviation range to merge results from rapture. 0 means no segment merging takes place.', default=3.5)
    parser.add_argument('-o', '--output', help='File to which rejected runs are saved.', required=True)
    parser.add_argument('--plotResult', help='Name of the image file to which rejected points and segmentation results are being drawn.')
    parser.add_argument('-d', '--display', action='store_true', help='Show input parameters.')
    parser.add_argument('-ae', action='store_true', help='Use average error instead of weighted STD for segmentation merging.')
    parser.add_argument('-g', '--gamma', type=float, help='characteristic length for kernel.', default=None)
    parser.add_argument('-n', action='store_true', help='Will assume the distribution follows piece-wise Gaussian. Enabling it will disable -s, -g and -pen flag.')
    parser.add_argument('--JMLR', action='store_true', help='Determine number of change points with heristics from JMLR paper (Section 3.3.2 of https://www.jmlr.org/papers/volume20/16-155/16-155.pdf). Enabling it will disable -s, -n, -g and -pen flag.')
    parser.add_argument('-s', '--strength', choices=['1', '2', '3', '4'], help='Strength, or aggressiveness of the segmentation. 1 gives you the least number of segments and 4 gives you the most. This argument overrides --pen and --gamma.')

    args = parser.parse_args()

    if args.display:
        print('Parameters:')
        print('\n'.join(f'{k}={v}' for k, v in vars(args).items()))
        print()

    if args.strength is not None:
        if args.strength == '1':
            args.pen = 5
            args.gamma = None
        elif args.strength == '2':
            args.pen = 2
            args.gamma = None
        elif args.strength == '3':
            args.pen = 1
            args.gamma = None
        else:
            args.pen = 0.5
            args.gamma = None
        

    filename = args.input
    print('Run data is being loaded from ' + filename)
    runs, signal, signalErr, feature = readData(filename)
    result = segmentation(pen=args.pen, min_size=args.minSize, signal=signal, gamma=args.gamma, useNormal=args.n, useJMLR=args.JMLR)
    mergedResult = mergeID(signal, signalErr, result, stdRange=args.mergeRange, useAveErr=args.ae, minSize=args.minSize)

    if args.plotResult is not None:
        fig, _ = plotSegmentationAndRejection(signal, mergedResult, feature)
        plt.savefig(args.plotResult)
        plt.show()

    np.savetxt(args.output, runs['runID'].iloc[mergedResult[:-1]], fmt='%s', delimiter=' ')




