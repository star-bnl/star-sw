import uproot
import numpy as np

def getVarNames(varList='QA_variable.list'):
    with open(varList, 'r') as file_:
        qaList = file_.read().rstrip('\n').split('\n')
    return [var.rstrip(' ') for var in qaList]

def getNamesAllTProfile(filename):
    file_ = uproot.open(filename)
    TProfiles = []
    for key, obj in file_.items():
        if obj.classname == 'TProfile':
             TProfiles.append(key)
    return TProfiles
       

def readFromROOT(filename, varNames, runIDTranslate=None, legacy=False):
    file_ = uproot.open(filename)
    runs = None
    x_all = []
    x_err_all = []
    counts = []

    for var in varNames:
        hist = file_[var]
        if hist.classname != 'TProfile':
            raise Exception(var + ' is not a TProfile. It is a %s. Please convert it to TProfile' % hist.classname)
        runs = np.floor(hist.axis(0).centers()).astype(int)
        x = hist.values()
        x_err = hist.errors()#error_mode="s")
        x_counts = hist.counts(False)

        if legacy:
            # round to 6 sig fig
            # because that's the default precision of cout
            # therefore that's the precision of of run-by-run v2
            def signif(x, p):
                x = np.asarray(x)
                x_positive = np.where(np.isfinite(x) & (x != 0), np.abs(x), 10**(p-1))
                mags = 10 ** (p - 1 - np.floor(np.log10(x_positive)))
                return np.round(x * mags) / mags
            x = signif(x, 6)
            x_err = signif(x_err, 6)

        x_all.append(x)
        x_err_all.append(x_err)
        counts.append(x_counts)

    x_all = np.array(x_all).T
    x_err_all = np.array(x_err_all).T
    counts = np.array(counts).T
    if runIDTranslate is not None:
        with open(runIDTranslate) as f:
            runsTrans = np.array([int(line) for line in f])
            if runsTrans.shape[0] != runs.shape[0]:
                raise RuntimeError('Number of runs in mapping file disagrees with that from TProfile')
            idSort = np.argsort(runsTrans)
            runs = runsTrans[idSort]
            x_all = x_all[idSort]
            x_err_all = x_err_all[idSort]
            counts = counts[idSort]

    id = np.all(counts > 0, axis=1)# & np.all(x_err_all > 0, axis=1)
    x_all = x_all[id]
    x_err_all = x_err_all[id]
    counts = counts[id]
    runs = runs[id]

    return runs, x_all, x_err_all, counts


if __name__ == '__main__':
    qaList = getVarNames()
    print(qaList)
    print(readFromROOT('qahist.root', qaList))


    

