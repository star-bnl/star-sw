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
       

def readFromROOT(filename, varNames, runIDTranslate=None):
    file_ = uproot.open(filename)
    runs = None
    x_normal = []
    x_err_normal = []
    counts = []

    for var in varNames:
        hist = file_[var]
        if hist.classname != 'TProfile':
            raise Exception(var + ' is not a TProfile. It is a %s. Please convert it to TProfile' % hist.classname)
        runs = np.floor(hist.axis(0).centers()).astype(int)
        x = hist.values()
        x_err = hist.errors()#error_mode="s")
        x_counts = hist.counts(False)

        x_normal.append(x)
        x_err_normal.append(x_err)
        counts.append(x_counts)

    x_normal = np.array(x_normal).T
    x_err_normal = np.array(x_err_normal).T
    counts = np.array(counts).T
    if runIDTranslate is not None:
        with open(runIDTranslate) as f:
            runsTrans = np.array([int(line) for line in f])
            if runsTrans.shape[0] != runs.shape[0]:
                raise RuntimeError('Number of runs in mapping file disagrees with that from TProfile')
            idSort = np.argsort(runsTrans)
            runs = runsTrans[idSort]
            x_normal = x_normal[idSort]
            x_err_normal = x_err_normal[idSort]
            counts = counts[idSort]

    id = np.all(counts > 0, axis=1) & np.all(x_err_normal > 0, axis=1)
    x_normal = x_normal[id]
    x_err_normal = x_err_normal[id]
    counts = counts[id]
    runs = runs[id]

    std = x_normal.std(axis=0)
    x_mean = x_normal.mean(axis=0)
    x_normal = (x_normal - x_normal.mean(axis=0)) / std
    x_err_normal = x_err_normal/std
    return runs, x_normal, x_err_normal, x_mean, std, counts


if __name__ == '__main__':
    qaList = getVarNames()
    print(qaList)
    print(readFromROOT('qahist.root', qaList))


    

