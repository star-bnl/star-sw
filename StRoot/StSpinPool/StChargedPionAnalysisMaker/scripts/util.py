from bisect import bisect

def getRun(path):
    """searches for an integer runnumber in the supplied path.
    if not found, return the filename after stripping path and suffix info"""
    import re
    import os
    regex = re.search('(6|7)\d{6}',path)
    try:
        return int(regex.group())
    except AttributeError:
        return os.path.basename(path).split('.')[0]


def getFill(runnumber):
    """queries DB to get fill for this runnumber"""
    import MySQLdb
    db = MySQLdb.connect(host='star1.lns.mit.edu', port=3316, db='RunLog_onl')
    dbc = db.cursor()
    nrows = dbc.execute('select blueFillNumber from beamInfo where runNumber=%d\
        and deactive=0 order by beginTime desc limit 1' % (runnumber,))
    if nrows ==0:
        print runnumber, 'has no beamInfo record in the RunLog_onl database'
        return 0
    else:
        fill = int(dbc.fetchone()[0])
        #temporary hacks
        #http://www.star.bnl.gov/HyperNews-star/protected/get/starspin/3324.html
        if 6144002 <= runnumber <= 6144029: fill = 7128
        if 6144041 <= runnumber <= 6144042: fill = 7129
        if 6145067 <= runnumber <= 6145068: fill = 7136
        if 6146001 <= runnumber <= 6146026: fill = 7138
        return fill

def getAllFills(runlist):
    """returns tuple of (run,fill) tuples"""
    import MySQLdb
    db = MySQLdb.connect(host='star1.lns.mit.edu', port=3316, db='RunLog_onl')
    dbc = db.cursor()
    nrows = dbc.execute('select runNumber,blueFillNumber from beamInfo where \
        runNumber>=%d and runNumber<=%d and deactive=0 order by runNumber desc'\
        % (min(runlist),max(runlist)))
    return dbc.fetchall()


def getAllFills2(runlist):
    """returns a run,fill dict containing only entries from specified runs"""
    tuples = getAllFills(runlist)
    d = {}
    for run,fill in tuples:
        if run in runlist:
            d[run] = int(fill)
    return d


def fillList(runlist):
    tuples = getAllFills(runlist)
    return uniqify([fill for run,fill in tuples])


def uniqify(seq, idfun=None):
    # order-preserving method to return unique elements in list
    if idfun is None: 
        def idfun(x): return x 
    seen = {} 
    result = [] 
    for item in seq: 
        marker = idfun(item) 
        # in old Python versions: 
        # if seen.has_key(marker) 
        # but in new ones: 
        if marker in seen: continue 
        seen[marker] = 1 
        result.append(item) 
    return result

def daqEventCount(run, trigId):
    """queries RunLog Browser to get event count for this (run,trigId) pair"""
    """basically the world's stupidest HTML parser"""
    import urllib2
    sock = urllib2.urlopen(
        'http://online.star.bnl.gov/RunLogRun6/Summary.php?run=%d' % run
    )
    for line in sock:
        items = line.split()
        for row,item in enumerate(items):
            if item.find(str(trigId)) > 0:
                eventCount = items[row+4]
                return int(eventCount.lstrip('align="right">').rstrip('</TD>R'))
    
    

def lafferty_wyatt_point(lowedge, highedge, expo_slope):
    """calculates the l-w point for a bin where the true distribution is an
    exponential characterized by expo_slope.
    """
    import math
    rhs = (math.exp(expo_slope*highedge) - math.exp(expo_slope*lowedge)) 
    rhs /= expo_slope
    rhs /= (highedge - lowedge)
    return math.log(rhs) / expo_slope
    
def hadd_interactive(histDir, runlist, trig, spin, charge, key):
    import ROOT
    from glob import glob
    import os.path
    from sys import stdout
    allFiles = glob( os.path.join(histDir, "*.root") )
    if charge == None:
        keystring = "_%s_%s_%s" % (trig,spin,key)
    else:
        keystring = "_%s_%s_%s_%s" % (trig,spin,charge,key)
    stdout.write('getting %s ' % keystring)
    ## should check that the first file is in the runlist
    f = ROOT.TFile(allFiles[0])
    h = f.Get(keystring).Clone()
    h.SetDirectory(0)
    f.Close()
    for fname in allFiles[1:]:
        run = getRun(fname)
        if runlist is None or run in runlist:
            stdout.write('.')
            stdout.flush()
            tmp = ROOT.TFile(fname)
            h.Add( tmp.Get(keystring) )
            tmp.Close()
    stdout.write(' done\n\n')
    return h


def tf1(fun, rangeMin, rangeMax, **kw):
    """
    returns a TF1 after applying kwargs to the supplied function
    """
    from uuid import uuid1 as uuid
    from ROOT import TF1
    return TF1(str(uuid()), lambda x: fun(x[0], **kw), rangeMin, rangeMax)


def upload_scalers(scaler_path, db_path):
    import sqlite3
    f = open(scaler_path)
    f.readline() # skip header
    db = sqlite3.connect(db_path)
    dbc = db.cursor()
    for line in f:
        fill, run, board, timebin, uu, du, ud, dd = line.split()
        values = tuple([int(v) for v in (run,board,timebin, uu,ud,du,dd)])
        print values
        dbc.execute('INSERT INTO scalers VALUES (?,?,?, ?,?,?,?)', values)
    db.commit()
    dbc.close()

## retrieve start_run -> fill mapping from RunLog_onl by
# SELECT blueFillNumber, MIN(runNumber) FROM beamInfo 
#     WHERE runNumber>6119000 AND runNumber<8000000 AND deactive=0 
#     GROUP BY blueFillNumber;
_first_run = (
( 6119001, 6988 ),
( 6119043, 6989 ),
( 6119048, 6990 ),
( 6119074, 6991 ),
( 6120001, 6992 ),
( 6120025, 6994 ),
( 6120057, 6995 ),
( 6121001, 6997 ),
( 6121026, 6998 ),
( 6121038, 6999 ),
( 6121050, 7001 ),
( 6122004, 7002 ),
( 6122025, 7005 ),
( 6122042, 7007 ),
( 6123030, 7008 ),
( 6123033, 7010 ),
( 6125001, 7021 ),
( 6125012, 7023 ),
( 6125013, 7024 ),
( 6126001, 7028 ),
( 6126055, 7029 ),
( 6127002, 7030 ),
( 6127020, 7032 ),
( 6127049, 7034 ),
( 6128035, 7035 ),
( 6128065, 7036 ),
( 6129001, 7038 ),
( 6129002, 7039 ),
( 6129011, 7040 ),
( 6129017, 7041 ),
( 6129021, 7044 ),
( 6130002, 7046 ),
( 6130041, 7047 ),
( 6130044, 7048 ),
( 6131001, 7049 ),
( 6131064, 7051 ),
( 6131099, 7053 ),
( 6131104, 7055 ),
( 6132027, 7059 ),
( 6132030, 7060 ),
( 6132105, 7063 ),
( 6132109, 7064 ),
( 6133034, 7065 ),
( 6133039, 7067 ),
( 6134016, 7068 ),
( 6134049, 7069 ),
( 6134073, 7070 ),
( 6135016, 7072 ),
( 6135039, 7073 ),
( 6135043, 7075 ),
( 6136001, 7079 ),
( 6136044, 7081 ),
( 6136045, 7082 ),
( 6136048, 7084 ),
( 6136078, 7085 ),
( 6137059, 7086 ),
( 6137132, 7087 ),
( 6137151, 7088 ),
( 6138006, 7092 ),
( 6138021, 7093 ),
( 6138029, 7094 ),
( 6138044, 7096 ),
( 6138048, 7102 ),
( 6139014, 7103 ),
( 6139042, 7110 ),
( 6140006, 7112 ),
( 6140043, 7113 ),
( 6140047, 7114 ),
( 6141001, 7115 ),
( 6141004, 7118 ),
( 6141034, 7119 ),
( 6141040, 7120 ),
( 6141070, 7122 ),
( 6142028, 7123 ),
( 6142065, 7124 ),
( 6143004, 7125 ),
( 6143036, 7126 ),
( 6143043, 7127 ),
( 6144002, 7128 ), ## missing from RunLog_onl DB
( 6144041, 7129 ), ## missing from RunLog_onl DB
( 6144043, 7131 ),
( 6145001, 7133 ),
( 6145030, 7134 ),
( 6145067, 7136 ), ## missing from RunLog_onl DB
( 6146001, 7138 ), ## missing from RunLog_onl DB
( 6146027, 7139 ),
( 6146030, 7140 ),
( 6146033, 7142 ),
( 6146046, 7143 ),
( 6147002, 7151 ),
( 6147033, 7152 ),
( 6148002, 7153 ),
( 6148028, 7154 ),
( 6148042, 7156 ),
( 6148046, 7161 ),
( 6149010, 7162 ),
( 6149038, 7163 ),
( 6149041, 7164 ),
( 6149058, 7165 ),
( 6150007, 7166 ),
( 6150030, 7172 ),
( 6153001, 7210 ),
( 6153002, 7212 ),
( 6153003, 7213 ),
( 6153004, 7214 ),
( 6153005, 7215 ),
( 6153012, 7216 ),
( 6153013, 7218 ),
( 6153015, 7219 ),
( 6153022, 7222 ),
( 6153023, 7223 ),
( 6154018, 7224 ),
( 6154046, 7232 ),
( 6155005, 7233 ),
( 6155007, 7234 ),
( 6155014, 7237 ),
( 6156020, 7238 ),
( 6156046, 7240 ),
( 6157001, 7241 ),
( 6157028, 7246 ),
( 6157042, 7249 ),
( 6158026, 7250 ),
( 6158043, 7253 ),
( 6158066, 7255 ),
( 6159001, 7260 ),
( 6159052, 7263 ),
( 6160018, 7264 ),
( 6160032, 7265 ),
( 6160074, 7266 ),
( 6161010, 7268 ),
( 6161016, 7269 ),
( 6161060, 7270 ),
( 6162008, 7271 ),
( 6162015, 7272 ),
( 6162048, 7273 ),
( 6162052, 7274 ),
( 6163001, 7276 ),
( 6163031, 7278 ),
( 6164006, 7279 ),
( 6164029, 7280 ),
( 6165002, 7287 ),
( 6165004, 7288 ),
( 6165005, 7289 ),
( 6165006, 7291 ),
( 6165008, 7292 ),
( 6165015, 7293 ),
( 6165066, 7294 ),
( 6166035, 7295 ),
( 6166078, 7296 ),
( 6167089, 7297 ),
( 6167090, 7298 ),
( 6167097, 7299 ),
( 6167098, 7300 ),
( 6168010, 7301 ),
( 6168059, 7302 ),
( 6169009, 7303 ),
( 6169061, 7304 ),
( 6170020, 7305 ),
( 6170063, 7306 ),
( 6171014, 7308 ),
( 6171051, 7309 ),
( 6171054, 7311 ),
( 6172017, 7313 ),
( 6172028, 7315 ),
( 6172055, 7317 ),
( 6173002, 7318 ),
( 6173045, 7319 ),
( 6174001, 7320 ),
( 6174032, 7321 ),
( 6174036, 7325 ),
( 6175001, 7327 ),
( 7096047, 7718 ),
( 7097057, 7719 ),
( 7097073, 7722 ),
( 7098009, 7724 ),
( 7098044, 7725 ),
( 7099007, 7729 ),
( 7099063, 7731 ),
( 7099076, 7732 ),
( 7100001, 7738 ),
( 7100004, 7739 ),
( 7100032, 7740 ),
( 7101005, 7742 ),
( 7101007, 7744 ),
( 7101060, 7745 ),
( 7102001, 7747 ),
( 7102057, 7748 ),
( 7103001, 7753 ),
( 7103041, 7754 ),
( 7103046, 7755 ),
( 7103047, 7756 ),
( 7104006, 7757 ),
( 7104019, 7759 ),
( 7113006, 7775 ),
( 7113013, 7776 ),
( 7114005, 7777 ),
( 7114021, 7778 ),
( 7114033, 7780 ),
( 7115070, 7781 ),
( 7116001, 7782 ),
( 7116006, 7784 ),
( 7116019, 7785 ),
( 7117022, 7786 ),
( 7117038, 7788 ),
( 7118019, 7789 ),
( 7118060, 7790 ),
( 7119007, 7791 ),
( 7119042, 7792 ),
( 7119093, 7793 ),
( 7120002, 7794 ),
( 7120056, 7795 ),
( 7120102, 7796 ),
( 7121003, 7797 ),
( 7121046, 7798 ),
( 7121087, 7799 ),
( 7121098, 7800 ),
( 7122008, 7801 ),
( 7122021, 7803 ),
( 7122061, 7804 ),
( 7123001, 7805 ),
( 7123047, 7806 ),
( 7123049, 7807 ),
( 7123064, 7810 ),
( 7124035, 7811 ),
( 7124075, 7812 ),
( 7124091, 7813 ),
( 7124109, 7814 ),
( 7124115, 7815 ),
( 7125030, 7816 ),
( 7125032, 7817 ),
( 7125073, 7819 ),
( 7126002, 7820 ),
( 7126040, 7821 ),
( 7126045, 7822 ),
( 7126046, 7823 ),
( 7127015, 7824 ),
( 7127053, 7825 ),
( 7127082, 7826 ),
( 7128015, 7827 ),
( 7128033, 7828 ),
( 7128038, 7830 ),
( 7129011, 7831 ),
( 7129045, 7833 ),
( 7130002, 7838 ),
( 7131001, 7842 ),
( 7131004, 7844 ),
( 7131009, 7846 ),
( 7131015, 7847 ),
( 7132029, 7849 ),
( 7132034, 7850 ),
( 7133001, 7851 ),
( 7133027, 7852 ),
( 7133055, 7853 ),
( 7134018, 7854 ),
( 7134019, 7855 ),
( 7134058, 7856 ),
( 7135006, 7858 ),
( 7135032, 7859 ),
( 7135037, 7860 ),
( 7136009, 7861 ),
( 7136010, 7863 ),
( 7136047, 7864 ),
( 7137002, 7865 ),
( 7137017, 7866 ),
( 7137029, 7871 ),
( 7138018, 7872 ),
( 7138046, 7873 ),
( 7138049, 7874 ),
( 7138052, 7876 ),
( 7138066, 7877 ),
( 7139002, 7878 ),
( 7139003, 7882 ),
( 7139007, 7883 ),
( 7140001, 7886 ),
( 7140025, 7887 ),
( 7141001, 7889 ),
( 7141027, 7890 ),
( 7141049, 7891 ),
( 7142007, 7892 ),
( 7142038, 7893 ),
( 7142053, 7896 ),
( 7143032, 7897 ),
( 7143037, 7898 ),
( 7144003, 7901 ),
( 7144019, 7903 ),
( 7144034, 7904 ),
( 7144052, 7906 ),
( 7145002, 7908 ),
( 7145045, 7909 ),
( 7146011, 7911 ),
( 7146026, 7913 ),
( 7146083, 7915 ),
( 7147037, 7916 ),
( 7148001, 7917 ),
( 7148003, 7918 ),
( 7148047, 7921 ),
( 7149007, 7922 ),
( 7150002, 7926 ),
( 7150026, 7929 ),
( 7150034, 7930 ),
( 7151002, 7936 ),
( 7152001, 7940 ),
( 7152025, 7944 ),
( 7153085, 7946 ),
( 7153097, 7949 ),
( 7154058, 7950 ),
( 7154062, 7951 ),
( 7155001, 7952 ),
( 7155029, 7953 ),
( 7155031, 7954 ),
( 7156001, 7957 ),
( 7156046, 7959 ),
( 7157001, 7962 ),
( 7158001, 7994 ),
( 7158015, 7995 ),
( 7158020, 7996 ),
( 7159002, 7998 ),
( 7159014, 7999 ),
( 7159064, 8003 ),
( 7160017, 8005 ),
( 7160041, 8007 ),
( 7160047, 8008 ),
( 7160056, 8010 ),
( 7160065, 8014 ),
( 7161005, 8015 ),
( 7161025, 8016 ),
( 7162001, 8021 ),
( 7162015, 8022 ),
( 7162033, 8023 ),
( 7163028, 8024 ),
( 7163037, 8025 ),
( 7163044, 8026 ),
( 7163064, 8027 ),
( 7164001, 8030 ),
( 7164003, 8031 ),
( 7164034, 8032 ),
( 7165001, 8035 ),
( 7166001, 8036 ),
( 7166031, 8037 ),
( 7167001, 8040 ),
( 7167002, 8047 ),
( 7167015, 8048 ),
( 7167022, 8049 ),
( 7168010, 8051 ),
( 7168011, 8052 ),
( 7168018, 8054 ),
( 7169027, 8055 ),
( 7169040, 8056 ),
( 7170025, 8057 ),
( 7170031, 8058 ),
( 7171001, 8059 ),
( 7171021, 8060 ),
( 7171027, 8061 ),
( 7174001, 8092 ),
( 7175002, 8104 ),
( 7176001, 8107 ),
( 7346019, 8119 )
)

def fill(run):
    index = bisect(_first_run, (run, 9999)) - 1
    return _first_run[index][1]


def calculate_binning(resolution_fun, ptmin=2.00, ptmax=10.00):
    """
    
    """
    pt = ptmin
    bin_edges = [ptmin]
    while pt < ptmax:
        width = 0
        while resolution_fun(pt+width) > width:
            width += 0.01
        bin_edges.append(pt+2*width)
        pt = pt+2*width
    return bin_edges
