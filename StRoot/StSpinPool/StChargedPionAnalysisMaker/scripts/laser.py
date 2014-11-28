import os, datetime, time, calendar, ROOT, MySQLdb, math

dataDir = '/Users/kocolosk/data/run6/laser'

ROOT.gStyle.SetCanvasColor(10)
ROOT.gStyle.SetFillColor(10)
ROOT.gStyle.SetStatColor(0)
ROOT.gStyle.SetPalette(1)
ROOT.gStyle.SetCanvasBorderMode(0)
ROOT.gStyle.SetOptDate(1)
ROOT.gStyle.SetOptFit(111)
ROOT.gStyle.SetPadLeftMargin(0.12)
#ROOT.gStyle.SetPadRightMargin(0.08)
#ROOT.gStyle.SetLabelOffset(0.005,'y')

class laserDV:
    """TPC laser measurement"""
    def __init__(self, sqlLine=None, nt=None):
        if sqlLine is not None:
            elems = sqlLine.split('|')
            elems = [elem.strip() for elem in elems[1:-1]]
            self.beginTime = self.dateTimeFromTimeStamp(elems[4])
            self.entryTime = self.dateTimeFromTimeStamp(elems[1])
            self.deactive  = self.setDeactive(int(elems[8]))
            self.flavor    = elems[5]
            self.laserDriftVelocityEast = float(elems[9])
            self.laserDriftVelocityWest = float(elems[10])
            self.cathodeDriftVelocityEast = float(elems[11])
            self.cathodeDriftVelocityWest = float(elems[12])
        if nt is not None:
            date = '200' + str(int(nt.date))
            time = str(int(nt.time)).rjust(6,'0')
            self.beginTime = self.dateTimeFromTimeStamp(date[0:4] + '-' + date[4:6] + '-' + date[6:8] + ' ' + time[0:2] + ':' + time[2:4] + ':' + time[4:6])
            self.entryTime = None
            self.deactive  = None
            self.flavor    = None
            self.laserDriftVelocityEast = nt.dvEast
            self.laserDriftVelocityWest = nt.dvWest
            self.laserDriftVelocity     = nt.dvAll
            self.laserDriftVelocityEastErr = nt.ddvEast
            self.laserDriftVelocityWestErr = nt.ddvWest
            self.laserDriftVelocityErr     = nt.ddvAll
            self.slope     = nt.slAll
            self.slopeEast = nt.slEast
            self.slopeWest = nt.slWest
            self.slopeErr  = nt.dslAll
            self.slopeEastErr = nt.dslEast
            self.slopeWestErr = nt.dslWest
            self.events = nt.events
            self.run    = nt.run
            self.day    = nt.day
    
    
    def setDeactive(self, unixTime):
        """store deactive given in unixTime as datetime object"""
        if unixTime > 0:
            self.deactive = datetime.datetime( time.gmtime(unixTime)[0:6] )
        else:
            self.deactive = None
    
    
    def getBeginTime(self):
        """returns beginTime as string"""
        return self.beginTime.isoformat(' ')
    
    
    def getEntryTime(self):
        """returns entryTime as string"""
        return self.entryTime.isoformat(' ')
    
    
    def getDeactive(self):
        """returns deactivation time as string"""
        if self.deactive is not None:
            return self.deactive.isoformat(' ')
        return '0'
    
    
    def dateTimeFromTimeStamp(self, s):
        """convert string == yyyy-mm-dd hh:mm:ss to datetime.datetime UTC"""
        d = datetime.datetime(int(s[0:4]),int(s[5:7]),int(s[8:10]), int(s[11:13]),int(s[14:16]),int(s[17:19]))
        #print s, d
        return d
    


class ShiftedExponential:
    def __call__(self, x, par):
        """custom function for a TF1 used to fit drift velocity measurements"""
        self.constant = par[0]
        self.slope    = par[1]
        self.xoffset  = par[2]
        self.yoffset  = par[3]
        
        #return ( self.constant * math.exp((self.slope - self.xoffset)*x[0] / 1000) - self.yoffset )
        #return (par[0] * math.exp(par[1] * (x[0] - 495000)) - 5.5)
        
        #shift x-axis and rescale
        #print self.slope, x[0], self.xoffset, (self.slope * (x[0] - self.xoffset))/10000, self.yoffset, self.constant
        x = (x[0] - self.xoffset) / 1000
        arg = self.slope * x
        #print arg
        result = math.exp(arg)
        result = result * self.constant
        result = result - self.yoffset
        return result
        
        #return self.constant * math.exp( -1.0 * self.slope * (x[0]-self.xoffset) / 10000 ) - self.yoffset
    
    


#some important dates
start = datetime.datetime(2006, 5, 15, 4, 4, 36)
purge = datetime.datetime(2006, 5, 18, 22, 40) #UTC
end   = datetime.datetime(2006, 5, 24, 10, 4, 2)

#list of runs from Murad
badRuns = [7139018, 7139019, 7140007, 7140008, 7140009, 7140010, 7140011, 7140015, 7140016, 7140017
, 7140018, 7141010, 7141011, 7141015, 7141016, 7141034, 7141038, 7141039, 7141042, 7141043, 7141044
, 7141064, 7141066, 7141069, 7141070, 7141071, 7141074, 7141075, 7141076, 7141077, 7142001, 7142005
, 7142014, 7142015, 7142016, 7142017, 7142018, 7142022, 7142023, 7142024, 7142025, 7142028, 7142029
, 7142033, 7142034, 7142035, 7142036, 7142045, 7142046, 7142047, 7142048, 7142049, 7142059, 7142060
, 7142061, 7143001, 7143004, 7143005, 7143006, 7143008, 7143011, 7143012, 7143013, 7143014, 7143025
, 7144011, 7144014, 7144015, 7144018 ]

def runBeginTime(runNumber):
    """return datetime indicating beginning of a STAR run"""
    db = MySQLdb.connect(host='star1.lns.mit.edu', port=3316, db='RunLog_onl')
    dbc = db.cursor()
    dbc.execute('select beginTime from beamInfo where runNumber=%d and deactive=0 order by beginTime desc limit 1' % (runNumber,))
    return dbc.fetchone()[0]
    
firstBad = runBeginTime(7139018)
lastBad  = runBeginTime(7143001)

#load the DB measurements
db = []
f = open( os.path.join(dataDir, 'tables.txt') )
lines = f.readlines()
for line in lines[3:-1]:
    db.append( laserDV(line) )
    
# load the new measurements from Yuri's ntuple
local = []
f = ROOT.TFile.Open( os.path.join(dataDir, 'LaserPlots.root') )
nt = f.Get("RunNT")
for i in range(nt.GetEntries()):
    nt.GetEntry(i)
    local.append ( laserDV(nt=nt) )
#c = ROOT.TCanvas()
#for key in f.GetListOfKeys():
#    obj = key.ReadObj()
#    if obj.InheritsFrom("TH1"):
#        obj.Draw()
#        c.Print( os.path.join(dataDir, 'yuri', '%s.gif' % obj.GetName()) )

dbGraphEast = ROOT.TGraph(len(db))
dbGraphWest = ROOT.TGraph(len(db))

localGraphEast = ROOT.TGraphErrors(len(local))
localGraphWest = ROOT.TGraphErrors(len(local))
localGraphAll  = ROOT.TGraphErrors(len(local))

X0 = calendar.timegm( datetime.datetime(2006,05,14).timetuple() )

def fillGraph(graph, measurements, datamember):
    """sets points on the graph using specified datamember from measurements"""
    points = []
    counter = 0
    for dv in measurements:
        if dv.beginTime < start or dv.beginTime > end: continue
        val = getattr(dv, datamember)
        xval = calendar.timegm(dv.beginTime.timetuple()) #unixtime
        graph.SetPoint(counter, xval-X0, val)
        points.append((xval-X0,val))
        #DB measurements have no errors
        errorMember = datamember + 'Err'
        try:
            error = getattr(dv, errorMember)
            graph.SetPointError(counter, 0.0, error)
        except AttributeError: pass
        
        counter += 1
    graph.Set(counter)
    return points


dbGraphEastPoints = fillGraph(dbGraphEast, db, 'laserDriftVelocityEast')
dbGraphWestPoints = fillGraph(dbGraphWest, db, 'laserDriftVelocityWest')
localGraphEastPoints = fillGraph(localGraphEast, local, 'laserDriftVelocityEast')
localGraphWestPoints = fillGraph(localGraphWest, local, 'laserDriftVelocityWest')
localGraphAllPoints = fillGraph(localGraphAll, local, 'laserDriftVelocity')

ratioGraphWest = ROOT.TGraph(localGraphWest.GetN())
ratioGraphWest.SetTitle('')
counter = 0
bin = 0
for dv in localGraphWestPoints:
    for row,elem in enumerate(dbGraphWestPoints):
        if elem[0] > dv[0]: 
            if row == 0:
                val = (dbGraphWestPoints[row][1] - dv[1]) / dbGraphWestPoints[row-1][1]
            else:
                val = (dbGraphWestPoints[row-1][1] - dv[1]) / dbGraphWestPoints[row-1][1]
            break
    ratioGraphWest.SetPoint(bin, dv[0], val)
    bin += 1
ratioGraphWest.GetYaxis().SetRangeUser(-0.005, 0.005)

print '   '

ratioGraphEast = ROOT.TGraph(dbGraphEast.GetN())
ratioGraphEast.SetTitle('')
counter = 0
bin = 0
for dv in localGraphEastPoints:
    for row,elem in enumerate(dbGraphEastPoints):
        if elem[0] > dv[0]: 
            if row == 0:
                val = (dbGraphEastPoints[row][1] - dv[1]) / dbGraphEastPoints[row-1][1]
            else:
                val = (dbGraphEastPoints[row-1][1] - dv[1]) / dbGraphEastPoints[row-1][1]
            break
    ratioGraphEast.SetPoint(bin, dv[0], val)
    bin += 1
ratioGraphEast.GetYaxis().SetRangeUser(-0.005, 0.005)

# fit the d.v. points in exponential region so we can interpolate
#graphToFit = ROOT.TGraph(localGraphAll.GetN())
#print len(localGraphAllPoints)
#graphToFitPoints = localGraphAllPoints[14:]
graphToFit = ROOT.TGraph(localGraphEast.GetN())
print len(localGraphEastPoints)
graphToFitPoints = localGraphEastPoints[14:]
offset = graphToFitPoints[0][0] - 1000
for row,elem in enumerate(graphToFitPoints):
    print elem[0], elem[1]
    graphToFit.SetPoint(row, elem[0] , elem[1] * 1000)
graphToFit.Set(len(graphToFitPoints))
graphToFit.SetMarkerStyle(30)

fitEast = ROOT.TF1('fit', ShiftedExponential(), graphToFitPoints[0][0], graphToFitPoints[-1][0], 4)
fitEast.SetParameter(2, graphToFitPoints[0][0]-100)
fitEast.SetParameter(3, 5.5)
fitEast.SetParLimits(1, -100, -0.0001)
fitEast.SetParName(0,'constant')
fitEast.SetParName(1,'slope')
fitEast.SetParName(2,'xoffset')
fitEast.SetParName(3,'yoffset')
#fitEast.SetParLimits(2, 400 * 1000, 600 * 1000)
#fitEast.SetParLimits(3, 5.5, 5.6)
#fitEast.SetParLimits(1, -5.0*10**-8, -3.0*10**-8)

graphToFit.Fit(fitEast)


graphToFitWest = ROOT.TGraph(localGraphWest.GetN())
print len(localGraphWestPoints)
graphToFitWestPoints = localGraphWestPoints[14:]
offset = graphToFitWestPoints[0][0] - 1000
for row,elem in enumerate(graphToFitWestPoints):
    print elem[0], elem[1]
    graphToFitWest.SetPoint(row, elem[0] , elem[1] * 1000)
graphToFitWest.Set(len(graphToFitWestPoints))
graphToFitWest.SetMarkerStyle(30)

fitWest = ROOT.TF1('fitWest', ShiftedExponential(), graphToFitWestPoints[0][0], graphToFitWestPoints[-1][0], 4)
fitWest.SetParameter(2, graphToFitWestPoints[0][0]-100)
fitWest.SetParameter(3, 5.5)
fitWest.SetParLimits(1, -100, -0.0001)
fitWest.SetParName(0,'constant')
fitWest.SetParName(1,'slope')
fitWest.SetParName(2,'xoffset')
fitWest.SetParName(3,'yoffset')

graphToFitWest.Fit(fitWest)



for run in badRuns:
    ts = runBeginTime(run)
    unixTimeStamp = calendar.timegm(ts.timetuple())
    print run, runBeginTime(run), fitEast.Eval(unixTimeStamp-X0)/1000
    
    ts = ts - datetime.timedelta(seconds=10)
    
    # make a new D.V. table
    filename = 'tpcDriftVelocity.%4d%02d%02d.%02d%02d%02d.C' % (ts.year, ts.month, ts.day, ts.hour, ts.minute, ts.second)
    f = open(filename, 'w')
    f.write('TDataSet *CreateTable() {')
    f.write('  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;\n')
    f.write('  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);\n')
    f.write('  tpcDriftVelocity_st row; // interpolated for run %d\n' % (run,))
    f.write('  row.laserDriftVelocityEast	 =   %1.5f;\n' % (fitEast.Eval(unixTimeStamp-X0)/1000,))
    f.write('  row.laserDriftVelocityWest	 =   %1.5f;\n' % (fitWest.Eval(unixTimeStamp-X0)/1000,))
    f.write('  row.cathodeDriftVelocityEast	 =          0;\n')
    f.write('  row.cathodeDriftVelocityWest	 =          0;\n')
    f.write('  tableSet->AddAt(&row);\n')
    f.write('  return (TDataSet *)tableSet;\n')
    f.write('};\n')

# time to make pretty pictures

dbGraphEast.SetMarkerStyle(20)
dbGraphWest.SetMarkerStyle(21)
localGraphEast.SetMarkerStyle(24)
localGraphWest.SetMarkerStyle(25)
localGraphAll.SetMarkerStyle(30)

graphList = [dbGraphEast, dbGraphWest, localGraphEast, localGraphWest, localGraphAll, ratioGraphEast, ratioGraphWest, graphToFit]
for gr in graphList:
    gr.GetXaxis().SetNdivisions(710)
    gr.GetXaxis().SetTimeDisplay(1)
    gr.GetXaxis().SetTimeFormat('%b %d %H:%M')
    gr.GetXaxis().SetTimeOffset(X0, 'gmt')
    gr.GetYaxis().SetTitle('cm / #mu s')
    gr.SetTitle('Drift Velocity Investigations')
    

ratioGraphEast.GetYaxis().SetTitle('')    
ratioGraphWest.GetYaxis().SetTitle('')    

tpcPurge = ROOT.TLine()
tpcPurge.SetLineColor(ROOT.kRed)
tpcPurge.SetLineWidth(3)

tpcPurgeText = ROOT.TText()
tpcPurgeText.SetTextColor(ROOT.kRed)

badDataLine = ROOT.TLine()
badDataLine.SetLineWidth(15)
badDataLine.SetLineColor(ROOT.kBlue)
badDataText = ROOT.TText()
badDataText.SetTextColor(ROOT.kBlue)

dateText = ROOT.TText()

leg = ROOT.TLegend(0.7,0.65,0.89,0.89)
leg.AddEntry(dbGraphEast, 'db d.v. East', 'p')
leg.AddEntry(dbGraphWest, 'db d.v. West', 'p')
leg.AddEntry(localGraphEast, 'local d.v. East', 'p')
leg.AddEntry(localGraphWest, 'local d.v. West', 'p')
leg.AddEntry(localGraphAll, 'local d.v. All', 'p')

c1 = ROOT.TCanvas('c1')
dbGraphWest.SetTitle('West Laser Drift Velocities')
dbGraphWest.Draw('ap')
localGraphWest.Draw('p same')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphWest.GetYaxis().GetXmin(), 
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphWest.GetYaxis().GetXmax()
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
leg.Draw()
c1.Print('west.gif')


c1b = ROOT.TCanvas('c1b')
ratioGraphWest.SetTitle('(db-local)/db for West Drift Velocities')
ratioGraphWest.SetMarkerStyle(21)
ratioGraphWest.Draw('ap')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    -0.005, 
    calendar.timegm(purge.timetuple())-X0, 
    0.005
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
badDataLine.DrawLine(
    calendar.timegm(firstBad.timetuple())-X0,
    -0.003,
    calendar.timegm(lastBad.timetuple())-X0,
    -0.003
    )
badDataText.DrawTextNDC(0.55, 0.15, 'Large DCA')
c1b.Print('westRatio.gif')


c2 = ROOT.TCanvas('c2')
dbGraphEast.SetTitle('East Laser Drift Velocities')
dbGraphEast.Draw('ap')
localGraphEast.Draw('p same')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphEast.GetYaxis().GetXmin(), 
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphEast.GetYaxis().GetXmax()
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
leg.Draw()
c2.Print('east.gif')


c2b = ROOT.TCanvas('c2b')
ratioGraphEast.SetTitle('(db-local)/db for East Drift Velocities')
ratioGraphEast.SetMarkerStyle(20)
ratioGraphEast.Draw('ap')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    -0.005,
    calendar.timegm(purge.timetuple())-X0, 
    0.005
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
badDataLine.DrawLine(
    calendar.timegm(firstBad.timetuple())-X0,
    -0.003,
    calendar.timegm(lastBad.timetuple())-X0,
    -0.003
    )
badDataText.DrawTextNDC(0.55, 0.15, 'Large DCA')
c2b.Print('eastRatio.gif')


c3 = ROOT.TCanvas('c3')
localGraphEast.SetTitle('New Drift Velocities')
#localGraphEast.GetXaxis().
localGraphEast.Draw('ap')
localGraphWest.Draw('p same')
localGraphAll.Draw('p same')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    localGraphEast.GetYaxis().GetXmin(), 
    calendar.timegm(purge.timetuple())-X0, 
    localGraphEast.GetYaxis().GetXmax()
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
leg.Draw()
c3.Print('new.gif')




c4 = ROOT.TCanvas('c4')
dbGraphEast2 = ROOT.TGraph(dbGraphEast) #copy so we can change the title
dbGraphEast2.GetXaxis().SetNdivisions(710)
dbGraphEast2.GetXaxis().SetTimeDisplay(1)
dbGraphEast2.GetXaxis().SetTimeFormat('%b %d %H:%M')
dbGraphEast2.GetXaxis().SetTimeOffset(X0, 'gmt')

dbGraphEast2.SetTitle('Old Drift Velocities')
dbGraphEast2.Draw('ap')
dbGraphWest.Draw('p same')
tpcPurge.DrawLine(
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphEast2.GetYaxis().GetXmin(), 
    calendar.timegm(purge.timetuple())-X0, 
    dbGraphEast2.GetYaxis().GetXmax()
    )
tpcPurgeText.DrawTextNDC(0.15,0.75,'TPC Purge')
leg.Draw()
c4.Print('old.gif')


c5 = ROOT.TCanvas('c5')
graphToFit.SetTitle('Exponential Fit to East D.V.')
graphToFit.GetXaxis().SetNdivisions(110)
graphToFit.GetXaxis().SetTimeFormat('%b %d')
graphToFit.GetYaxis().SetTitle('(cm/#mu s) * 1000')
graphToFit.GetYaxis().SetLabelOffset(0.003)
graphToFit.GetYaxis().SetTitleOffset(1.15)
graphToFit.Draw('ap')
c5.Print('fitEast.gif')

if __name__ == '__main__':
    #loadData()
    raw_input('press enter:')

