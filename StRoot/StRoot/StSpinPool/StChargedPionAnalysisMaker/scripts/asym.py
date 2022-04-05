from array import array
import ROOT
from xsec import datapoint
from analysis.runlists import golden_runlist_c, minbias_runs, run6a
from analysis.runlists import final_runlist_run5_no_minbias, final_runlist_run5


class AsymmetryGenerator:
    """Python version of FastAsymmetryMaker"""  
    def __init__(self, name, usePolarizations = True, useR123 = True, \
        bins = [4, 2.0, 10.0], key='pt'):
        self.name = name
        self.bins = bins
        self.key = key
        self.multi_stats = False
        if len(self.bins) == 3:
            self.top = { 
                'ls' : ROOT.TH1D('_%s_ls' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'us' : ROOT.TH1D('_%s_us' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'ly' : ROOT.TH1D('_%s_ly' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'lb' : ROOT.TH1D('_%s_lb' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'll' : ROOT.TH1D('_%s_ll' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
            }
            self.bot = { 
                'ls' : ROOT.TH1D('_%s_ls_bot' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'us' : ROOT.TH1D('_%s_us_bot' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'ly' : ROOT.TH1D('_%s_ly_bot' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'lb' : ROOT.TH1D('_%s_lb_bot' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
                'll' : ROOT.TH1D('_%s_ll_bot' % (name,), '', self.bins[0], \
                     self.bins[1], self.bins[2]),
            }
        else:
            arraybins = array('d')
            arraybins.fromlist(self.bins)
            nbins = len(self.bins)
            self.top = { 
                'ls' : ROOT.TH1D('_%s_ls' % (name,), '', nbins-1, arraybins),
                'us' : ROOT.TH1D('_%s_us' % (name,), '', nbins-1, arraybins),
                'ly' : ROOT.TH1D('_%s_ly' % (name,), '', nbins-1, arraybins),
                'lb' : ROOT.TH1D('_%s_lb' % (name,), '', nbins-1, arraybins),
                'll' : ROOT.TH1D('_%s_ll' % (name,), '', nbins-1, arraybins),
            }
            self.bot = { 
                'ls' : ROOT.TH1D('_%s_ls_bot' % (name,), '', nbins-1,arraybins),
                'us' : ROOT.TH1D('_%s_us_bot' % (name,), '', nbins-1,arraybins),
                'ly' : ROOT.TH1D('_%s_ly_bot' % (name,), '', nbins-1,arraybins),
                'lb' : ROOT.TH1D('_%s_lb_bot' % (name,), '', nbins-1,arraybins),
                'll' : ROOT.TH1D('_%s_ll_bot' % (name,), '', nbins-1,arraybins),
            }
        
        [ h.Sumw2() for h in self.top.values() ]
        [ h.Sumw2() for h in self.bot.values() ]
        
        self.usePolarizations = usePolarizations
        self.useR123             = useR123
    
    
    def FillFromHistogramManager(self,mgr,trigId,charge,uu,ud,du,dd,Py,Pb):
         if self.key in ('jet_pt',):
                self.FillFromHistogram(5, uu, ud, du, dd, Py, Pb, \
                     mgr['uu'][trigId][self.key])
                self.FillFromHistogram(6, uu, ud, du, dd, Py, Pb, \
                     mgr['du'][trigId][self.key])
                self.FillFromHistogram(9, uu, ud, du, dd, Py, Pb, \
                     mgr['ud'][trigId][self.key])
                self.FillFromHistogram(10,uu, ud, du, dd, Py, Pb, \
                     mgr['dd'][trigId][self.key])
         elif self.key in ('z_away',):
                # c = ROOT.TCanvas()
                # mgr['uu'][trigId].trackHistograms(charge)[self.key].Draw('col z')
                # raw_input('hold')
                self.FillFromHistogram(5, uu, ud, du, dd, Py, Pb, \
                  mgr['uu'][trigId].trackHistograms(charge)[self.key].ProjectionY())
                self.FillFromHistogram(6, uu, ud, du, dd, Py, Pb, \
                  mgr['du'][trigId].trackHistograms(charge)[self.key].ProjectionY())
                self.FillFromHistogram(9, uu, ud, du, dd, Py, Pb, \
                  mgr['ud'][trigId].trackHistograms(charge)[self.key].ProjectionY())
                self.FillFromHistogram(10, uu, ud, du, dd, Py, Pb, \
                  mgr['dd'][trigId].trackHistograms(charge)[self.key].ProjectionY())
         else:              
                self.FillFromHistogram(5, uu, ud, du, dd, Py, Pb, \
                     mgr['uu'][trigId].trackHistograms(charge)[self.key])
                self.FillFromHistogram(6, uu, ud, du, dd, Py, Pb, \
                     mgr['du'][trigId].trackHistograms(charge)[self.key])
                self.FillFromHistogram(9, uu, ud, du, dd, Py, Pb, \
                     mgr['ud'][trigId].trackHistograms(charge)[self.key])
                self.FillFromHistogram(10,uu, ud, du, dd, Py, Pb, \
                     mgr['dd'][trigId].trackHistograms(charge)[self.key])
    
    
    def FillFromHistogram(self, spinbit, uu, ud, du, dd, Py, Pb, h):
        if self.multi_stats:
            R1 = float(uu+ud)/(du+dd)
            R2 = float(uu+du)/(ud+dd)
            R3 = float(uu+dd)/(ud+du)

            Rls = float(uu)/dd
            Rus = float(ud)/du

            if spinbit == 5: ## yb == uu
                self.top['ly'].Add(h, (-1.*Py) )
                self.bot['ly'].Add(h, (Py*Py) )

                self.top['lb'].Add(h, (-1.*Pb) )
                self.bot['lb'].Add(h, (Pb*Pb) )

                self.top['ll'].Add(h, (Py*Pb) )
                self.bot['ll'].Add(h, (Py*Py*Pb*Pb) )

                self.top['ls'].Add(h, (Py*Pb) )
                self.bot['ls'].Add(h, (Py*Py*Pb*Pb) )
            elif spinbit == 9: ## yb == ud
                self.top['ly'].Add(h, (-1.*Py) )
                self.bot['ly'].Add(h, (Py*Py) )

                self.top['lb'].Add(h, (R2*Pb) )
                self.bot['lb'].Add(h, (R2*Pb*Pb) )

                self.top['ll'].Add(h, (-1.*R3*Py*Pb) )
                self.bot['ll'].Add(h, (R3*Py*Py*Pb*Pb) )

                self.top['us'].Add(h, (Py*Pb) )
                self.bot['us'].Add(h, (Py*Py*Pb*Pb) )
            elif spinbit == 6: ## yb == du
                self.top['ly'].Add(h, (R1*Py) )
                self.bot['ly'].Add(h, (R1*Py*Py) )

                self.top['lb'].Add(h, (-1.*Pb) )
                self.bot['lb'].Add(h, (Pb*Pb) )

                self.top['ll'].Add(h, (-1.*R3*Py*Pb) )
                self.bot['ll'].Add(h, (R3*Py*Py*Pb*Pb) )

                self.top['us'].Add(h, (-1.*Rus*Py*Pb) )
                self.bot['us'].Add(h, (Rus*Py*Py*Pb*Pb) )
            elif spinbit == 10: ## yb == dd
                self.top['ly'].Add(h, (R1*Py) )
                self.bot['ly'].Add(h, (R1*Py*Py) )

                self.top['lb'].Add(h, (R2*Pb) )
                self.bot['lb'].Add(h, (R2*Pb*Pb) )

                self.top['ll'].Add(h, (Py*Pb) )
                self.bot['ll'].Add(h, (Py*Py*Pb*Pb) )

                self.top['ls'].Add(h, (-1.*Rls*Py*Pb) )
                self.bot['ls'].Add(h, (Rls*Py*Py*Pb*Pb) )
            else:
                print spinbit, 'is not a valid spinbit'
        
        if not self.multi_stats:
            for bin in range(1, h.GetNbinsX()+1):
                x = h.GetBinCenter(bin)
                count = int(h.GetBinContent(bin))
                for i in range(count):
                    self.Fill(spinbit, uu, ud, du, dd, Py, Pb, x)
    
    
    def Fill(self, spinbit, uu, ud, du, dd, Py, Pb, val):
        if not self.usePolarizations: Py = Pb = 1.0
        
        if self.useR123:
            self._FillR123(spinbit, uu, ud, du, dd, Py, Pb, val)
        else:
            self._FillR456(spinbit, uu, ud, du, dd, Py, Pb, val)
    
    
    def _FillR123(self, spinbit, uu, ud, du, dd, Py, Pb, val):
        R1 = float(uu+ud)/(du+dd)
        R2 = float(uu+du)/(ud+dd)
        R3 = float(uu+dd)/(ud+du)
        
        Rls = float(uu)/dd
        Rus = float(ud)/du
        
        if spinbit == 5: ## yb == uu
            self.top['ly'].Fill(val, (-1.*Py) )
            self.bot['ly'].Fill(val, (Py*Py) )
            
            self.top['lb'].Fill(val, (-1.*Pb) )
            self.bot['lb'].Fill(val, (Pb*Pb) )
            
            self.top['ll'].Fill(val, (Py*Pb) )
            self.bot['ll'].Fill(val, (Py*Py*Pb*Pb) )
            
            self.top['ls'].Fill(val, (Py*Pb) )
            self.bot['ls'].Fill(val, (Py*Py*Pb*Pb) )
        elif spinbit == 9: ## yb == ud
            self.top['ly'].Fill(val, (-1.*Py) )
            self.bot['ly'].Fill(val, (Py*Py) )
            
            self.top['lb'].Fill(val, (R2*Pb) )
            self.bot['lb'].Fill(val, (R2*Pb*Pb) )
            
            self.top['ll'].Fill(val, (-1.*R3*Py*Pb) )
            self.bot['ll'].Fill(val, (R3*Py*Py*Pb*Pb) )
            
            self.top['us'].Fill(val, (Py*Pb) )
            self.bot['us'].Fill(val, (Py*Py*Pb*Pb) )
        elif spinbit == 6: ## yb == du
            self.top['ly'].Fill(val, (R1*Py) )
            self.bot['ly'].Fill(val, (R1*Py*Py) )
            
            self.top['lb'].Fill(val, (-1.*Pb) )
            self.bot['lb'].Fill(val, (Pb*Pb) )
            
            self.top['ll'].Fill(val, (-1.*R3*Py*Pb) )
            self.bot['ll'].Fill(val, (R3*Py*Py*Pb*Pb) )
            
            self.top['us'].Fill(val, (-1.*Rus*Py*Pb) )
            self.bot['us'].Fill(val, (Rus*Py*Py*Pb*Pb) )
        elif spinbit == 10: ## yb == dd
            self.top['ly'].Fill(val, (R1*Py) )
            self.bot['ly'].Fill(val, (R1*Py*Py) )
            
            self.top['lb'].Fill(val, (R2*Pb) )
            self.bot['lb'].Fill(val, (R2*Pb*Pb) )
            
            self.top['ll'].Fill(val, (Py*Pb) )
            self.bot['ll'].Fill(val, (Py*Py*Pb*Pb) )
            
            self.top['ls'].Fill(val, (-1.*Rls*Py*Pb) )
            self.bot['ls'].Fill(val, (Rls*Py*Py*Pb*Pb) )
        else:
            print spinbit, 'is not a valid spinbit'
        
    
    
    def _FillR456(self, spinbit, uu, ud, du, dd, Py, Pb, val):
        ## NB: these might not be the traditional definitions of R4/5/6
        R4 = float(dd)/uu
        R5 = float(dd)/ud
        R6 = float(dd)/du
        
        if spinbit == 5: ## yb == uu
            self.top['ly'].Fill(val, (R4*Py) )
            self.bot['ly'].Fill(val, (R4*Py*Py) )
            
            self.top['lb'].Fill(val, (R4*Pb) )
            self.bot['lb'].Fill(val, (R4*Pb*Pb) )
            
            self.top['ll'].Fill(val, (R4*Py*Pb) )
            self.bot['ll'].Fill(val, (R4*Py*Py*Pb*Pb) )
            
            self.top['ls'].Fill(val, (R4*Py*Pb) )
            self.bot['ls'].Fill(val, (R4*Py*Py*Pb*Pb) )
        elif spinbit == 9: ## yb == ud
            self.top['ly'].Fill(val, (R5*Py) )
            self.bot['ly'].Fill(val, (R5*Py*Py) )
            
            self.top['lb'].Fill(val, (-1.*R5*Pb) )
            self.bot['lb'].Fill(val, (R5*Pb*Pb) )
            
            self.top['ll'].Fill(val, (-1.*R5*Py*Pb) )
            self.bot['ll'].Fill(val, (R5*Py*Py*Pb*Pb) )
            
            self.top['us'].Fill(val, (R5*Py*Pb) )
            self.bot['us'].Fill(val, (R5*Py*Py*Pb*Pb) )
        elif spinbit == 6: ## yb == du
            self.top['ly'].Fill(val, (-1.*R6*Py) )
            self.bot['ly'].Fill(val, (R6*Py*Py) )
            
            self.top['lb'].Fill(val, (R6*Pb) )
            self.bot['lb'].Fill(val, (R6*Pb*Pb) )
            
            self.top['ll'].Fill(val, (-1.*R6*Py*Pb) )
            self.bot['ll'].Fill(val, (R6*Py*Py*Pb*Pb) )
            
            self.top['us'].Fill(val, (-1.*Py*Pb) )
            self.bot['us'].Fill(val, (Py*Py*Pb*Pb) )
        elif spinbit == 10: ## yb == dd
            self.top['ly'].Fill(val, (-1.*Py) )
            self.bot['ly'].Fill(val, (Py*Py) )
            
            self.top['lb'].Fill(val, (-1.*Pb) )
            self.bot['lb'].Fill(val, (Pb*Pb) )
            
            self.top['ll'].Fill(val, (Py*Pb) )
            self.bot['ll'].Fill(val, (Py*Py*Pb*Pb) )
            
            self.top['ls'].Fill(val, (-1.*Py*Pb) )
            self.bot['ls'].Fill(val, (Py*Py*Pb*Pb) )
        else:
            print spinbit, 'is not a valid spinbit'
        
    
    
    def GetAsymmetry(self, asymname):
        try:
            asym = self.top[asymname].Clone()
            asym.Divide(self.bot[asymname])
            return asym
        except KeyError:
            print 'asymname must be one of', self.top.keys
            return None
    


class ScalarCountsRecord:
    def __init__(self, fill, run, board, timebin, uu, du, ud, dd):
        self.fill       = fill
        self.run            = run
        self.board      = board
        self.timebin    = timebin
        self.uu         = uu
        self.du         = du
        self.ud         = ud
        self.dd         = dd


class ScalarCounts(dict):
    def __init__(self, pathToData):
        f = open(pathToData)
        f.readline()  ## skip header
        for line in f:
            items = line.split()
            key = '%s-%s-%s' % (items[1], items[2], items[3])
            fill        = int(items[0])
            run     = int(items[1])
            board       = int(items[2])
            timebin = int(items[3])
            uu          = int(items[4])
            du          = int(items[5])
            ud          = int(items[6])
            dd          = int(items[7])
            self[key] = ScalarCountsRecord(fill, run, board, timebin, uu,du,ud,dd)
        
    


class PolarizationRecord:
    def __init__(self, fill, version, pb, eb, py, ey):
        self.fill       = fill
        self.version    = version
        if pb > 1.00:
            self.pb         = pb/100.0
            self.eb         = eb/100.0
            self.py         = py/100.0
            self.ey         = ey/100.0
        else:
            self.pb         = pb
            self.eb         = eb
            self.py         = py
            self.ey         = ey
    


class Polarizations:
    ## these were the original 'final' 2005 numbers, but have since been revised
    FinalApril  = {
    6928:PolarizationRecord(6928, 'final-april', 0.4316, 0.0268, 0.4953, 0.0316),
    6931:PolarizationRecord(6931, 'final-april', 0.4384, 0.0296, 0.5276, 0.0353),
    6936:PolarizationRecord(6936, 'final-april', 0.5167, 0.0316, 0.4985, 0.0369),
    6943:PolarizationRecord(6943, 'final-april', 0.5180, 0.0254, 0.4931, 0.0336),
    6945:PolarizationRecord(6945, 'final-april', 0.5082, 0.0251, 0.4680, 0.0310),
    6947:PolarizationRecord(6947, 'final-april', 0.4825, 0.0245, 0.4448, 0.0294),
    6955:PolarizationRecord(6955, 'final-april', 0.5396, 0.0412, 0.5540, 0.0466),
    6957:PolarizationRecord(6957, 'final-april', 0.4877, 0.0260, 0.4625, 0.0306),
    6959:PolarizationRecord(6959, 'final-april', 0.4869, 0.0250, 0.4777, 0.0308),
    6963:PolarizationRecord(6963, 'final-april', 0.4870, 0.0261, 0.4273, 0.0295),
    6966:PolarizationRecord(6966, 'final-april', 0.5623, 0.0395, 0.4770, 0.0409),
    6967:PolarizationRecord(6967, 'final-april', 0.5652, 0.0291, 0.4422, 0.0297),
    6968:PolarizationRecord(6968, 'final-april', 0.4427, 0.0251, 0.4605, 0.0322),
    6969:PolarizationRecord(6969, 'final-april', 0.5448, 0.0278, 0.4405, 0.0296),
    6971:PolarizationRecord(6971, 'final-april', 0.5156, 0.0280, 0.4799, 0.0325),
    6972:PolarizationRecord(6972, 'final-april', 0.4497, 0.0240, 0.4945, 0.0339),
    6973:PolarizationRecord(6973, 'final-april', 0.4082, 0.0224, 0.4387, 0.0321),
    6975:PolarizationRecord(6975, 'final-april', 0.4477, 0.0244, 0.4216, 0.0293),
    6980:PolarizationRecord(6980, 'final-april', 0.4881, 0.0283, 0.5082, 0.0378),
    6988:PolarizationRecord(6988, 'final-april', 0.5016, 0.0264, 0.4708, 0.0350),
    6990:PolarizationRecord(6990, 'final-april', 0.4868, 0.0267, 0.4673, 0.0316),
    6991:PolarizationRecord(6991, 'final-april', 0.4767, 0.0304, 0.4042, 0.0493),
    6992:PolarizationRecord(6992, 'final-april', 0.4487, 0.0243, 0.4594, 0.0340),
    6994:PolarizationRecord(6994, 'final-april', 0.4538, 0.0256, 0.4591, 0.0360),
    6995:PolarizationRecord(6995, 'final-april', 0.5125, 0.0292, 0.4795, 0.0376),
    6997:PolarizationRecord(6997, 'final-april', 0.4565, 0.0255, 0.4681, 0.0333),
    6998:PolarizationRecord(6998, 'final-april', 0.4908, 0.0282, 0.4440, 0.0356),
    6999:PolarizationRecord(6999, 'final-april', 0.4862, 0.0254, 0.5257, 0.0336),
    7001:PolarizationRecord(7001, 'final-april', 0.4649, 0.0259, 0.4686, 0.0309),
    7002:PolarizationRecord(7002, 'final-april', 0.4625, 0.0297, 0.5168, 0.0801),
    7007:PolarizationRecord(7007, 'final-april', 0.4646, 0.0260, 0.4988, 0.0333),
    7010:PolarizationRecord(7010, 'final-april', 0.5135, 0.0261, 0.5294, 0.0338),
    7028:PolarizationRecord(7028, 'final-april', 0.4671, 0.0236, 0.4744, 0.0301),
    7029:PolarizationRecord(7029, 'final-april', 0.4871, 0.0269, 0.5054, 0.0348),
    7030:PolarizationRecord(7030, 'final-april', 0.5937, 0.0293, 0.5371, 0.0344),
    7032:PolarizationRecord(7032, 'final-april', 0.5371, 0.0278, 0.4925, 0.0321),
    7034:PolarizationRecord(7034, 'final-april', 0.5131, 0.0261, 0.4434, 0.0296),
    7035:PolarizationRecord(7035, 'final-april', 0.4555, 0.0239, 0.4730, 0.0310),
    7036:PolarizationRecord(7036, 'final-april', 0.5003, 0.0281, 0.4936, 0.0338),
    7037:PolarizationRecord(7037, 'final-april', 0.4980, 0.0368, 0.5020, 0.0402),
    7039:PolarizationRecord(7039, 'final-april', 0.4945, 0.0305, 0.4956, 0.0354),
    7046:PolarizationRecord(7046, 'final-april', 0.6471, 0.0318, 0.4718, 0.0445),
    7049:PolarizationRecord(7049, 'final-april', 0.5098, 0.0254, 0.4656, 0.0289),
    7051:PolarizationRecord(7051, 'final-april', 0.5126, 0.0287, 0.3986, 0.0290),
    7059:PolarizationRecord(7059, 'final-april', 0.5655, 0.0329, 0.4929, 0.0381),
    7063:PolarizationRecord(7063, 'final-april', 0.5435, 0.0319, 0.4882, 0.0358),
    7064:PolarizationRecord(7064, 'final-april', 0.5254, 0.0272, 0.3915, 0.0265),
    7067:PolarizationRecord(7067, 'final-april', 0.5480, 0.0282, 0.4389, 0.0320),
    7068:PolarizationRecord(7068, 'final-april', 0.5142, 0.0262, 0.3908, 0.0553),
    7069:PolarizationRecord(7069, 'final-april', 0.5682, 0.0301, 0.5540, 0.0514),
    7070:PolarizationRecord(7070, 'final-april', 0.5266, 0.0267, 0.5015, 0.0323),
    7072:PolarizationRecord(7072, 'final-april', 0.5519, 0.0312, 0.6420, 0.0473),
    7074:PolarizationRecord(7074, 'final-april', 0.5336, 0.0363, 0.6550, 0.0480),
    7075:PolarizationRecord(7075, 'final-april', 0.5158, 0.0351, 0.5690, 0.0427),
    7079:PolarizationRecord(7079, 'final-april', 0.5107, 0.0257, 0.5269, 0.0336),
    7084:PolarizationRecord(7084, 'final-april', 0.5166, 0.0282, 0.4935, 0.0391),
    7085:PolarizationRecord(7085, 'final-april', 0.5122, 0.0263, 0.4424, 0.0305),
    7086:PolarizationRecord(7086, 'final-april', 0.5000, 0.0259, 0.5310, 0.0337),
    7087:PolarizationRecord(7087, 'final-april', 0.5069, 0.0363, 0.5410, 0.0427),
    7088:PolarizationRecord(7088, 'final-april', 0.4930, 0.0257, 0.4684, 0.0304),
    7092:PolarizationRecord(7092, 'final-april', 0.4715, 0.0269, 0.5342, 0.0354),
    7102:PolarizationRecord(7102, 'final-april', 0.4516, 0.0239, 0.5213, 0.0333),
    7103:PolarizationRecord(7103, 'final-april', 0.5296, 0.0287, 0.5201, 0.0346),
    7110:PolarizationRecord(7110, 'final-april', 0.5001, 0.0266, 0.4804, 0.0312),
    7112:PolarizationRecord(7112, 'final-april', 0.5201, 0.0273, 0.4879, 0.0321),
    7114:PolarizationRecord(7114, 'final-april', 0.5567, 0.0316, 0.4269, 0.0292),
    7118:PolarizationRecord(7118, 'final-april', 0.5339, 0.0311, 0.5633, 0.0386),
    7120:PolarizationRecord(7120, 'final-april', 0.5794, 0.0297, 0.4535, 0.0308),
    7122:PolarizationRecord(7122, 'final-april', 0.5815, 0.0298, 0.4433, 0.0331),
    7123:PolarizationRecord(7123, 'final-april', 0.5403, 0.0285, 0.4523, 0.0353),
    7124:PolarizationRecord(7124, 'final-april', 0.5549, 0.0280, 0.4877, 0.0332),
    7125:PolarizationRecord(7125, 'final-april', 0.5199, 0.0273, 0.4853, 0.0347),
    7128:PolarizationRecord(7128, 'final-april', 0.5454, 0.0277, 0.5361, 0.0347),
    7129:PolarizationRecord(7129, 'final-april', 0.5890, 0.0331, 0.4902, 0.0338),
    7131:PolarizationRecord(7131, 'final-april', 0.5965, 0.0298, 0.5407, 0.0371),
    7133:PolarizationRecord(7133, 'final-april', 0.4907, 0.0262, 0.5626, 0.0379),
    7134:PolarizationRecord(7134, 'final-april', 0.5825, 0.0296, 0.5630, 0.0360),
    7136:PolarizationRecord(7136, 'final-april', 0.5644, 0.0321, 0.5667, 0.0387),
    7137:PolarizationRecord(7137, 'final-april', 0.5930, 0.0382, 0.5310, 0.0416),
    7138:PolarizationRecord(7138, 'final-april', 0.5574, 0.0320, 0.5389, 0.0375),
    7139:PolarizationRecord(7139, 'final-april', 0.5910, 0.0382, 0.6070, 0.0445),
    7142:PolarizationRecord(7142, 'final-april', 0.5152, 0.0303, 0.5030, 0.0355),
    7143:PolarizationRecord(7143, 'final-april', 0.5386, 0.0280, 0.5405, 0.0368),
    7151:PolarizationRecord(7151, 'final-april', 0.5499, 0.0284, 0.4546, 0.0321),
    7153:PolarizationRecord(7153, 'final-april', 0.5350, 0.0272, 0.4353, 0.0287),
    7154:PolarizationRecord(7154, 'final-april', 0.4992, 0.0295, 0.4088, 0.0309),
    7161:PolarizationRecord(7161, 'final-april', 0.5255, 0.0269, 0.3673, 0.0260),
    7162:PolarizationRecord(7162, 'final-april', 0.5573, 0.0288, 0.4793, 0.0316),
    7163:PolarizationRecord(7163, 'final-april', 0.5589, 0.0319, 0.4723, 0.0341),
    7164:PolarizationRecord(7164, 'final-april', 0.5076, 0.0280, 0.5134, 0.0343),
    7165:PolarizationRecord(7165, 'final-april', 0.5144, 0.0302, 0.4938, 0.0350),
    7166:PolarizationRecord(7166, 'final-april', 0.5266, 0.0286, 0.5287, 0.0346),
    7172:PolarizationRecord(7172, 'final-april', 0.5921, 0.0295, 0.4926, 0.0317),
    7232:PolarizationRecord(7232, 'final-april', 0.4265, 0.0275, 0.3633, 0.0288),
    7233:PolarizationRecord(7233, 'final-april', 0.5168, 0.0282, 0.4566, 0.0312),
    7234:PolarizationRecord(7234, 'final-april', 0.5300, 0.0310, 0.4840, 0.0388),
    7236:PolarizationRecord(7236, 'final-april', 0.4970, 0.0352, 0.4150, 0.0517),
    7237:PolarizationRecord(7237, 'final-april', 0.5298, 0.0271, 0.4314, 0.0291),
    7238:PolarizationRecord(7238, 'final-april', 0.5160, 0.0303, 0.4481, 0.0362),
    7240:PolarizationRecord(7240, 'final-april', 0.4524, 0.0262, 0.3960, 0.0294),
    7241:PolarizationRecord(7241, 'final-april', 0.5570, 0.0296, 0.4918, 0.0376),
    7245:PolarizationRecord(7245, 'final-april', 0.4652, 0.0290, 0.5000, 0.0395),
    7249:PolarizationRecord(7249, 'final-april', 0.5196, 0.0266, 0.4488, 0.0435),
    7250:PolarizationRecord(7250, 'final-april', 0.5554, 0.0377, 0.5320, 0.0409),
    7253:PolarizationRecord(7253, 'final-april', 0.5811, 0.0328, 0.4971, 0.0350),
    7255:PolarizationRecord(7255, 'final-april', 0.5396, 0.0303, 0.5513, 0.0410),
    7263:PolarizationRecord(7263, 'final-april', 0.5241, 0.0266, 0.4572, 0.0313),
    7264:PolarizationRecord(7264, 'final-april', 0.5267, 0.0354, 0.5600, 0.0425),
    7265:PolarizationRecord(7265, 'final-april', 0.5400, 0.0281, 0.5491, 0.0418),
    7266:PolarizationRecord(7266, 'final-april', 0.4821, 0.0291, 0.5079, 0.0516),
    7269:PolarizationRecord(7269, 'final-april', 0.5116, 0.0304, 0.5172, 0.0446),
    7270:PolarizationRecord(7270, 'final-april', 0.4982, 0.0266, 0.5198, 0.0356),
    7271:PolarizationRecord(7271, 'final-april', 0.4623, 0.0335, 0.5330, 0.0447),
    7272:PolarizationRecord(7272, 'final-april', 0.4898, 0.0260, 0.4991, 0.0330),
    7274:PolarizationRecord(7274, 'final-april', 0.4676, 0.0245, 0.4704, 0.0313),
    7276:PolarizationRecord(7276, 'final-april', 0.4767, 0.0273, 0.4824, 0.0372),
    7278:PolarizationRecord(7278, 'final-april', 0.4781, 0.0248, 0.4140, 0.0353),
    7279:PolarizationRecord(7279, 'final-april', 0.4811, 0.0249, 0.4572, 0.0429),
    7293:PolarizationRecord(7293, 'final-april', 0.4966, 0.0275, 0.5384, 0.0355),
    7294:PolarizationRecord(7294, 'final-april', 0.4748, 0.0269, 0.4813, 0.0858),
    7295:PolarizationRecord(7295, 'final-april', 0.4755, 0.0244, 0.5025, 0.0323),
    7296:PolarizationRecord(7296, 'final-april', 0.4695, 0.0233, 0.5206, 0.0322),
    7300:PolarizationRecord(7300, 'final-april', 0.4859, 0.0248, 0.5164, 0.0326),
    7301:PolarizationRecord(7301, 'final-april', 0.4703, 0.0241, 0.4997, 0.0317),
    7302:PolarizationRecord(7302, 'final-april', 0.4826, 0.0238, 0.5110, 0.0317),
    7303:PolarizationRecord(7303, 'final-april', 0.4327, 0.0226, 0.4014, 0.0274),
    7304:PolarizationRecord(7304, 'final-april', 0.4676, 0.0232, 0.5096, 0.0316),
    7305:PolarizationRecord(7305, 'final-april', 0.4528, 0.0229, 0.5130, 0.0323),
    7306:PolarizationRecord(7306, 'final-april', 0.4761, 0.0243, 0.4956, 0.0314),
    7308:PolarizationRecord(7308, 'final-april', 0.4526, 0.0243, 0.4917, 0.0318),
    7311:PolarizationRecord(7311, 'final-april', 0.4679, 0.0254, 0.4386, 0.0486),
    7317:PolarizationRecord(7317, 'final-april', 0.4683, 0.0242, 0.5020, 0.0351),
    7318:PolarizationRecord(7318, 'final-april', 0.4258, 0.0220, 0.5232, 0.0330),
    7319:PolarizationRecord(7319, 'final-april', 0.4782, 0.0245, 0.4981, 0.0326),
    7320:PolarizationRecord(7320, 'final-april', 0.4467, 0.0241, 0.5027, 0.0340),
    7325:PolarizationRecord(7325, 'final-april', 0.4898, 0.0257, 0.4709, 0.0500)
    }
    Final = {
    6928 : PolarizationRecord(6928, 'final', 43.06, 2.67, 48.56, 3.11),
    6931 : PolarizationRecord(6931, 'final', 43.75, 2.95, 51.68, 3.48),
    6936 : PolarizationRecord(6936, 'final', 51.49, 3.14, 48.97, 3.64),
    6943 : PolarizationRecord(6943, 'final', 51.64, 2.53, 48.62, 3.32),
    6945 : PolarizationRecord(6945, 'final', 50.67, 2.50, 46.00, 3.05),
    6947 : PolarizationRecord(6947, 'final', 48.12, 2.45, 43.96, 2.91),
    6955 : PolarizationRecord(6955, 'final', 53.80, 4.09, 54.68, 4.66),
    6957 : PolarizationRecord(6957, 'final', 48.62, 2.59, 45.88, 3.04),
    6959 : PolarizationRecord(6959, 'final', 48.59, 2.49, 46.77, 3.03),
    6963 : PolarizationRecord(6963, 'final', 48.52, 2.60, 42.22, 2.93),
    6966 : PolarizationRecord(6966, 'final', 56.03, 3.93, 47.31, 4.10),
    6967 : PolarizationRecord(6967, 'final', 56.36, 2.90, 43.72, 2.94),
    6968 : PolarizationRecord(6968, 'final', 44.13, 2.51, 45.19, 3.18),
    6969 : PolarizationRecord(6969, 'final', 54.31, 2.77, 43.16, 2.92),
    6971 : PolarizationRecord(6971, 'final', 51.43, 2.79, 47.00, 3.20),
    6972 : PolarizationRecord(6972, 'final', 44.85, 2.39, 49.04, 3.37),
    6973 : PolarizationRecord(6973, 'final', 40.65, 2.24, 43.28, 3.17),
    6975 : PolarizationRecord(6975, 'final', 44.61, 2.44, 41.34, 2.88),
    6980 : PolarizationRecord(6980, 'final', 48.63, 2.82, 49.94, 3.72),
    6988 : PolarizationRecord(6988, 'final', 49.98, 2.64, 46.47, 3.48),
    6990 : PolarizationRecord(6990, 'final', 48.55, 2.66, 46.30, 3.15),
    6991 : PolarizationRecord(6991, 'final', 47.50, 3.02, 39.83, 4.86),
    6992 : PolarizationRecord(6992, 'final', 44.76, 2.44, 45.39, 3.39),
    6994 : PolarizationRecord(6994, 'final', 45.22, 2.55, 45.05, 3.57),
    6995 : PolarizationRecord(6995, 'final', 51.08, 2.91, 47.05, 3.70),
    6997 : PolarizationRecord(6997, 'final', 45.51, 2.55, 46.30, 3.31),
    6998 : PolarizationRecord(6998, 'final', 48.93, 2.81, 43.85, 3.53),
    6999 : PolarizationRecord(6999, 'final', 48.51, 2.53, 51.88, 3.32),
    7001 : PolarizationRecord(7001, 'final', 46.36, 2.58, 46.22, 3.05),
    7002 : PolarizationRecord(7002, 'final', 46.09, 2.97, 50.78, 7.88),
    7007 : PolarizationRecord(7007, 'final', 46.35, 2.59, 48.74, 3.28),
    7010 : PolarizationRecord(7010, 'final', 51.13, 2.60, 52.25, 3.34),
    7028 : PolarizationRecord(7028, 'final', 46.57, 2.35, 46.71, 2.97),
    7029 : PolarizationRecord(7029, 'final', 48.58, 2.68, 49.92, 3.44),
    7030 : PolarizationRecord(7030, 'final', 59.21, 2.92, 52.90, 3.40),
    7032 : PolarizationRecord(7032, 'final', 53.55, 2.77, 48.21, 3.16),
    7034 : PolarizationRecord(7034, 'final', 51.16, 2.60, 43.83, 2.94),
    7035 : PolarizationRecord(7035, 'final', 45.39, 2.38, 46.65, 3.07),
    7036 : PolarizationRecord(7036, 'final', 49.90, 2.80, 48.50, 3.34),
    7037 : PolarizationRecord(7037, 'final', 49.64, 3.68, 48.93, 3.96),
    7039 : PolarizationRecord(7039, 'final', 49.31, 3.05, 49.20, 3.50),
    7046 : PolarizationRecord(7046, 'final', 64.55, 3.17, 45.99, 4.36),
    7049 : PolarizationRecord(7049, 'final', 50.81, 2.53, 45.77, 2.85),
    7051 : PolarizationRecord(7051, 'final', 51.08, 2.86, 39.48, 2.88),
    7059 : PolarizationRecord(7059, 'final', 56.35, 3.27, 48.43, 3.76),
    7063 : PolarizationRecord(7063, 'final', 54.18, 3.19, 47.65, 3.52),
    7064 : PolarizationRecord(7064, 'final', 52.37, 2.71, 38.58, 2.62),
    7067 : PolarizationRecord(7067, 'final', 54.67, 2.81, 43.44, 3.17),
    7068 : PolarizationRecord(7068, 'final', 51.29, 2.62, 38.59, 5.47),
    7069 : PolarizationRecord(7069, 'final', 56.66, 3.01, 54.62, 5.09),
    7070 : PolarizationRecord(7070, 'final', 52.51, 2.66, 49.40, 3.19),
    7072 : PolarizationRecord(7072, 'final', 55.02, 3.12, 63.08, 4.66),
    7074 : PolarizationRecord(7074, 'final', 53.21, 3.65, 65.03, 4.78),
    7075 : PolarizationRecord(7075, 'final', 51.46, 3.54, 56.14, 4.24),
    7079 : PolarizationRecord(7079, 'final', 50.93, 2.57, 51.94, 3.32),
    7084 : PolarizationRecord(7084, 'final', 51.53, 2.82, 48.73, 3.86),
    7085 : PolarizationRecord(7085, 'final', 51.11, 2.63, 43.83, 3.03),
    7086 : PolarizationRecord(7086, 'final', 49.87, 2.58, 52.48, 3.34),
    7087 : PolarizationRecord(7087, 'final', 50.59, 3.65, 53.40, 4.24),
    7088 : PolarizationRecord(7088, 'final', 49.21, 2.56, 46.20, 3.01),
    7092 : PolarizationRecord(7092, 'final', 47.03, 2.67, 52.47, 3.49),
    7102 : PolarizationRecord(7102, 'final', 44.95, 2.39, 51.25, 3.28),
    7103 : PolarizationRecord(7103, 'final', 52.79, 2.87, 51.35, 3.43),
    7110 : PolarizationRecord(7110, 'final', 49.87, 2.64, 47.21, 3.08),
    7112 : PolarizationRecord(7112, 'final', 51.90, 2.73, 48.15, 3.16),
    7114 : PolarizationRecord(7114, 'final', 55.52, 3.15, 41.94, 2.88),
    7118 : PolarizationRecord(7118, 'final', 53.19, 3.10, 55.08, 3.80),
    7120 : PolarizationRecord(7120, 'final', 57.75, 2.96, 44.76, 3.04),
    7122 : PolarizationRecord(7122, 'final', 58.01, 2.97, 43.86, 3.28),
    7123 : PolarizationRecord(7123, 'final', 53.84, 2.84, 44.59, 3.49),
    7124 : PolarizationRecord(7124, 'final', 55.29, 2.79, 47.92, 3.28),
    7125 : PolarizationRecord(7125, 'final', 51.86, 2.72, 48.11, 3.45),
    7128 : PolarizationRecord(7128, 'final', 54.35, 2.76, 52.49, 3.41),
    7129 : PolarizationRecord(7129, 'final', 58.74, 3.30, 47.59, 3.31),
    7131 : PolarizationRecord(7131, 'final', 59.47, 2.97, 53.04, 3.64),
    7133 : PolarizationRecord(7133, 'final', 48.94, 2.61, 55.56, 3.75),
    7134 : PolarizationRecord(7134, 'final', 58.06, 2.96, 55.39, 3.56),
    7136 : PolarizationRecord(7136, 'final', 56.30, 3.21, 55.80, 3.81),
    7137 : PolarizationRecord(7137, 'final', 59.16, 3.82, 52.31, 4.11),
    7138 : PolarizationRecord(7138, 'final', 55.58, 3.19, 53.77, 3.74),
    7139 : PolarizationRecord(7139, 'final', 58.89, 3.83, 59.60, 4.41),
    7142 : PolarizationRecord(7142, 'final', 51.41, 3.02, 49.74, 3.53),
    7143 : PolarizationRecord(7143, 'final', 53.74, 2.80, 53.04, 3.62),
    7151 : PolarizationRecord(7151, 'final', 54.85, 2.83, 44.70, 3.16),
    7153 : PolarizationRecord(7153, 'final', 53.36, 2.71, 43.07, 2.85),
    7154 : PolarizationRecord(7154, 'final', 49.81, 2.96, 40.54, 3.09),
    7161 : PolarizationRecord(7161, 'final', 52.38, 2.67, 35.92, 2.56),
    7162 : PolarizationRecord(7162, 'final', 55.57, 2.87, 47.12, 3.12),
    7163 : PolarizationRecord(7163, 'final', 55.73, 3.18, 46.50, 3.39),
    7164 : PolarizationRecord(7164, 'final', 50.59, 2.79, 50.31, 3.37),
    7165 : PolarizationRecord(7165, 'final', 51.28, 3.03, 48.59, 3.46),
    7166 : PolarizationRecord(7166, 'final', 52.56, 2.86, 52.21, 3.43),
    7172 : PolarizationRecord(7172, 'final', 59.05, 2.94, 48.61, 3.13),
    7232 : PolarizationRecord(7232, 'final', 42.54, 2.73, 35.97, 2.87),
    7233 : PolarizationRecord(7233, 'final', 51.56, 2.82, 45.01, 3.10),
    7234 : PolarizationRecord(7234, 'final', 52.83, 3.10, 47.51, 3.83),
    7236 : PolarizationRecord(7236, 'final', 49.55, 3.55, 40.84, 5.12),
    7237 : PolarizationRecord(7237, 'final', 52.85, 2.70, 42.15, 2.86),
    7238 : PolarizationRecord(7238, 'final', 51.51, 3.03, 44.22, 3.62),
    7240 : PolarizationRecord(7240, 'final', 45.17, 2.62, 38.92, 2.92),
    7241 : PolarizationRecord(7241, 'final', 55.56, 2.96, 48.46, 3.71),
    7245 : PolarizationRecord(7245, 'final', 46.39, 2.89, 49.11, 3.91),
    7249 : PolarizationRecord(7249, 'final', 51.80, 2.65, 43.86, 4.27),
    7250 : PolarizationRecord(7250, 'final', 55.34, 3.74, 52.11, 4.08),
    7253 : PolarizationRecord(7253, 'final', 57.92, 3.27, 49.15, 3.46),
    7255 : PolarizationRecord(7255, 'final', 53.82, 3.02, 54.12, 4.04),
    7263 : PolarizationRecord(7263, 'final', 52.24, 2.66, 45.19, 3.10),
    7264 : PolarizationRecord(7264, 'final', 52.54, 3.57, 55.10, 4.19),
    7265 : PolarizationRecord(7265, 'final', 53.77, 2.79, 53.96, 4.12),
    7266 : PolarizationRecord(7266, 'final', 48.10, 2.90, 50.03, 5.10),
    7269 : PolarizationRecord(7269, 'final', 51.11, 3.02, 51.27, 4.44),
    7270 : PolarizationRecord(7270, 'final', 49.64, 2.65, 51.20, 3.51),
    7271 : PolarizationRecord(7271, 'final', 46.13, 3.34, 52.55, 4.41),
    7272 : PolarizationRecord(7272, 'final', 48.88, 2.60, 49.17, 3.25),
    7274 : PolarizationRecord(7274, 'final', 46.64, 2.44, 46.05, 3.07),
    7276 : PolarizationRecord(7276, 'final', 47.57, 2.73, 47.43, 3.66),
    7278 : PolarizationRecord(7278, 'final', 47.71, 2.47, 40.75, 3.49),
    7279 : PolarizationRecord(7279, 'final', 47.95, 2.48, 44.85, 4.23),
    7293 : PolarizationRecord(7293, 'final', 49.55, 2.75, 53.08, 3.51),
    7294 : PolarizationRecord(7294, 'final', 47.39, 2.68, 47.31, 8.44),
    7295 : PolarizationRecord(7295, 'final', 47.44, 2.43, 49.65, 3.20),
    7296 : PolarizationRecord(7296, 'final', 46.85, 2.32, 51.24, 3.17),
    7300 : PolarizationRecord(7300, 'final', 48.40, 2.47, 50.85, 3.22),
    7301 : PolarizationRecord(7301, 'final', 46.87, 2.41, 49.00, 3.11),
    7302 : PolarizationRecord(7302, 'final', 48.12, 2.37, 50.32, 3.12),
    7303 : PolarizationRecord(7303, 'final', 43.13, 2.26, 39.51, 2.71),
    7304 : PolarizationRecord(7304, 'final', 46.63, 2.31, 49.99, 3.10),
    7305 : PolarizationRecord(7305, 'final', 45.12, 2.28, 50.65, 3.20),
    7306 : PolarizationRecord(7306, 'final', 47.53, 2.42, 48.66, 3.09),
    7308 : PolarizationRecord(7308, 'final', 45.12, 2.42, 48.66, 3.16),
    7311 : PolarizationRecord(7311, 'final', 46.65, 2.53, 44.95, 4.90),
    7317 : PolarizationRecord(7317, 'final', 46.67, 2.40, 49.42, 3.47),
    7318 : PolarizationRecord(7318, 'final', 42.48, 2.19, 51.45, 3.26),
    7319 : PolarizationRecord(7319, 'final', 47.69, 2.45, 49.06, 3.22),
    7320 : PolarizationRecord(7320, 'final', 44.51, 2.40, 49.49, 3.38),
    7325 : PolarizationRecord(7325, 'final', 48.83, 2.56, 46.38, 4.93),
    7602 : PolarizationRecord(7602, 'final',    0.4113, 0.0, 0.3999, 0.0),
    7603 : PolarizationRecord(7603, 'final',    0.4354, 0.0, 0.3987, 0.0),
    7605 : PolarizationRecord(7605, 'final',    0.4646, 0.0, 0.4755, 0.0),
    7609 : PolarizationRecord(7609, 'final',    0.5372, 0.0, 0.4841, 0.0),
    7610 : PolarizationRecord(7610, 'final',    0.4818, 0.0, 0.4044, 0.0),
    7621 : PolarizationRecord(7621, 'final',    0.4966, 0.0, 0.4966, 0.0),
    7622 : PolarizationRecord(7622, 'final',    0.4691, 0.0, 0.5161, 0.0),
    7627 : PolarizationRecord(7627, 'final',    0.4876, 0.0, 0.4903, 0.0),
    7630 : PolarizationRecord(7630, 'final',    0.4997, 0.0, 0.4818, 0.0),
    7632 : PolarizationRecord(7632, 'final',    0.4581, 0.0, 0.4871, 0.0),
    7637 : PolarizationRecord(7637, 'final',    0.5219, 0.0, 0.5329, 0.0),
    7639 : PolarizationRecord(7639, 'final',    0.5440, 0.0, 0.5306, 0.0),
    7641 : PolarizationRecord(7641, 'final',    0.5340, 0.0, 0.5153, 0.0),
    7642 : PolarizationRecord(7642, 'final',    0.5401, 0.0, 0.5015, 0.0),
    7645 : PolarizationRecord(7645, 'final',    0.5493, 0.0, 0.5174, 0.0),
    7646 : PolarizationRecord(7646, 'final',    0.5149, 0.0, 0.4180, 0.0),
    7651 : PolarizationRecord(7651, 'final',    0.5283, 0.0, 0.5029, 0.0),
    7652 : PolarizationRecord(7652, 'final',    0.4934, 0.0, 0.5025, 0.0),
    7654 : PolarizationRecord(7654, 'final',    0.5437, 0.0, 0.5437, 0.0),
    7655 : PolarizationRecord(7655, 'final',    0.5758, 0.0, 0.4910, 0.0),
    7657 : PolarizationRecord(7657, 'final',    0.5403, 0.0, 0.5460, 0.0),
    7658 : PolarizationRecord(7658, 'final',    0.4989, 0.0, 0.4416, 0.0),
    7662 : PolarizationRecord(7662, 'final',    0.5489, 0.0, 0.5534, 0.0),
    7671 : PolarizationRecord(7671, 'final',    0.5224, 0.0, 0.5224, 0.0),
    7672 : PolarizationRecord(7672, 'final',    0.5197, 0.0, 0.4147, 0.0),
    7673 : PolarizationRecord(7673, 'final',    0.4166, 0.0, 0.2946, 0.0),
    7674 : PolarizationRecord(7674, 'final',    0.5323, 0.0, 0.4578, 0.0),
    7681 : PolarizationRecord(7681, 'final',    0.3936, 0.0, 0.4179, 0.0),
    7682 : PolarizationRecord(7682, 'final',    0.4841, 0.0, 0.5035, 0.0),
    7685 : PolarizationRecord(7685, 'final',    0.4813, 0.0, 0.5314, 0.0),
    7688 : PolarizationRecord(7688, 'final',    0.5078, 0.0, 0.4647, 0.0),
    7689 : PolarizationRecord(7689, 'final',    0.5305, 0.0, 0.5617, 0.0),
    7690 : PolarizationRecord(7690, 'final',    0.4802, 0.0, 0.5587, 0.0),
    7691 : PolarizationRecord(7691, 'final',    0.5580, 0.0, 0.5492, 0.0),
    7697 : PolarizationRecord(7697, 'final',    0.5429, 0.0, 0.5402, 0.0),
    7699 : PolarizationRecord(7699, 'final',    0.4802, 0.0, 0.5438, 0.0),
    7710 : PolarizationRecord(7710, 'final',    0.4959, 0.0, 0.5491, 0.0),
    7711 : PolarizationRecord(7711, 'final',    0.5424, 0.0, 0.3652, 0.0),
    7712 : PolarizationRecord(7712, 'final',    0.5187, 0.0, 0.5155, 0.0),
    7718 : PolarizationRecord(7718, 'final',    0.5456, 0.0, 0.5389, 0.0),
    7722 : PolarizationRecord(7722, 'final',    0.5398, 0.0, 0.5460, 0.0),
    7724 : PolarizationRecord(7724, 'final',    0.4914, 0.0, 0.5019, 0.0),
    7725 : PolarizationRecord(7725, 'final',    0.5193, 0.0, 0.5321, 0.0),
    7729 : PolarizationRecord(7729, 'final',    0.5320, 0.0, 0.5337, 0.0),
    7739 : PolarizationRecord(7739, 'final',    0.6047, 0.0, 0.5586, 0.0),
    7740 : PolarizationRecord(7740, 'final',    0.5470, 0.0, 0.5714, 0.0),
    7744 : PolarizationRecord(7744, 'final',    0.5847, 0.0, 0.5490, 0.0),
    7745 : PolarizationRecord(7745, 'final',    0.5375, 0.0, 0.5631, 0.0),
    7753 : PolarizationRecord(7753, 'final',    0.5346, 0.0, 0.4901, 0.0),
    7756 : PolarizationRecord(7756, 'final',    0.4981, 0.0, 0.5695, 0.0),
    7757 : PolarizationRecord(7757, 'final',    0.5274, 0.0, 0.5772, 0.0),
    7766 : PolarizationRecord(7766, 'final',    0.4093, 0.0, 0.4483, 0.0),
    7772 : PolarizationRecord(7772, 'final',    0.5717, 0.0, 0.5486, 0.0),
    7775 : PolarizationRecord(7775, 'final',    0.5942, 0.0, 0.5513, 0.0),
    7777 : PolarizationRecord(7777, 'final',    0.5892, 0.0, 0.5468, 0.0),
    7780 : PolarizationRecord(7780, 'final',    0.5558, 0.0, 0.5146, 0.0),
    7781 : PolarizationRecord(7781, 'final',    0.5241, 0.0, 0.5010, 0.0),
    7784 : PolarizationRecord(7784, 'final',    0.5517, 0.0, 0.5421, 0.0),
    7785 : PolarizationRecord(7785, 'final',    0.5924, 0.0, 0.5309, 0.0),
    7786 : PolarizationRecord(7786, 'final',    0.6245, 0.0, 0.5720, 0.0),
    7788 : PolarizationRecord(7788, 'final',    0.5441, 0.0, 0.5308, 0.0),
    7789 : PolarizationRecord(7789, 'final',    0.5557, 0.0, 0.5589, 0.0),
    7790 : PolarizationRecord(7790, 'final',    0.5857, 0.0, 0.5342, 0.0),
    7791 : PolarizationRecord(7791, 'final',    0.5586, 0.0, 0.5380, 0.0),
    7792 : PolarizationRecord(7792, 'final',    0.5280, 0.0, 0.5313, 0.0),
    7794 : PolarizationRecord(7794, 'final',    0.5799, 0.0, 0.5548, 0.0),
    7795 : PolarizationRecord(7795, 'final',    0.5601, 0.0, 0.5244, 0.0),
    7796 : PolarizationRecord(7796, 'final',    0.5954, 0.0, 0.6170, 0.0),
    7797 : PolarizationRecord(7797, 'final',    0.6090, 0.0, 0.5627, 0.0),
    7800 : PolarizationRecord(7800, 'final',    0.5286, 0.0, 0.5575, 0.0),
    7801 : PolarizationRecord(7801, 'final',    0.5601, 0.0, 0.5988, 0.0),
    7803 : PolarizationRecord(7803, 'final',    0.5552, 0.0, 0.5867, 0.0),
    7804 : PolarizationRecord(7804, 'final',    0.5440, 0.0, 0.5440, 0.0),
    7805 : PolarizationRecord(7805, 'final',    0.5306, 0.0, 0.5614, 0.0),
    7810 : PolarizationRecord(7810, 'final',    0.5617, 0.0, 0.5419, 0.0),
    7811 : PolarizationRecord(7811, 'final',    0.5540, 0.0, 0.5706, 0.0),
    7812 : PolarizationRecord(7812, 'final',    0.5267, 0.0, 0.5596, 0.0),
    7815 : PolarizationRecord(7815, 'final',    0.5294, 0.0, 0.5451, 0.0),
    7817 : PolarizationRecord(7817, 'final',    0.5302, 0.0, 0.5668, 0.0),
    7820 : PolarizationRecord(7820, 'final',    0.5248, 0.0, 0.5742, 0.0),
    7823 : PolarizationRecord(7823, 'final',    0.5415, 0.0, 0.5737, 0.0),
    7824 : PolarizationRecord(7824, 'final',    0.5153, 0.0, 0.5807, 0.0),
    7825 : PolarizationRecord(7825, 'final',    0.5623, 0.0, 0.5687, 0.0),
    7826 : PolarizationRecord(7826, 'final',    0.5357, 0.0, 0.5633, 0.0),
    7827 : PolarizationRecord(7827, 'final',    0.5374, 0.0, 0.5655, 0.0),
    7830 : PolarizationRecord(7830, 'final',    0.5121, 0.0, 0.5986, 0.0),
    7831 : PolarizationRecord(7831, 'final',    0.5424, 0.0, 0.5761, 0.0),
    7833 : PolarizationRecord(7833, 'final',    0.5288, 0.0, 0.5813, 0.0),
    7847 : PolarizationRecord(7847, 'final',    0.5083, 0.0, 0.4929, 0.0),
    7850 : PolarizationRecord(7850, 'final',    0.5479, 0.0, 0.5708, 0.0),
    7851 : PolarizationRecord(7851, 'final',    0.5917, 0.0, 0.5583, 0.0),
    7852 : PolarizationRecord(7852, 'final',    0.5715, 0.0, 0.5557, 0.0),
    7853 : PolarizationRecord(7853, 'final',    0.5544, 0.0, 0.5921, 0.0),
    7855 : PolarizationRecord(7855, 'final',    0.6194, 0.0, 0.5783, 0.0),
    7856 : PolarizationRecord(7856, 'final',    0.5806, 0.0, 0.5832, 0.0),
    7860 : PolarizationRecord(7860, 'final',    0.6219, 0.0, 0.6119, 0.0),
    7863 : PolarizationRecord(7863, 'final',    0.6130, 0.0, 0.5890, 0.0),
    7864 : PolarizationRecord(7864, 'final',    0.6211, 0.0, 0.5894, 0.0),
    7865 : PolarizationRecord(7865, 'final',    0.5200, 0.0, 0.5687, 0.0),
    7871 : PolarizationRecord(7871, 'final',    0.6260, 0.0, 0.6159, 0.0),
    7872 : PolarizationRecord(7872, 'final',    0.5575, 0.0, 0.6248, 0.0),
    7883 : PolarizationRecord(7883, 'final',    0.5497, 0.0, 0.5704, 0.0),
    7886 : PolarizationRecord(7886, 'final',    0.5163, 0.0, 0.5801, 0.0),
    7887 : PolarizationRecord(7887, 'final',    0.6014, 0.0, 0.6258, 0.0),
    7889 : PolarizationRecord(7889, 'final',    0.6076, 0.0, 0.5639, 0.0),
    7890 : PolarizationRecord(7890, 'final',    0.5699, 0.0, 0.6154, 0.0),
    7891 : PolarizationRecord(7891, 'final',    0.6030, 0.0, 0.5864, 0.0),
    7892 : PolarizationRecord(7892, 'final',    0.5958, 0.0, 0.6273, 0.0),
    7893 : PolarizationRecord(7893, 'final',    0.6213, 0.0, 0.6233, 0.0),
    7896 : PolarizationRecord(7896, 'final',    0.5847, 0.0, 0.6059, 0.0),
    7898 : PolarizationRecord(7898, 'final',    0.5779, 0.0, 0.5686, 0.0),
    7901 : PolarizationRecord(7901, 'final',    0.6184, 0.0, 0.5897, 0.0),
    7908 : PolarizationRecord(7908, 'final',    0.5077, 0.0, 0.6011, 0.0),
    7909 : PolarizationRecord(7909, 'final',    0.5300, 0.0, 0.5980, 0.0),
    7911 : PolarizationRecord(7911, 'final',    0.5184, 0.0, 0.6101, 0.0),
    7913 : PolarizationRecord(7913, 'final',    0.6044, 0.0, 0.6052, 0.0),
    7915 : PolarizationRecord(7915, 'final',    0.5644, 0.0, 0.6081, 0.0),
    7916 : PolarizationRecord(7916, 'final',    0.4892, 0.0, 0.6135, 0.0),
    7918 : PolarizationRecord(7918, 'final',    0.5480, 0.0, 0.5825, 0.0),
    7921 : PolarizationRecord(7921, 'final',    0.5779, 0.0, 0.5640, 0.0),
    7922 : PolarizationRecord(7922, 'final',    0.5631, 0.0, 0.5755, 0.0),
    7926 : PolarizationRecord(7926, 'final',    0.5777, 0.0, 0.5882, 0.0),
    7940 : PolarizationRecord(7940, 'final',    0.5200, 0.0, 0.5185, 0.0),
    7944 : PolarizationRecord(7944, 'final',    0.5522, 0.0, 0.5565, 0.0),
    7946 : PolarizationRecord(7946, 'final',    0.5323, 0.0, 0.6185, 0.0),
    7949 : PolarizationRecord(7949, 'final',    0.4963, 0.0, 0.5488, 0.0),
    7951 : PolarizationRecord(7951, 'final',    0.5509, 0.0, 0.5945, 0.0),
    7952 : PolarizationRecord(7952, 'final',    0.5183, 0.0, 0.5603, 0.0),
    7954 : PolarizationRecord(7954, 'final',    0.4989, 0.0, 0.5814, 0.0),
    7957 : PolarizationRecord(7957, 'final',    0.5202, 0.0, 0.5997, 0.0),
    7998 : PolarizationRecord(7998, 'final',    0.5434, 0.0, 0.5211, 0.0),
    8003 : PolarizationRecord(8003, 'final',    0.4203, 0.0, 0.5367, 0.0),
    8005 : PolarizationRecord(8005, 'final',    0.4541, 0.0, 0.5523, 0.0),
    8008 : PolarizationRecord(8008, 'final',    0.5258, 0.0, 0.5644, 0.0),
    8014 : PolarizationRecord(8014, 'final',    0.5094, 0.0, 0.5243, 0.0),
    8015 : PolarizationRecord(8015, 'final',    0.5100, 0.0, 0.4942, 0.0),
    8016 : PolarizationRecord(8016, 'final',    0.4564, 0.0, 0.4839, 0.0),
    8020 : PolarizationRecord(8020, 'final',    0.5220, 0.0, 0.4581, 0.0),
    8021 : PolarizationRecord(8021, 'final',    0.4847, 0.0, 0.4618, 0.0),
    8022 : PolarizationRecord(8022, 'final',    0.4856, 0.0, 0.4515, 0.0),
    8023 : PolarizationRecord(8023, 'final',    0.5242, 0.0, 0.5170, 0.0),
    8024 : PolarizationRecord(8024, 'final',    0.4563, 0.0, 0.4879, 0.0),
    8025 : PolarizationRecord(8025, 'final',    0.4938, 0.0, 0.5106, 0.0),
    8026 : PolarizationRecord(8026, 'final',    0.5161, 0.0, 0.5050, 0.0),
    8031 : PolarizationRecord(8031, 'final',    0.4647, 0.0, 0.4887, 0.0),
    8032 : PolarizationRecord(8032, 'final',    0.4799, 0.0, 0.4694, 0.0),
    8036 : PolarizationRecord(8036, 'final',    0.5358, 0.0, 0.4303, 0.0),
    8047 : PolarizationRecord(8047, 'final',    0.4753, 0.0, 0.4582, 0.0),
    8048 : PolarizationRecord(8048, 'final',    0.5357, 0.0, 0.5232, 0.0),
    8049 : PolarizationRecord(8049, 'final',    0.5076, 0.0, 0.5338, 0.0),
    8052 : PolarizationRecord(8052, 'final',    0.5022, 0.0, 0.5074, 0.0),
    8053 : PolarizationRecord(8053, 'final',    0.5835, 0.0, 0.4798, 0.0),
    8054 : PolarizationRecord(8054, 'final',    0.5077, 0.0, 0.4602, 0.0),
    8055 : PolarizationRecord(8055, 'final',    0.4747, 0.0, 0.5029, 0.0),
    8056 : PolarizationRecord(8056, 'final',    0.4558, 0.0, 0.4867, 0.0),
    8058 : PolarizationRecord(8058, 'final',    0.4724, 0.0, 0.5007, 0.0),
    8059 : PolarizationRecord(8059, 'final',    0.4892, 0.0, 0.4684, 0.0),
    8061 : PolarizationRecord(8061, 'final',    0.5162, 0.0, 0.4525, 0.0)
    }
    Online = {
    6928 : PolarizationRecord(6928, 'online', 0.412, 0.037, 0.433, 0.032),
    6931 : PolarizationRecord(6931, 'online', 0.413, 0.045, 0.475, 0.049),
    6936 : PolarizationRecord(6936, 'online', 0.477, 0.050, 0.416, 0.051),
    6943 : PolarizationRecord(6943, 'online', 0.514, 0.061, 0.516, 0.053),
    6945 : PolarizationRecord(6945, 'online', 0.508, 0.043, 0.435, 0.034),
    6947 : PolarizationRecord(6947, 'online', 0.481, 0.035, 0.415, 0.039),
    6957 : PolarizationRecord(6957, 'online', 0.495, 0.036, 0.376, 0.033),
    6959 : PolarizationRecord(6959, 'online', 0.488, 0.036, 0.413, 0.035),
    6963 : PolarizationRecord(6963, 'online', 0.496, 0.039, 0.375, 0.035),
    6967 : PolarizationRecord(6967, 'online', 0.551, 0.113, 0.451, 0.093),
    6968 : PolarizationRecord(6968, 'online', 0.437, 0.090, 0.407, 0.085),
    6971 : PolarizationRecord(6971, 'online', 0.510, 0.043, 0.420, 0.044),
    6972 : PolarizationRecord(6972, 'online', 0.435, 0.040, 0.427, 0.044),
    6973 : PolarizationRecord(6973, 'online', 0.391, 0.031, 0.375, 0.032),
    6975 : PolarizationRecord(6975, 'online', 0.441, 0.035, 0.346, 0.033),
    6980 : PolarizationRecord(6980, 'online', 0.462, 0.048, 0.417, 0.050),
    6988 : PolarizationRecord(6988, 'online', 0.475, 0.040, 0.399, 0.034),
    6990 : PolarizationRecord(6990, 'online', 0.473, 0.037, 0.400, 0.042),
    6991 : PolarizationRecord(6991, 'online', 0.442, 0.053, 0.347, 0.052),
    6992 : PolarizationRecord(6992, 'online', 0.420, 0.036, 0.387, 0.040),
    6994 : PolarizationRecord(6994, 'online', 0.435, 0.045, 0.390, 0.041),
    6995 : PolarizationRecord(6995, 'online', 0.473, 0.044, 0.360, 0.044),
    6997 : PolarizationRecord(6997, 'online', 0.438, 0.037, 0.402, 0.042),
    6998 : PolarizationRecord(6998, 'online', 0.450, 0.054, 0.348, 0.043),
    6999 : PolarizationRecord(6999, 'online', 0.470, 0.040, 0.381, 0.036),
    7001 : PolarizationRecord(7001, 'online', 0.439, 0.041, 0.400, 0.042),
    7002 : PolarizationRecord(7002, 'online', 0.440, 0.053, 0.430, 0.063),
    7007 : PolarizationRecord(7007, 'online', 0.441, 0.041, 0.485, 0.050),
    7010 : PolarizationRecord(7010, 'online', 0.493, 0.038, 0.502, 0.042),
    7028 : PolarizationRecord(7028, 'online', 0.439, 0.034, 0.476, 0.033),
    7029 : PolarizationRecord(7029, 'online', 0.463, 0.043, 0.462, 0.048),
    7030 : PolarizationRecord(7030, 'online', 0.594, 0.043, 0.476, 0.057),
    7032 : PolarizationRecord(7032, 'online', 0.516, 0.048, 0.480, 0.045),
    7034 : PolarizationRecord(7034, 'online', 0.461, 0.037, 0.429, 0.037),
    7035 : PolarizationRecord(7035, 'online', 0.466, 0.033, 0.455, 0.039),
    7046 : PolarizationRecord(7046, 'online', 0.617, 0.048, 0.429, 0.037),
    7048 : PolarizationRecord(7048, 'online', 0.496, 0.046, 0.387, 0.037),
    7049 : PolarizationRecord(7049, 'online', 0.519, 0.034, 0.457, 0.039),
    7051 : PolarizationRecord(7051, 'online', 0.486, 0.050, 0.368, 0.045),
    7055 : PolarizationRecord(7055, 'online', 0.544, 0.065, 0.433, 0.052),
    7064 : PolarizationRecord(7064, 'online', 0.512, 0.043, 0.446, 0.066),
    7067 : PolarizationRecord(7067, 'online', 0.555, 0.047, 0.435, 0.046),
    7068 : PolarizationRecord(7068, 'online', 0.511, 0.043, 0.371, 0.030),
    7069 : PolarizationRecord(7069, 'online', 0.573, 0.068, 0.498, 0.060),
    7070 : PolarizationRecord(7070, 'online', 0.528, 0.045, 0.434, 0.041),
    7072 : PolarizationRecord(7072, 'online', 0.540, 0.064, 0.592, 0.122),
    7075 : PolarizationRecord(7075, 'online', 0.500, 0.104, 0.530, 0.109),
    7079 : PolarizationRecord(7079, 'online', 0.508, 0.037, 0.494, 0.046),
    7084 : PolarizationRecord(7084, 'online', 0.504, 0.052, 0.470, 0.057),
    7085 : PolarizationRecord(7085, 'online', 0.513, 0.038, 0.414, 0.039),
    7086 : PolarizationRecord(7086, 'online', 0.488, 0.041, 0.512, 0.047),
    7087 : PolarizationRecord(7087, 'online', 0.486, 0.071, 0.500, 0.104),
    7088 : PolarizationRecord(7088, 'online', 0.489, 0.045, 0.442, 0.046),
    7092 : PolarizationRecord(7092, 'online', 0.475, 0.049, 0.488, 0.051),
    7102 : PolarizationRecord(7102, 'online', 0.487, 0.041, 0.499, 0.046),
    7103 : PolarizationRecord(7103, 'online', 0.541, 0.056, 0.504, 0.060),
    7110 : PolarizationRecord(7110, 'online', 0.524, 0.044, 0.452, 0.042),
    7112 : PolarizationRecord(7112, 'online', 0.538, 0.045, 0.461, 0.048),
    7114 : PolarizationRecord(7114, 'online', 0.558, 0.081, 0.375, 0.046),
    7118 : PolarizationRecord(7118, 'online', 0.538, 0.064, 0.524, 0.077),
    7120 : PolarizationRecord(7120, 'online', 0.542, 0.050, 0.386, 0.042),
    7122 : PolarizationRecord(7122, 'online', 0.552, 0.051, 0.439, 0.046),
    7123 : PolarizationRecord(7123, 'online', 0.536, 0.050, 0.446, 0.047),
    7124 : PolarizationRecord(7124, 'online', 0.518, 0.044, 0.399, 0.039),
    7125 : PolarizationRecord(7125, 'online', 0.516, 0.048, 0.435, 0.041),
    7128 : PolarizationRecord(7128, 'online', 0.566, 0.082, 0.535, 0.110),
    7131 : PolarizationRecord(7131, 'online', 0.562, 0.047, 0.492, 0.051),
    7133 : PolarizationRecord(7133, 'online', 0.494, 0.051, 0.516, 0.044),
    7134 : PolarizationRecord(7134, 'online', 0.576, 0.053, 0.521, 0.054),
    7136 : PolarizationRecord(7136, 'online', 0.542, 0.079, 0.535, 0.078),
    7138 : PolarizationRecord(7138, 'online', 0.538, 0.064, 0.530, 0.077),
    7142 : PolarizationRecord(7142, 'online', 0.513, 0.061, 0.484, 0.071),
    7143 : PolarizationRecord(7143, 'online', 0.544, 0.056, 0.519, 0.076),
    7151 : PolarizationRecord(7151, 'online', 0.577, 0.053, 0.421, 0.052),
    7153 : PolarizationRecord(7153, 'online', 0.543, 0.046, 0.434, 0.041),
    7154 : PolarizationRecord(7154, 'online', 0.497, 0.059, 0.389, 0.058),
    7161 : PolarizationRecord(7161, 'online', 0.511, 0.053, 0.349, 0.043),
    7162 : PolarizationRecord(7162, 'online', 0.551, 0.051, 0.444, 0.046),
    7164 : PolarizationRecord(7164, 'online', 0.519, 0.054, 0.483, 0.058),
    7165 : PolarizationRecord(7165, 'online', 0.494, 0.059, 0.457, 0.067),
    7166 : PolarizationRecord(7166, 'online', 0.532, 0.055, 0.497, 0.059),
    7172 : PolarizationRecord(7172, 'online', 0.590, 0.040, 0.450, 0.042),
    7232 : PolarizationRecord(7232, 'online', 0.441, 0.046, 0.338, 0.052),
    7237 : PolarizationRecord(7237, 'online', 0.516, 0.040, 0.382, 0.036),
    7238 : PolarizationRecord(7238, 'online', 0.481, 0.071, 0.411, 0.050),
    7240 : PolarizationRecord(7240, 'online', 0.416, 0.039, 0.357, 0.034),
    7241 : PolarizationRecord(7241, 'online', 0.529, 0.049, 0.470, 0.057),
    7249 : PolarizationRecord(7249, 'online', 0.518, 0.048, 0.444, 0.042),
    7250 : PolarizationRecord(7250, 'online', 0.527, 0.077, 0.500, 0.103),
    7253 : PolarizationRecord(7253, 'online', 0.565, 0.067, 0.450, 0.054),
    7255 : PolarizationRecord(7255, 'online', 0.525, 0.055, 0.524, 0.077),
    7263 : PolarizationRecord(7263, 'online', 0.000, 0.000, 0.000, 0.000),
    7264 : PolarizationRecord(7264, 'online', 0.000, 0.000, 0.000, 0.000),
    7265 : PolarizationRecord(7265, 'online', 0.468, 0.044, 0.494, 0.046),
    7266 : PolarizationRecord(7266, 'online', 0.446, 0.054, 0.499, 0.073),
    7269 : PolarizationRecord(7269, 'online', 0.460, 0.055, 0.482, 0.071),
    7270 : PolarizationRecord(7270, 'online', 0.474, 0.040, 0.495, 0.046),
    7271 : PolarizationRecord(7271, 'online', 0.437, 0.091, 0.496, 0.103),
    7272 : PolarizationRecord(7272, 'online', 0.450, 0.042, 0.482, 0.050),
    7274 : PolarizationRecord(7274, 'online', 0.441, 0.032, 0.437, 0.040),
    7276 : PolarizationRecord(7276, 'online', 0.448, 0.066, 0.458, 0.067),
    7278 : PolarizationRecord(7278, 'online', 0.455, 0.038, 0.411, 0.038),
    7279 : PolarizationRecord(7279, 'online', 0.427, 0.034, 0.407, 0.038),
    7293 : PolarizationRecord(7293, 'online', 0.448, 0.047, 0.475, 0.049),
    7294 : PolarizationRecord(7294, 'online', 0.459, 0.055, 0.474, 0.069),
    7295 : PolarizationRecord(7295, 'online', 0.465, 0.048, 0.471, 0.049),
    7296 : PolarizationRecord(7296, 'online', 0.452, 0.031, 0.507, 0.042),
    7300 : PolarizationRecord(7300, 'online', 0.469, 0.043, 0.483, 0.050),
    7301 : PolarizationRecord(7301, 'online', 0.432, 0.037, 0.449, 0.046),
    7302 : PolarizationRecord(7302, 'online', 0.436, 0.034, 0.446, 0.038),
    7303 : PolarizationRecord(7303, 'online', 0.393, 0.033, 0.379, 0.040),
    7304 : PolarizationRecord(7304, 'online', 0.417, 0.031, 0.474, 0.040),
    7305 : PolarizationRecord(7305, 'online', 0.444, 0.038, 0.488, 0.050),
    7306 : PolarizationRecord(7306, 'online', 0.408, 0.043, 0.451, 0.047),
    7308 : PolarizationRecord(7308, 'online', 0.430, 0.036, 0.439, 0.052),
    7311 : PolarizationRecord(7311, 'online', 0.461, 0.055, 0.464, 0.056),
    7315 : PolarizationRecord(7315, 'online', 0.000, 0.000, 0.000, 0.000),
    7317 : PolarizationRecord(7317, 'online', 0.470, 0.043, 0.503, 0.052),
    7318 : PolarizationRecord(7318, 'online', 0.442, 0.030, 0.519, 0.040),
    7319 : PolarizationRecord(7319, 'online', 0.456, 0.030, 0.498, 0.038),
    7320 : PolarizationRecord(7320, 'online', 0.438, 0.034, 0.445, 0.041),
    7325 : PolarizationRecord(7325, 'online', 0.477, 0.040, 0.473, 0.034),
    7327 : PolarizationRecord(7327, 'online', 0.472, 0.056, 0.488, 0.050)
    }



werner_plus_dss_cteqm5_std = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=2.215E+06 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.814E+05 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=6.836E+04 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=2.285E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=9.215E+03 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=4.138E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=2.017E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=1.045E+03 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=5.683E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=3.216E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=1.886E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=1.132E+02 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=6.925E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=4.356E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=2.784E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=1.798E+01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.178E+01 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=7.817E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=5.258E+00 )
]

werner_plus_dss_cteqm5_max = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.204E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.788E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=5.553E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=1.578E+05 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=5.477E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=2.161E+04 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=9.384E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=4.376E+03 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.166E+03 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.123E+03 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=6.065E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=3.386E+02 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.938E+02 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.147E+02 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=6.915E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=4.229E+01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=2.636E+01 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.672E+01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=1.075E+01 )
]

werner_plus_dss_cteqm5_zero = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=2.380E+05 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=4.740E+04 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=1.307E+04 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=4.752E+03 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=2.024E+03 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=9.668E+02 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=4.967E+02 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=2.709E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.551E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=9.209E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=5.615E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=3.527E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=2.263E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=1.476E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=9.807E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=6.587E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=4.483E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=3.093E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=2.143E+00 )
]

werner_plus_dss_cteqm5_min = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y= 1.912E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 1.325E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 2.218E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 5.173E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 1.445E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 4.284E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 1.288E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y= 3.453E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y= 5.096E+01 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=-3.051E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=-4.436E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=-3.896E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-2.979E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-2.122E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-1.467E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-9.947E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-6.664E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-4.435E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-2.939E+00 )
]

werner_plus_dss_cteqm5_gsc = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=1.166E+05 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=1.261E+04 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=2.695E+03 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=8.108E+02 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=3.002E+02 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.305E+02 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=6.381E+01 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=3.472E+01 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=2.067E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=1.317E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=8.829E+00 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=6.103E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=4.311E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=3.104E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.263E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.652E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.221E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=9.019E-01 )
]


werner_minus_dss_cteqm5_std = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=1.788E+06 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.091E+05 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=4.676E+04 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=1.444E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=5.371E+03 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=2.207E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=9.838E+02 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=4.626E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=2.255E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=1.138E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=5.938E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=3.100E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.622E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=8.648E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=4.581E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.365E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.177E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=5.548E-01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=2.301E-01 )
]

werner_minus_dss_cteqm5_max = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.018E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.537E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=4.872E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=1.331E+05 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=4.417E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=1.657E+04 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=6.806E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=2.983E+03 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.380E+03 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=6.638E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=3.310E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=1.692E+02 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=8.794E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=4.698E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=2.536E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=1.372E+01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=7.477E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=4.095E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=2.244E+00 )
]

werner_minus_dss_cteqm5_zero = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y= 1.079E+05 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 1.984E+04 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 4.295E+03 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 1.194E+03 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 3.684E+02 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 1.171E+02 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 3.391E+01 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y= 5.356E+00 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=-3.632E+00 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=-5.628E+00 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=-5.325E+00 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=-4.427E+00 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-3.429E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-2.591E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-1.922E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-1.416E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-1.035E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-7.575E-01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-5.496E-01 )
]

werner_minus_dss_cteqm5_min = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y= 2.073E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 1.521E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 2.718E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 6.902E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 2.158E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 7.551E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 2.899E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y= 1.180E+03 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y= 5.043E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y= 2.239E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y= 1.024E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y= 4.720E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y= 2.172E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y= 1.006E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y= 4.521E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y= 1.932E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y= 7.110E-01 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y= 1.828E-01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-4.303E-02 )
]

werner_minus_dss_cteqm5_gsc = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 6.750E+04 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 2.606E+03 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=-3.009E+02 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=-3.196E+02 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=-2.022E+02 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=-1.219E+02 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=-7.421E+01 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=-4.599E+01 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=-2.903E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=-1.866E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=-1.220E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-8.113E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-5.460E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-3.725E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-2.572E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-1.781E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-1.252E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-8.834E-01 )
]


werner_zero_dss_cteqm5_std = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=2.001E+06 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.453E+05 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=5.755E+04 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=1.864E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=7.291E+03 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=3.173E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=1.500E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=7.533E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=3.970E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=2.177E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=1.239E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=7.207E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=4.275E+01 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=2.611E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=1.622E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=1.017E+01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=6.478E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=4.186E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=2.746E+00 )
]

werner_zero_dss_cteqm5_max = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y=3.111E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y=2.663E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y=5.213E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y=1.454E+05 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y=4.947E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y=1.909E+04 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y=8.096E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=3.680E+03 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=1.773E+03 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=8.934E+02 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=4.687E+02 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=2.540E+02 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=1.409E+02 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=8.085E+01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=4.725E+01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=2.800E+01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=1.691E+01 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=1.041E+01 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=6.494E+00 )
]

werner_zero_dss_cteqm5_zero = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y= 1.729E+05 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 3.360E+04 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 8.679E+03 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 2.973E+03 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 1.197E+03 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 5.422E+02 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 2.651E+02 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y= 1.382E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=-7.575E+01 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=-4.323E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=-2.541E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=-1.543E+01 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-9.595E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-6.085E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-3.941E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-2.586E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-1.723E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-1.168E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-7.963E-01 )
]

werner_zero_dss_cteqm5_min = [
datapoint( x=1.500E+00, xlow=1.000E+00, binwidth=1.000E+00, y= 1.992E+07 ),
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 1.423E+06 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 2.468E+05 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 6.038E+04 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 1.801E+04 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 5.917E+03 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 2.093E+03 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y= 7.625E+02 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y= 2.777E+02 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y= 9.665E+01 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y= 2.889E+01 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y= 4.139E+00 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-4.067E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-5.563E+00 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-5.083E+00 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-4.013E+00 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-2.971E+00 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-2.128E+00 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y=-1.489E+00 )
]

werner_zero_dss_cteqm5_gsc = [
datapoint( x=2.500E+00, xlow=2.000E+00, binwidth=1.000E+00, y= 9.204E+04 ),
datapoint( x=3.500E+00, xlow=3.000E+00, binwidth=1.000E+00, y= 7.603E+03 ),
datapoint( x=4.500E+00, xlow=4.000E+00, binwidth=1.000E+00, y= 1.198E+03 ),
datapoint( x=5.500E+00, xlow=5.000E+00, binwidth=1.000E+00, y= 2.461E+02 ),
datapoint( x=6.500E+00, xlow=6.000E+00, binwidth=1.000E+00, y= 4.916E+01 ),
datapoint( x=7.500E+00, xlow=7.000E+00, binwidth=1.000E+00, y= 4.242E+00 ),
datapoint( x=8.500E+00, xlow=8.000E+00, binwidth=1.000E+00, y=-5.156E+00 ),
datapoint( x=9.500E+00, xlow=9.000E+00, binwidth=1.000E+00, y=-5.615E+00 ),
datapoint( x=1.050E+01, xlow=1.000E+01, binwidth=1.000E+00, y=-4.206E+00 ),
datapoint( x=1.150E+01, xlow=1.100E+01, binwidth=1.000E+00, y=-2.730E+00 ),
datapoint( x=1.250E+01, xlow=1.200E+01, binwidth=1.000E+00, y=-1.686E+00 ),
datapoint( x=1.350E+01, xlow=1.300E+01, binwidth=1.000E+00, y=-1.015E+00 ),
datapoint( x=1.450E+01, xlow=1.400E+01, binwidth=1.000E+00, y=-5.765E-01 ),
datapoint( x=1.550E+01, xlow=1.500E+01, binwidth=1.000E+00, y=-3.112E-01 ),
datapoint( x=1.650E+01, xlow=1.600E+01, binwidth=1.000E+00, y=-1.545E-01 ),
datapoint( x=1.750E+01, xlow=1.700E+01, binwidth=1.000E+00, y=-6.494E-02 ),
datapoint( x=1.850E+01, xlow=1.800E+01, binwidth=1.000E+00, y=-1.536E-02 ),
datapoint( x=1.950E+01, xlow=1.900E+01, binwidth=1.000E+00, y= 9.608E-03 )
]

class theoryCurves:
    ##    pt        std          max         zero         min
    plus = [
    [1.2500E+00,  3.4460E-04,   4.8506E-03,  3.5261E-05,  2.8853E-03],
    [1.7500E+00,  6.3455E-04,   7.3671E-03,  9.1986E-05,  3.8942E-03],
    [2.2500E+00,  1.1518E-03,   1.1547E-02,  1.9264E-04,  5.4536E-03],
    [2.7500E+00,  1.8469E-03,   1.6531E-02,  3.3688E-04,  7.0562E-03],
    [3.2500E+00,  2.7062E-03,   2.1872E-02,  5.2338E-04,  8.3889E-03],
    [3.7500E+00,  3.7270E-03,   2.7424E-02,  7.5468E-04,  9.3654E-03],
    [4.2500E+00,  4.9175E-03,   3.3212E-02,  1.0344E-03,  1.0024E-02],
    [4.7500E+00,  6.2551E-03,   3.8887E-02,  1.3657E-03,  1.0213E-02],
    [5.2500E+00,  7.6712E-03,   4.4236E-02,  1.7300E-03,  9.7673E-03],
    [5.7500E+00,  9.1442E-03,   4.9221E-02,  2.1294E-03,  9.0873E-03],
    [6.2500E+00,  1.0704E-02,   5.3800E-02,  2.5723E-03,  7.8317E-03],
    [6.7500E+00,  1.2273E-02,   5.7949E-02,  3.0365E-03,  6.3188E-03],
    [7.2500E+00,  1.3898E-02,   6.1819E-02,  3.5343E-03,  4.5107E-03],
    [7.7500E+00,  1.5551E-02,   6.5345E-02,  4.0739E-03,  2.4027E-03],
    [8.5000E+00,  1.7835E-02,   6.9521E-02,  4.8562E-03, -7.4050E-04],
    [9.5000E+00,  2.1100E-02,   7.4679E-02,  6.0541E-03, -5.4630E-03],
    [1.1000E+01,  2.5352E-02,   7.9877E-02,  7.7940E-03, -1.1894E-02]
    ]
    minus = [
    [1.2500E+00,  2.8859E-04,   4.6304E-03,  1.9491E-05,  3.1480E-03],
    [1.7500E+00,  5.1079E-04,   6.9323E-03,  4.7805E-05,  4.3857E-03],
    [2.2500E+00,  8.7832E-04,   1.0685E-02,  8.7123E-05,  6.4265E-03],
    [2.7500E+00,  1.3442E-03,   1.5080E-02,  1.3163E-04,  8.7363E-03],
    [3.2500E+00,  1.8894E-03,   1.9648E-02,  1.7756E-04,  1.1006E-02],
    [3.7500E+00,  2.4952E-03,   2.4297E-02,  2.1965E-04,  1.3136E-02],
    [4.2500E+00,  3.1647E-03,   2.9014E-02,  2.5186E-04,  1.5179E-02],
    [4.7500E+00,  3.8674E-03,   3.3430E-02,  2.7805E-04,  1.6905E-02],
    [5.2500E+00,  4.5472E-03,   3.7411E-02,  2.8548E-04,  1.8230E-02],
    [5.7500E+00,  5.2230E-03,   4.0821E-02,  2.6862E-04,  1.9308E-02],
    [6.2500E+00,  5.8506E-03,   4.3749E-02,  2.3730E-04,  1.9942E-02],
    [6.7500E+00,  6.4328E-03,   4.6121E-02,  1.7588E-04,  2.0261E-02],
    [7.2500E+00,  6.9299E-03,   4.8145E-02,  8.4696E-05,  2.0410E-02],
    [7.7500E+00,  7.3931E-03,   4.9564E-02, -2.6112E-05,  2.0187E-02],
    [8.5000E+00,  7.8554E-03,   5.0457E-02, -2.2816E-04,  1.9492E-02],
    [9.5000E+00,  8.2509E-03,   5.0751E-02, -6.2721E-04,  1.8030E-02],
    [1.1000E+01,  8.2579E-03,   4.8985E-02, -1.3289E-03,  1.5208E-02]
    ]
    def __init__(self, pol_xsec=None, unpol_xsec=None):
        if pol_xsec is not None and unpol_xsec is not None:
            self.graph = ROOT.TGraph(len(pol_xsec))
            for i,point in enumerate(pol_xsec):
                val = point.y / unpol_xsec[i].y
                self.graph.SetPoint(i, point.x, val)
        else: self.graph = None
        #f = open('/Users/kocolosk/data/theory/theoryCurves.txt')
        #lines = f.readlines()
        #self.plus = [line.split() for line in lines[1:18]]
        #self.minus = [line.split() for line in lines[20:]]
    
    def ptBins(self):
        return [float(line[0]) for line in self.plus]
    
    def getList(self,charge,name):
        charge = charge.lower()
        if charge == 'plus': curveList = self.plus
        elif charge == 'minus': curveList = self.minus
        else: return None
        
        name = name.lower()
        if name     == 'std':   return [float(line[1]) for line in curveList]
        elif name   == 'max':   return [float(line[2]) for line in curveList]
        elif name   == 'zero':  return [float(line[3]) for line in curveList]
        elif name   == 'min':   return [float(line[4]) for line in curveList]
        else: return None
    
    def getGraph(self,charge=None,name=None):
        if self.graph is not None:
            return self.graph
        
        li = self.getList(charge,name)
        if li is not None:
            arrayX = array('f',self.ptBins())
            arrayY = array('f',li)
            gr = ROOT.TGraph(len(li),arrayX,arrayY)
            gr.SetLineWidth(3)
            if name     == 'std':   gr.SetLineColor(ROOT.kBlack)
            elif name   == 'max':   gr.SetLineColor(ROOT.kRed)
            elif name   == 'zero':  gr.SetLineColor(ROOT.kBlue)
            elif name   == 'min':   gr.SetLineColor(ROOT.kGreen)
            return gr


class polarizations:
    def __init__(self,year,version):
        path = {
             '2005_online':'/Users/kocolosk/data/run5/online_polarizations.txt',
             '2005_final':'/Users/kocolosk/data/run5/final_polarizations.txt',
             '2006_online':'/Users/kocolosk/data/run6/online_polarizations.txt',
        }
        try:
             version = version.lower()
             f = open(path['%s_%s' % (year,version)])
             lines = f.readlines()
             self.fillDict = {}
             if version == 'online':
                 for line in lines[1:]:
                     line = line.split() 
                     self.fillDict[int(line[0])] = line[1:]
                 #fyellow = open('/Users/kocolosk/data/run5/yellow_fill_average.dat')
                 #fblue  = open('/Users/kocolosk/data/run5/blue_fill_average.dat')
                 #ylines = fyellow.readlines()
                 #ylines = [line.split() for line in ylines]
                 #blines = fblue.readlines()
                 #blines = [line.split() for line in blines]
                 #bdict = {}
                 #for line in blines:
                 #     bdict[int(line[0])] = line[1:]
             elif version == 'final':
                 print 'final polarizations in use'
                 for line in lines[2:]:
                     line = line.split()
                     self.fillDict[int(line[0])] = [line[2],line[7],line[9],line[14]]
        except:
             print 'Exception raised in polarization init'
             
    def blue(self,fill):
        try:
             val = self.fillDict[fill]
             return float(val[0])
        except:
             #print 'missing fill', fill
             return 0.
    
    def blueError(self,fill):
        try:
             val = self.fillDict[fill]
             return float(val[1])
        except:
             #print 'missing fill', fill
             return 0.
        
    def yellow(self,fill):
        try:
             val = self.fillDict[fill]
             return float(val[2])
        except:
             #print 'missing fill', fill
             return 0.
        
    def yellowError(self,fill):
        try:
             val = self.fillDict[fill]
             return float(val[3])
        except:
             #print 'missing fill', fill
             return 0.


def compareTheoryCurves(charge='plus'):
    """plots mockup and DSS theory curves on one plot"""
    import xsec
    if charge == 'plus':
        dss_std = theoryCurves(werner_plus_dss_cteqm5_std, \
        xsec.werner_plus_dss_cteqm5_pt).getGraph()
        dss_max = theoryCurves(werner_plus_dss_cteqm5_max, xsec.werner_plus_dss_cteqm5_pt).getGraph()
        dss_zero= theoryCurves(werner_plus_dss_cteqm5_zero,xsec.werner_plus_dss_cteqm5_pt).getGraph()
        dss_min = theoryCurves(werner_plus_dss_cteqm5_min, xsec.werner_plus_dss_cteqm5_pt).getGraph()
        dss_gsc = theoryCurves(werner_plus_dss_cteqm5_gsc, xsec.werner_plus_dss_cteqm5_pt).getGraph()
    elif charge == 'minus':
        dss_std = theoryCurves(werner_minus_dss_cteqm5_std, xsec.werner_minus_dss_cteqm5_pt).getGraph()
        dss_max = theoryCurves(werner_minus_dss_cteqm5_max, xsec.werner_minus_dss_cteqm5_pt).getGraph()
        dss_zero= theoryCurves(werner_minus_dss_cteqm5_zero,xsec.werner_minus_dss_cteqm5_pt).getGraph()
        dss_min = theoryCurves(werner_minus_dss_cteqm5_min, xsec.werner_minus_dss_cteqm5_pt).getGraph()
        dss_gsc = theoryCurves(werner_minus_dss_cteqm5_gsc, xsec.werner_minus_dss_cteqm5_pt).getGraph()
    mock = theoryCurves()
    
    c = ROOT.TCanvas()
    bg = ROOT.TH2D('bg','',1,0,15,1,-0.03,0.11)
    bg.SetXTitle('p_{T}')
    bg.SetYTitle('A_{LL}')
    if charge == 'plus': bg.SetTitle('NLO predictions for A_{LL}(#pi^{+}), |#eta| < 1')
    else :               bg.SetTitle('NLO predictions for A_{LL}(#pi^{-}), |#eta| < 1')
    bg.Draw()
    graphs = [ mock.getGraph(charge,'std'), mock.getGraph(charge,'max'), mock.getGraph(charge,'zero'), mock.getGraph(charge,'min') ]
    for gr in graphs: gr.Draw('l same')
    
    newGraphs = [dss_std, dss_max, dss_zero, dss_min, dss_gsc]
    [gr.SetLineStyle(7) for gr in newGraphs]
    dss_max.SetLineColor(ROOT.kRed)
    dss_min.SetLineColor(ROOT.kGreen)
    dss_zero.SetLineColor(ROOT.kBlue)
    dss_gsc.SetLineColor(ROOT.kYellow)
    
    [gr.Draw('l same') for gr in newGraphs]
    
    leg = ROOT.TLegend(0.13,0.55,0.35,0.89,"solid=old, dash=DSS")
    leg.AddEntry(dss_std,'GRSV-std','l')
    leg.AddEntry(dss_max,'GRSV-max','l')
    leg.AddEntry(dss_zero,'GRSV-zero','l')
    leg.AddEntry(dss_min,'GRSV-min','l')
    leg.AddEntry(dss_gsc ,'GS-C','l')
    
    leg.Draw()
    
    raw_input('belrge')
    if charge == 'plus': c.Print('new_plus_predictions.gif')
    else:                c.Print('new_minus_predictions.gif')


def transverse_relative_luminosity_run5(tfile):
    """prints scaler counts in format for StTamuRelLum from Tai's file"""
    import analysis
    tree = tfile.relLumi
    tree.BuildIndex('runNumber','10000*detectorID+100*boardn+timebin')
    for run in analysis.transverse_run5:
        fill = analysis.getFill(run)
        for timebin in range(15):
            minorstring = '105%02d' % (timebin+1,)
            #print run, minorstring
            tree.GetEntryWithIndex(int(run), int(minorstring))
            print '%d %d 5 %2d %d %d %d %d' % (fill, run, timebin+1, tree.N5, \
                tree.N6, tree.N9, tree.N10)

