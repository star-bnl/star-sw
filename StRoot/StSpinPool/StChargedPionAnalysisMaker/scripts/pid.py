# encoding: utf-8
from array import array
from math import exp, log10

import ROOT
from analysis.util import fill

class Gaussian:
    def __init__(self, norm=None, mean=None, sigma=None):
        self.norm = norm
        self.mean = mean
        self.sigma = sigma
    
    def __call__(self, x, par=None):
        norm  = par and par[0] or self.norm
        mean  = par and par[1] or self.mean
        sigma = par and par[2] or self.sigma
            
        if sigma == 0 and mean == 0:
            return norm
        elif sigma == 0:
            return 0
        else:
            return norm*exp(-1*(x[0]-mean)**2/(2*sigma**2))


_calib = {
6988 : ( 0.066608, 0.889596),
6990 : ( 0.028513, 0.872612),
6992 : ( 0.016085, 0.856032),
6994 : ( 0.076727, 0.867838),
6995 : ( 0.167452, 0.855377),
6997 : ( 0.206903, 0.882462),
6998 : (-0.018938, 0.844754),
7001 : (-0.006526, 0.863949),
7002 : ( 0.014921, 0.865907),
7032 : (-0.059741, 0.856267),
7034 : (-0.064317, 0.870471),
7035 : (-0.090822, 0.871124),
7048 : (-0.387454, 0.866186),
7049 : (-0.393791, 0.865776),
7051 : (-0.137894, 0.792171),
7055 : (-0.420703, 0.858151),
7064 : (-0.283141, 0.878024),
7067 : (-0.087372, 0.873845),
7068 : (-0.077816, 0.840146),
7069 : ( 0.010756, 0.861486),
7070 : (-0.054644, 0.879342),
7072 : (-0.022931, 0.863193),
7075 : (-0.065000, 0.814292),
7079 : (-0.261988, 0.896589),
7085 : (-0.207958, 0.879845),
7087 : (-0.062486, 0.818310),
7088 : (-0.184964, 0.869739),
7092 : (-0.095016, 0.879631),
7102 : (-0.161471, 0.899666),
7103 : (-0.180760, 0.900305),
7110 : (-0.194342, 0.894236),
7112 : (-0.116899, 0.878893),
7114 : (-0.140619, 0.871448),
7118 : (-0.206615, 0.883038),
7120 : (-0.096027, 0.884061),
7122 : (-0.187495, 0.903031),
7123 : (-0.172261, 0.885190),
7124 : (-0.175057, 0.906155),
7125 : (-0.134531, 0.887071),
#7127 : (-0.149092, 0.897838), ## RunLog_onl problem
7128 : (-0.149092, 0.897838),
7131 : (-0.197370, 0.889249),
7133 : (-0.207133, 0.888365),
#7134 : (-0.130486, 0.893232), ## RunLog_onl problem
7134 : (-0.119566, 0.901472),
7136 : (-0.026902, 0.621362),
7138 : (-0.150951, 0.892991),
7151 : ( 0.033011, 0.851425),
7153 : (-0.094305, 0.892003),
7154 : ( 0.010605, 0.824362),
7161 : (-0.167815, 0.894401),
7162 : (-0.152793, 0.906894),
7164 : (-0.125891, 0.894419),
7165 : (-0.162527, 0.870979),
7166 : (-0.133315, 0.884772),
7172 : (-0.207923, 0.894637),
7232 : (-0.110842, 0.864740),
7237 : (-0.269559, 0.916554),
7238 : (-0.259372, 0.891293),
7249 : (-0.247810, 0.894708),
7250 : (-0.316081, 0.909442),
7253 : (-0.271044, 0.845922),
7255 : (-0.249242, 0.857201),
7265 : (-0.190056, 0.865755),
7266 : (-0.243196, 0.869884),
7269 : (-0.242802, 0.892759),
7270 : (-0.314700, 0.886310),
7271 : (-0.389984, 0.910892),
7272 : (-0.194932, 0.883244),
7274 : (-0.344731, 0.903323),
7276 : (-0.198418, 0.897573),
7278 : (-0.273830, 0.875383),
7279 : (-0.219441, 0.895625),
7293 : (-0.346773, 0.803589), ## transverse
7295 : (-0.268542, 0.859643), ## transverse
7296 : (-0.328538, 0.901968), ## transverse
7300 : (-0.283700, 0.876021),
7301 : (-0.325708, 0.891317),
7302 : (-0.333164, 0.890520),
7303 : (-0.274641, 0.894173),
7304 : (-0.306279, 0.890682),
7305 : ( 0.726845, 0.934432),
7308 : (-0.297895, 0.913868),
7311 : (-0.232436, 0.905442),
7317 : (-0.301910, 0.886113),
7320 : (-0.229533, 0.893824),
7325 : (-0.251390, 0.898416),
7327 : (-0.251943, 0.895118),
7718 : (-0.077766, 0.879930),  ## transverse
7722 : (-0.019419, 0.848818),  ## transverse
7724 : (-0.115915, 0.875705),  ## transverse
7725 : (-0.089028, 0.869330),  ## transverse
7729 : (-0.149134, 0.896646),  ## transverse
7739 : (-0.092455, 0.885406),  ## transverse
7740 : (-0.108566, 0.891201),  ## transverse
7744 : (-0.051341, 0.894509),  ## transverse
7745 : (-0.072091, 0.864465),  ## transverse
7753 : (-0.051187, 0.889440),  ## transverse
7756 : ( 0.151309, 0.848012),  ## transverse
7757 : (-0.108126, 0.880723),  ## transverse
7781 : (-0.045714, 0.907452),  ## transverse
7785 : ( 0.079990, 0.866071),  ## transverse
7786 : (-0.043541, 0.882520),  ## transverse
7788 : ( 0.063298, 0.888150),  ## transverse
7789 : (-0.115123, 0.894880),  ## transverse
7790 : (-0.163721, 0.903411),  ## transverse
7791 : (-0.107792, 0.909530),  ## transverse
7792 : (-0.058317, 0.892502),  ## transverse
7794 : (-0.062679, 0.903440),  ## transverse
7795 : (-0.120403, 0.904253),  ## transverse
7796 : (-0.208192, 0.927860),  ## transverse
7797 : (-0.157988, 0.910834),  ## transverse
7800 : (-0.135945, 0.889122),  ## transverse
7803 : (-0.024001, 0.873819),  ## transverse
7804 : (-0.253835, 0.897332),  ## transverse
7805 : (-0.176320, 0.895462),  ## transverse
7810 : (-0.084868, 0.877880),  ## transverse
7811 : (-0.157192, 0.863181),  ## transverse
7815 : (-0.270277, 0.898946),  ## transverse
7817 : (-0.196213, 0.880809),  ## transverse
7820 : (-0.238482, 0.899890),  ## transverse
7823 : (-0.247331, 0.894104),  ## transverse
7824 : (-0.278816, 0.909190),  ## transverse
7825 : (-0.174100, 0.886618),  ## transverse
7826 : (-0.234116, 0.887850),  ## transverse
7827 : (-0.171585, 0.863359),  ## transverse
7830 : (-0.238725, 0.895052),  ## transverse
7831 : (-0.222429, 0.897187),  ## transverse
7847 : (-0.027059, 0.874814),
7850 : (-0.123179, 0.900492),
7851 : (-0.036289, 0.889293),
7852 : (-0.045614, 0.891709),
7853 : (-0.119185, 0.898291),
7855 : (-0.134738, 0.908692),
7856 : (-0.089265, 0.895362),
7858 : (-0.102251, 0.897319),
7863 : (-0.083709, 0.911717),
7864 : (-0.113995, 0.904617),
7865 : ( 0.009516, 0.813364),
7871 : (-0.109983, 0.906486),
7872 : (-0.138412, 0.882469),
7883 : (-0.182051, 0.885964),
7886 : (-0.185474, 0.889406),
7887 : (-0.155607, 0.886540),
7889 : (-0.100630, 0.863594),
7890 : (-0.053211, 0.921421),
7891 : (-0.099581, 0.861152),
7892 : (-0.077996, 0.892110),
7893 : (-0.123162, 0.907638),
7896 : (-0.094806, 0.894019),
7898 : (-0.057477, 0.896248),
7901 : (-0.014140, 0.865671),
7908 : (-0.040456, 0.883598),
7909 : (-0.077470, 0.880354),
7911 : (-0.048150, 0.861430),
7913 : (-0.076636, 0.896822),
7916 : (-0.078441, 0.894539),
7918 : ( 0.043434, 0.888361),
7921 : (-0.010669, 0.876733),
7922 : ( 0.022409, 0.907525),
7926 : (-0.033154, 0.886284),
7944 : (-0.068744, 0.905098),
7949 : (-0.039570, 0.876990),
7951 : ( 0.056927, 0.936951),
7952 : ( 0.025446, 0.885525),
7954 : ( 0.205871, 0.864685),
7957 : ( 0.014065, 0.899964)
}

def shift(run, nsigpi):
    offset = _calib[fill(run)][0]
    return nsigpi - offset

def min(run, cut=-1.0):
    mean, width = _calib[fill(run)]
    return mean + cut*width

def max(run, cut=2.0):
    mean, width = _calib[fill(run)]
    return mean + cut*width

def fit(h, p):
    """
    performs the spectra-style 8 Gaussian fit on a histogram containing both
    + and - tracks shifted by 5*track.charge().  Fixed parameters include
    
    π+ mean == π- mean
    K+ mean == K- mean
    e+ mean == e- mean
    p  mean == pbar mean
    all widths equal
    K mean (from recalibration) (expressed as π-K)
    p mean (from recalibration) (expressed as π-p)
    e mean (from recalibration) (expressed as π-e)
    
    In other words, the only parameters of the fit that are actually allowed to
    float are the yields of each species (8), the resolution (1), and the π
    mean.
    
    Yichun doesn't have any recalibrated data for p_{T} < 3 GeV.  I obtained 
    the π-K and π-p separations in that regime from Bichsel.  Bichsel's π-e 
    numbers were quite far off, so I allowed π-e to float and then stuck the
    values I got into this code.  That procedure can be repeated by 
    uncommenting a few select lines.
    """
    from uuid import uuid1 as uuid
    
    xpoints = array('d', [2.125, 2.375, 2.625, 2.875, 3.125, 3.375, 3.625, 
        3.875, 4.25, 4.75, 5.25, 5.75, 6.25, 6.75, 7.5, 9.0, 11.0, 13.5])
    K_pi_points = [
        -1.4983, ## Bichsel
        -1.5858, ## Bichsel
        -1.6393, ## Bichsel
        -1.6791, ## Bichsel
        -1.69081,
        -1.69629,
        -1.69354,
        -1.6873,
        -1.68201,
        -1.70533,
        -1.71762,
        -1.68769,
        -1.64231,
        -1.62156,
        -1.5798,
        -1.52717,
        -1.48498,
        -1.41808
    ]
    p_pi_points = [
        -0.9990, ## Bichsel
        -1.3854, ## Bichsel
        -1.6398, ## Bichsel
        -1.8385, ## Bichsel
        -1.98053,
        -2.09692,
        -2.21161,
        -2.2801,
        -2.33066,
        -2.43533,
        -2.4858,
        -2.52706,
        -2.54293,
        -2.53356,
        -2.49091,
        -2.42632,
        -2.33928,
        -2.22135
    ]
    e_pi_points = [
        # 4.7485, ## Bichsel
        # 4.5271, ## Bichsel
        # 4.3310, ## Bichsel
        # 4.1556, ## Bichsel
        3.78652, ## allowed e-pi separation to float, then took these numbers
        3.78652, ## allowed e-pi separation to float, then took these numbers
        3.73179, ## allowed e-pi separation to float, then took these numbers
        3.73179, ## allowed e-pi separation to float, then took these numbers
        3.33640,
        3.24293,
        3.15668,
        3.07248,
        2.96264,
        2.78870,
        2.63843,
        2.52391,
        2.42252,
        2.33433,
        2.24786,
        2.08437,
        1.84996,
        1.66622
    ]
    K_pi = ROOT.TGraph(len(xpoints), xpoints, array('d', K_pi_points))
    p_pi = ROOT.TGraph(len(xpoints), xpoints, array('d', p_pi_points))
    e_pi = ROOT.TGraph(len(xpoints), xpoints, array('d', e_pi_points))
    
    class ParticleIdentifier(dict):
        def __init__(self):
            super(ParticleIdentifier, self).__init__()
        
        def __call__(self, x, par):
            """
            par[0] = π+ norm
            par[1] = π- norm
            par[2] = K+/π+
            par[3] = K-/π-
            par[4] = p/π+
            par[5] = pbar/π-
            par[6] = e+/π+
            par[7] = e-/π-
            par[8] = width
            par[9] = π mean
            """
            self['pi_plus']  = Gaussian(par[0], par[9]+6, par[8])
            self['K_plus']   = Gaussian(par[2]*par[0], par[9]+6+K_pi.Eval(p), par[8])
            self['proton']   = Gaussian(par[4]*par[0], par[9]+6+p_pi.Eval(p), par[8])
            self['positron'] = Gaussian(par[6]*par[0], par[9]+6+e_pi.Eval(p), par[8])
            # self['positron'] = Gaussian(par[6]*par[0], par[10]+6, par[8])
        
            self['pi_minus'] = Gaussian(par[1], par[9]-6, par[8])
            self['K_minus']  = Gaussian(par[3]*par[1], par[9]-6+K_pi.Eval(p), par[8])
            self['pbar']     = Gaussian(par[5]*par[1], par[9]-6+p_pi.Eval(p), par[8])
            self['electron'] = Gaussian(par[7]*par[1], par[9]-6+e_pi.Eval(p), par[8])
            # self['electron'] = Gaussian(par[7]*par[1], par[10]-6, par[8])
            
            return sum([f(x) for f in self.values()])
    
    h.SetStats(True)
    ROOT.gStyle.SetOptFit(1101)
    
    fitter = ParticleIdentifier()
    tf1 = ROOT.TF1(str(uuid()), fitter, -12.0, 12.0, 10)
    tf1.SetParName(0, "#pi+ Norm")
    tf1.SetParName(1, "#pi- Norm")
    tf1.SetParName(2, "K+/#pi+")
    tf1.SetParName(3, "K-/#pi-")
    tf1.SetParName(4, "p/#pi+")
    tf1.SetParName(5, "#bar{p}/#pi-")
    tf1.SetParName(6, "e+/#pi+")
    tf1.SetParName(7, "e-/#pi-")
    tf1.SetParName(8, "Width")
    tf1.SetParName(9, "#pi Mean")
    # tf1.SetParName(10, "e Mean")
    
    tf1.SetParameter(0, h.GetMaximum())
    tf1.SetParameter(1, h.GetMaximum())
    tf1.SetParameter(2, 0.25)
    tf1.SetParameter(3, 0.25)
    tf1.SetParameter(4, 0.2)
    tf1.SetParameter(5, 0.2)
    tf1.SetParameter(6, 0.025)
    tf1.SetParameter(7, 0.025)
    tf1.SetParameter(8, 0.9)
    tf1.SetParameter(9, -0.3)
    # tf1.SetParameter(10, 4.0)
    
    tf1.SetParLimits(2, 0.0, 0.5)
    tf1.SetParLimits(3, 0.0, 0.5)
    tf1.SetParLimits(4, 0.0, 0.5)
    tf1.SetParLimits(5, 0.0, 0.5)
    tf1.SetParLimits(6, 0.0, 0.5)
    tf1.SetParLimits(7, 0.0, 0.5)
    # tf1.SetParLimits(10, 2.0, 5.0)
    
    tf1.SetLineWidth(2)
    
    h.Fit(tf1, 'rq')
    
    return fitter

def bichsel(species='pi', method='GetI70', resolution=0.08):
    mass = { 'pi': 0.139570, 'K': 0.493677, 'p': 0.938272, 'e': 0.000511 }
    b = ROOT.Bichsel.Instance()
    fun = getattr(b, method)
    
    ## distribute data points so we have uniform coverage on log scale
    xvals = [10**(float(i)/1000) for i in range(-1000, 2000)]
    yvals = [fun(log10(p/mass[species])) for p in xvals]
    ex = [0 for i in range(-1000, 2000)]
    ey = [resolution*val for val in yvals]
    graph = ROOT.TGraphErrors(len(xvals), array('d',xvals), array('d',yvals), 
        array('d', ex), array('d', ey))
    return graph

def bichsel_acceptance(method='GetI70', resolution=0.08):
    mass = 0.139570
    b = ROOT.Bichsel.Instance()
    fun = getattr(b, method)
    xvals = [10**(float(i)/1000) for i in range(-1000, 2000)]
    xvals = filter(lambda x: x>3.0 and x<12.5, xvals)
    yvals = [fun(log10(p/mass)) for p in xvals]
    ex = [0 for i in range(-1000, 2000)]
    eyl = [1*resolution*val for val in yvals]
    eyh = [2*resolution*val for val in yvals]
    graph = ROOT.TGraphAsymmErrors(len(xvals), array('d',xvals), array('d',yvals), 
        array('d', ex), array('d', ex), array('d', eyl), array('d', eyh))
    return graph

