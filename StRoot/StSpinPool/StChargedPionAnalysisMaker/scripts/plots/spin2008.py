# encoding: utf-8
# i need a better header template :-/
# this module is supposed to collect all the plots i'll be showing at SPIN 2008

import os
import math
from glob import glob

import ROOT

from analysis.asym import AsymmetryGenerator, ScalarCounts, Polarizations
from analysis.histos import HistogramManager, shifted
from analysis.plots import pid_calibration
from analysis.runlists import long2_run6 as runlist
from analysis.runlists import transverse_run6 as transverse_runlist
from analysis.util import getRun, fillList, hadd_interactive
from analysis.plots import graphics

histDir         = '/Users/kocolosk/data/run6/spin2008/hist'
transHistDir    = '/Users/kocolosk/data/run6/spin2008/hist-transverse'
mcasymFile      = '/Users/kocolosk/data/run6/simu/mcasym_10.cphist.root'
datamcFile      = '/Users/kocolosk/data/run6/spin2008/merged.cphist.root'

zbins = [
    # 0.075, 0.125, ## these two are biased b/c of the track pT cut
    0.2, 0.3, 0.45, 0.65, 1.0
]

def result():
    """
    result plot showing asymmetries versus z
    """    
    asym_p = AsymmetryGenerator('asym_p', bins=zbins, key='z_away2')
    asym_m = AsymmetryGenerator('asym_m', bins=zbins, key='z_away2')
    
    scalars = ScalarCounts(os.environ['STAR'] + 
        '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt')
    
    polarizations = Polarizations.Final
    
    allFiles = glob(histDir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = getRun(fname)
        if run in runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname), ['z_away2'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                bin6 = scalars[str(run) + '-6-6']
                bin7 = scalars[str(run) + '-6-7']
                bin8 = scalars[str(run) + '-6-8']
                bin9 = scalars[str(run) + '-6-9']
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            pol = polarizations[bin7.fill]
            
            asym_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
    
    ## fun with graphics
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptDate(0)
    
    c = graphics.canvas2()
    c.SetLogy(0)
    
    line = ROOT.TLine(zbins[0], 0.0, zbins[-1], 0.0)
    line.SetLineStyle(2)
    line.SetLineColor(10)
    
    prelim = ROOT.TText()
    prelim.SetTextColor(44)
    prelim.SetTextAlign(21)
    
    latex = ROOT.TLatex()
    latex.SetTextSize(0.25)
    # latex.SetTextAlign(21)
    
    syst = systematic_uncertainties()
    syst_m = ROOT.TGraphErrors(len(zbins))
    syst_p = ROOT.TGraphErrors(len(zbins))
    
    for g in (syst_m,syst_p):
        g.SetMarkerColor(15)
        g.SetFillColor(15)
        g.SetPoint(len(zbins), 1.0, 0)
        g.SetPointError(len(zbins), 0., 0.)
        g.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    
    hm = asym_m.GetAsymmetry('ll')
    hp = asym_p.GetAsymmetry('ll')
    
    for i in range(len(zbins)-1):
        z = (zbins[i] + zbins[i+1])/2
        syst_m.SetPoint(i, z, hm.GetBinContent(i+1))
        syst_m.SetPointError(i, 0.01, syst['minus'][i])
        syst_p.SetPoint(i, z, hp.GetBinContent(i+1))
        syst_p.SetPointError(i, 0.01, syst['plus'][i])
        
    c.cd(1)
    syst_m.Draw('a2p')
    line.Draw()
    hm.SetMarkerStyle(20)
    hm.Draw('e1 same')
    prelim.DrawText(0.5,0.065,"2006 STAR Preliminary")
    latex.DrawLatex(0.07 + zbins[0], -0.085, '#pi^{-}')
    
    c.cd(2)
    syst_p.Draw('a2p')
    line.Draw()
    hp.SetMarkerStyle(21)
    hp.Draw('e1 same')
    prelim.DrawText(0.5,0.065,"2006 STAR Preliminary")
    latex.DrawLatex(0.07 + zbins[0], -0.085, '#pi^{+}')
    
    for h in (hm,hp, syst_m, syst_p):
        h.SetTitle('')
        h.GetXaxis().SetTitle('z = p_{T}(#pi)/p_{T}(jet)')
        h.GetYaxis().SetRangeUser(-0.1, 0.1)
    
    raw_input('wait here:')
    c.Print('preliminary_result.png')
    
    print '\nπ- A_{LL}'
    for bin in range(1, hm.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hm.GetBinLowEdge(bin), 
            hm.GetBinLowEdge(bin+1), hm.GetBinContent(bin), hm.GetBinError(bin))
    
    print '\nπ+ A_{LL}'
    for bin in range(1, hp.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hp.GetBinLowEdge(bin), 
            hp.GetBinLowEdge(bin+1), hp.GetBinContent(bin), hp.GetBinError(bin))
    
    color = {
        'STD': ROOT.kBlack,
        'ZERO': ROOT.kBlue,
        'GS_NLOC': ROOT.kMagenta,
        'MIN': ROOT.kGreen,
        'DSSV': ROOT.kGreen + 100
    }
    title = {
        'STD': 'GRSV-STD',
        'MIN': 'GRSV-MIN',
        'ZERO': 'GRSV-ZERO',
        'GS_NLOC': 'GS Set C',
        'DSSV': 'DSSV'
    }
    mcasym_tfile = ROOT.TFile(mcasymFile)
    keys = ['STD','ZERO','GS_NLOC','DSSV']
    wkeys = [key+'w' for key in keys]
    mgr2 = HistogramManager(mcasym_tfile, keys=wkeys)
    leg = ROOT.TLegend(0.6, 0.13, 0.87, 0.37)
    leg.SetHeader('LO MC Evaluation')
    leg.SetBorderSize(1)
    for key in keys:
        m = mgr2['anyspin']['117001'].tracks_minus[key+'w']
        p = mgr2['anyspin']['117001'].tracks_plus[key+'w']
        for h in (m,p):
            h.Smooth(2)
            h.SetLineColor(color[key])
            h.SetFillColor(color[key])
            h.SetLineWidth(2)
        leg.AddEntry(m, title[key])
        c.cd(1)
        m.Draw('e3 same')
        hm.Draw('e1 same')
        c.cd(2)
        p.Draw('e3 same')
        hp.Draw('e1 same')
    c.cd(1)
    leg.Draw()
    raw_input('now wait here:')
    
    c.Print('preliminary_result_models.png')


def systematic_uncertainties():
    """tabulates sources of uncertainty and sums them in quadrature"""
    
    result_m = [
         0.066,     # [0.07-0.12]   0.066 ± 0.019
         0.019,     # [0.12-0.20]   0.019 ± 0.009
         0.002,     # [0.20-0.30]   0.002 ± 0.009
        -0.006,     # [0.30-0.45]  -0.006 ± 0.014
         0.007,     # [0.45-0.65]   0.007 ± 0.023
         0.012      # [0.65-1.00]   0.012 ± 0.040
    ]
    result_p = [
         0.026,     # [0.07-0.12]   0.026 ± 0.019
         0.021,     # [0.12-0.20]   0.021 ± 0.008
         0.002,     # [0.20-0.30]   0.002 ± 0.009
        -0.014,     # [0.30-0.45]  -0.014 ± 0.013
         0.024,     # [0.45-0.65]   0.024 ± 0.022
         0.046      # [0.65-1.00]   0.046 ± 0.037
    ]
    
    pid_contamination = 0.10
    pid_asym_m = [
        ( 0.051 , 0.038),      # [0.07-0.12]   0.051 ± 0.038
        (-0.017 , 0.016),      # [0.12-0.20]  -0.017 ± 0.016
        (-0.032 , 0.016),      # [0.20-0.30]  -0.032 ± 0.016
        (-0.006 , 0.023),      # [0.30-0.45]  -0.006 ± 0.023
        (-0.031 , 0.042),      # [0.45-0.65]  -0.031 ± 0.042
        ( 0.089 , 0.085)       # [0.65-1.00]   0.089 ± 0.085
    ]
    pid_asym_p = [
        ( 0.005 , 0.036),      # [0.07-0.12]   0.005 ± 0.036
        ( 0.006 , 0.015),      # [0.12-0.20]   0.006 ± 0.015
        (-0.006 , 0.015),      # [0.20-0.30]  -0.006 ± 0.015
        ( 0.018 , 0.020),      # [0.30-0.45]   0.018 ± 0.020
        (-0.038 , 0.032),      # [0.45-0.65]  -0.038 ± 0.032
        ( 0.142 , 0.059)       # [0.65-1.00]   0.142 ± 0.059
    ]
    
    for i in range(len(pid_asym_m)):
        val, err = pid_asym_m[i]
        pid_asym_m[i] = max( val-result_m[i], err)
    for i in range(len(pid_asym_p)):
        val, err = pid_asym_p[i]
        pid_asym_p[i] = max( val-result_p[i], err)
    
    beam_vector = 0.0102
    asigma_m = [
        0.035,      # [0.07-0.12]   0.005 ± 0.035
        0.015,      # [0.12-0.20]  -0.012 ± 0.015
        0.016,      # [0.20-0.30]  -0.014 ± 0.016
        0.027,      # [0.30-0.45]  -0.027 ± 0.023
        0.066,      # [0.45-0.65]  -0.066 ± 0.040
        0.073       # [0.65-1.00]  -0.072 ± 0.073
    ]
    asigma_p = [
        0.034,      # [0.07-0.12]  -0.001 ± 0.034
        0.014,      # [0.12-0.20]  -0.007 ± 0.014
        0.015,      # [0.20-0.30]   0.007 ± 0.015
        0.025,      # [0.30-0.45]  -0.025 ± 0.022
        0.039,      # [0.45-0.65]  -0.039 ± 0.037
        0.061       # [0.65-1.00]   0.033 ± 0.061
    ]
    
    mcasym_m = [
        0.0066,     # [0.07-0.12]   0.0012 ± 0.0066
        0.0057,     # [0.12-0.20]   0.0057 ± 0.0025
        0.0089,     # [0.20-0.30]   0.0089 ± 0.0020
        0.0077,     # [0.30-0.45]   0.0077 ± 0.0026
        0.0042,     # [0.45-0.65]   0.0038 ± 0.0042
        0.0070      # [0.65-1.00]   0.0053 ± 0.0070
    ]
    mcasym_p = [
        0.0047,     # [0.07-0.12]  -0.0014 ± 0.0047
        0.0077,     # [0.12-0.20]   0.0077 ± 0.0024
        0.0147,     # [0.20-0.30]   0.0147 ± 0.0023
        0.0105,     # [0.30-0.45]   0.0105 ± 0.0024
        0.0057,     # [0.45-0.65]   0.0057 ± 0.0044
        0.0112      # [0.65-1.00]   0.0112 ± 0.0081
    ]
    
    pt_shift_m = [ 0, 0,
        0.003, # [0.20-0.30]   0.006 low, 0.001 high, 0.003 avg
        0.005, # [0.30-0.45]   0.007 low, 0.003 high, 0.005 avg
        0.016, # [0.45-0.65]   0.020 low, 0.012 high, 0.016 avg
        0.010  # [0.65-1.00]   0.011 low, 0.008 high, 0.010 avg
    ]
    
    pt_shift_p = [ 0, 0,
        0.004, # [0.20-0.30]   0.005 low, 0.003 high, 0.004 avg
        0.007, # [0.30-0.45]   0.008 low, 0.006 high, 0.007 avg
        0.016, # [0.45-0.65]   0.023 low, 0.008 high, 0.016 avg
        0.016  # [0.65-1.00]   0.012 low, 0.020 high, 0.016 avg
    ]
    
    relative_luminosity = 9.4e-4
    
    minus = [0.0 for bin in zbins[:-1]]
    plus = [0.0 for bin in zbins[:-1]]
    
    start = len(zbins) == 5 and 2 or 0
    for i in range(start, start+len(zbins)-1):
        minus[i-start] = math.sqrt(
            pow(relative_luminosity, 2) + 
            pow(pid_contamination*pid_asym_m[i], 2) +
            pow(beam_vector*asigma_m[i], 2) +
            pow(mcasym_m[i], 2) + 
            pow(pt_shift_m[i], 2)
        )
        plus[i-start] = math.sqrt(
            pow(relative_luminosity, 2) +
            pow(pid_contamination*pid_asym_p[i], 2) + 
            pow(beam_vector*asigma_p[i], 2) + 
            pow(mcasym_p[i], 2) + 
            pow(pt_shift_p[i], 2)
        )
    
    return {'minus':minus, 'plus':plus}


def ssa():
    """
    plots single-spin asymmetries versus fill and z, saves them in PNG format
    """
    asym_zp = AsymmetryGenerator('asym_zp', bins=zbins, key='z_away2')
    asym_zm = AsymmetryGenerator('asym_zm', bins=zbins, key='z_away2')
    asym_fp = {}
    asym_fm = {}
    
    fills = [int(f) for f in fillList(runlist)]
    for fill in fills:
        asym_fp[fill] = AsymmetryGenerator('p%d' % fill, bins=[1,-0.5,0.5], 
            key='one')
        asym_fm[fill] = AsymmetryGenerator('m%d' % fill, bins=[1,-0.5,0.5], 
            key='one')
    
    scalars = ScalarCounts(os.environ['STAR'] + 
        '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt')
    
    polarizations = Polarizations.Final
    
    allFiles = glob(histDir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = getRun(fname)
        if run in runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname), ['z_away2','one'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                bin6 = scalars[str(run) + '-6-6']
                bin7 = scalars[str(run) + '-6-7']
                bin8 = scalars[str(run) + '-6-8']
                bin9 = scalars[str(run) + '-6-9']
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            pol = polarizations[bin7.fill]
            
            asym_zp.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd,
                pol.py,pol.pb)
            asym_zm.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd,
                pol.py,pol.pb)
            asym_fp[bin7.fill].FillFromHistogramManager(mgr, 'jetpatch', 1, 
                uu,ud,du,dd, pol.py,pol.pb)
            asym_fm[bin7.fill].FillFromHistogramManager(mgr, 'jetpatch', -1, 
                uu,ud,du,dd, pol.py,pol.pb)
            
    title = {
        'ly':'Yellow Beam', 
        'lb':'Blue Beam', 
        'ls':'Like-Sign', 
        'us':'Unlike-Sign'
    }
    marker_color = {
        'ly':ROOT.kYellow, 
        'lb':ROOT.kBlue, 
        'ls':ROOT.kRed, 
        'us':ROOT.kBlack
    }
    
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    
    canvases = []
    for key in ('ly','lb','ls','us'):
        final_plus = ROOT.TH1D('final_plus_%s' % key, '', len(fills), 0.5, 
            len(fills)+0.5)
        final_minus = ROOT.TH1D('final_minus_%s' % key, '', len(fills), 0.5, 
            len(fills)+0.5)
        canvases.append([final_minus, final_plus])
        for i,f in enumerate(fills):
            hplus = asym_fp[f].GetAsymmetry(key)
            final_plus.SetBinContent( i+1, hplus.GetBinContent(1) )
            final_plus.SetBinError( i+1, hplus.GetBinError(1) )
            hplus.Delete()
            
            hminus = asym_fm[f].GetAsymmetry(key)
            final_minus.SetBinContent( i+1, hminus.GetBinContent(1) )
            final_minus.SetBinError( i+1, hminus.GetBinError(1) )
            hminus.Delete()
            
        for var,p,m in [('z',asym_zp,asym_zm), ('fill',asym_fp,asym_fm)]:
            c = graphics.canvas2()
            canvases.append(c)
        
            hp = (var == 'z') and p.GetAsymmetry(key) or final_plus
            hp.SetTitle(title[key] + ' SSA for #pi^{+}')
            hp.SetMarkerStyle(21)
    
            hm = (var == 'z') and m.GetAsymmetry(key) or final_minus
            hm.SetTitle(title[key] + ' SSA for #pi^{-}')
            hm.SetMarkerStyle(20)
    
            for h in (hp,hm):
                h.SetMarkerColor(marker_color[key])
                if var == 'fill':
                    h.GetYaxis().SetRangeUser(-0.2, 0.2)
                    h.SetXTitle('fill index')
                elif var == 'z':
                    h.SetXTitle('p_{T}(#pi) / p_{T}(jet)')
                    if key in ('ly', 'lb'):
                        h.GetYaxis().SetRangeUser(-0.05, 0.05)
                    else:
                        h.GetYaxis().SetRangeUser(-0.1, 0.1)
        
            c.cd(1)
            hm.Fit('pol0')
            hm.Draw('e1')
        
            c.cd(2)
            hp.Fit('pol0')
            hp.Draw('e1')
        
            c.Print('%s_ssa_%s.png' % (key, var))
    
    raw_input('wait here:')


def pid_background_asymmetry():
    """
    calculate A_{LL} for particles in the p/K sideband of the dE/dx window
    """
    asym_p = AsymmetryGenerator('asym_p', bins=zbins, key='z_away2_bg')
    asym_m = AsymmetryGenerator('asym_m', bins=zbins, key='z_away2_bg')
    
    scalars = ScalarCounts(os.environ['STAR'] + 
        '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt')
    
    polarizations = Polarizations.Final
    
    ## generate the asymmetries
    allFiles = glob(histDir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = getRun(fname)
        if run in runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname), ['z_away2_bg'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                bin6 = scalars[str(run) + '-6-6']
                bin7 = scalars[str(run) + '-6-7']
                bin8 = scalars[str(run) + '-6-8']
                bin9 = scalars[str(run) + '-6-9']
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            pol = polarizations[bin7.fill]
            
            asym_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
    
    c = graphics.canvas2()
    c.SetLogy(0)
    ROOT.gStyle.SetErrorX(0.0)
    
    c.cd(1)
    hm = asym_m.GetAsymmetry('ll')
    hm.SetMarkerStyle(20)
    hm.SetTitle('Background A_{LL} #pi^{-}')
    hm.Fit('pol0', 'q')
    hm.Draw('e1')
    
    c.cd(2)
    hp = asym_p.GetAsymmetry('ll')
    hp.SetTitle('Background A_{LL} #pi^{+}')
    hp.SetMarkerStyle(21)
    hp.Fit('pol0', 'q')
    hp.Draw('e1')
    
    for h in (hm,hp):
        h.GetXaxis().SetTitle('p_{T}(#pi)/p_{T}(jet)')
        h.GetYaxis().SetRangeUser(-0.2, 0.2)
    
    raw_input('wait here:')
    c.Print('pid_background_asymmetry.png')
    
    print '\nπ- Background Asymmetry'
    for bin in range(1, hm.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hm.GetBinLowEdge(bin), 
            hm.GetBinLowEdge(bin+1), hm.GetBinContent(bin), hm.GetBinError(bin))
    
    print '\nπ+ Background Asymmetry'
    for bin in range(1, hp.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hp.GetBinLowEdge(bin), 
            hp.GetBinLowEdge(bin+1), hp.GetBinContent(bin), hp.GetBinError(bin))
    


def pid_background_fraction():
    """simplistic calculation of background contamination fraction"""
    nsig_p = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 'plus', 
        'away2_nSigmaPion')
    nsig_m = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 'minus', 
        'away2_nSigmaPion')
    
    nsig_p.SetTitle('n#sigma(#pi) for #pi^{+}')
    nsig_m.SetTitle('n#sigma(#pi) for #pi^{-}')
    
    print 'Fit summary for π+'
    pfits = pid_calibration(nsig_p)
    print 'Fit summary for π-'
    mfits = pid_calibration(nsig_m)
    c = graphics.canvas2()
    
    c.cd(1)
    nsig_m.Draw()
    [fit.Draw('same') for fit in mfits[1:]]
    
    c.cd(2)
    nsig_p.Draw()
    [fit.Draw('same') for fit in pfits[1:]]
    
    raw_input('wait here:')
    c.Print('pid_background_fraction.png')


def asigma():
    asym_p = AsymmetryGenerator('asym_p', bins=zbins, key='z_away2')
    asym_m = AsymmetryGenerator('asym_m', bins=zbins, key='z_away2')
    
    scalars = ScalarCounts(os.environ['STAR'] + 
        '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt')
    
    polarizations = Polarizations.Final
    
    ## generate the asymmetries
    allFiles = glob(transHistDir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = getRun(fname)
        if run in transverse_runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname), ['z_away2'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                bin6 = scalars[str(run) + '-6-6']
                bin7 = scalars[str(run) + '-6-7']
                bin8 = scalars[str(run) + '-6-8']
                bin9 = scalars[str(run) + '-6-9']
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            pol = polarizations[bin7.fill]
            
            asym_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
    
    c = graphics.canvas2()
    c.SetLogy(0)
    ROOT.gStyle.SetErrorX(0.0)
    
    c.cd(1)
    hm = asym_m.GetAsymmetry('ll')
    hm.SetMarkerStyle(20)
    hm.SetTitle('A_{#sigma} #pi^{-}')
    hm.Fit('pol0', 'q')
    hm.Draw('e1')
    
    c.cd(2)
    hp = asym_p.GetAsymmetry('ll')
    hp.SetTitle('A_{#sigma} #pi^{+}')
    hp.SetMarkerStyle(21)
    hp.Fit('pol0', 'q')
    hp.Draw('e1')
    
    for h in (hm,hp):
        h.GetXaxis().SetTitle('p_{T}(#pi)/p_{T}(jet)')
        h.GetYaxis().SetRangeUser(-0.1, 0.1)
    
    raw_input('wait here:')
    c.Print('asigma.png')
    
    print '\nπ- A_{σ}'
    for bin in range(1, hm.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hm.GetBinLowEdge(bin), 
            hm.GetBinLowEdge(bin+1), hm.GetBinContent(bin), hm.GetBinError(bin))
    
    print '\nπ+ A_{σ}'
    for bin in range(1, hp.GetNbinsX()+1):
        print '[%.2f-%.2f]  % .3f ± %.3f' % (hp.GetBinLowEdge(bin), 
            hp.GetBinLowEdge(bin+1), hp.GetBinContent(bin), hp.GetBinError(bin))


def mcasym(spin = 'anyspin'):
    """
    comparison of MC asymmetries for minbias and 137222
    """
    if spin == 'anyspin':
        stitle = ''
    else:
        stitle = '_%(spin)s' % locals()
    
    f = ROOT.TFile(mcasymFile)
    keys = ['STD', 'MIN','ZERO','GS_NLOC']
    wkeys = [key+'w' for key in keys]
    mgr = HistogramManager(f, keys=keys+wkeys)
    
    line = ROOT.TLine(zbins[0], 0.0, zbins[-1], 0.0)
    line.SetLineStyle(2)
    ROOT.gStyle.SetErrorX()
    
    color = {
        'STD': ROOT.kBlack,
        'MAX': ROOT.kRed,
        'MIN': ROOT.kGreen,
        'ZERO': ROOT.kBlue,
        'GS_NLOC': ROOT.kMagenta
    }
    
    smooth_factor = 1
    
    cmb = graphics.canvas2()
    cmbw = graphics.canvas2()
    cjp = graphics.canvas2()
    alldiffs = graphics.canvas2('Asymmetry Differences')
    alldiffsw = graphics.canvas2('Reweighted Asymmetry Differences')
    keepme = []
    diffs = {}
    for i,key in enumerate(keys):
        mb_m = mgr[spin]['117001'].tracks_minus[key].Clone()
        mb_p = mgr[spin]['117001'].tracks_plus[key].Clone()
        jp_m = mgr[spin]['jetpatch'].tracks_minus[key].Clone()
        jp_p = mgr[spin]['jetpatch'].tracks_plus[key].Clone()
        mb_mw = mgr[spin]['117001'].tracks_minus[key+'w'].Clone()
        mb_pw = mgr[spin]['117001'].tracks_plus[key+'w'].Clone()
        
        opt = i>0 and 'e2 same' or 'e2'
        
        for h in (mb_m, mb_p, jp_m, jp_p, mb_mw, mb_pw):
            h.Smooth(smooth_factor)
            h.GetXaxis().SetTitle('p_{T}(#pi)/p_{T}(jet)')
            h.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
            h.GetYaxis().SetRangeUser(-0.08,0.08)
            h.SetLineColor(color[key])
            h.SetFillColor(color[key])
        
        cmb.cd(1)
        line.Draw()
        mb_m.SetTitle('MB MC Asymmetries for #pi^{-} %(stitle)s' % locals())
        mb_m.Draw(opt)
        
        cmb.cd(2)
        line.Draw()
        mb_p.SetTitle('MB MC Asymmetries for #pi^{+} %(stitle)s' % locals())
        mb_p.Draw(opt)
        
        cmbw.cd(1)
        line.Draw()
        mb_mw.SetTitle('Reweighted MB MC Asymmetries for #pi^{-} %(stitle)s' \
            % locals())
        mb_mw.Draw(opt)
        
        cmbw.cd(2)
        line.Draw()
        mb_pw.SetTitle('Reweighted MB MC Asymmetries for #pi^{+} %(stitle)s' \
            % locals())
        mb_pw.Draw(opt)
        
        cjp.cd(1)
        line.Draw()
        jp_m.SetTitle('JP1 MC Asymmetries for #pi^{-} %(stitle)s' % locals())
        jp_m.Draw(opt)
        
        cjp.cd(2)
        line.Draw()
        jp_p.SetTitle('JP1 MC Asymmetries for #pi^{+} %(stitle)s' % locals())
        jp_p.Draw(opt)
        
        diff_m = jp_m.Clone()
        diff_m.Add(mb_m, -1)
        
        diff_mw = jp_m.Clone()
        diff_mw.Add(mb_mw, -1)
        
        diff_p = jp_p.Clone()
        diff_p.Add(mb_p, -1)
        
        diff_pw = jp_p.Clone()
        diff_pw.Add(mb_pw, -1)
        
        for h in (diff_m, diff_p):
            h.GetYaxis().SetRangeUser(-0.03, 0.03)
            h.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
            h.SetMarkerStyle(20)
        
        
        for h in (diff_mw, diff_pw):
            h.GetYaxis().SetRangeUser(-0.03, 0.03)
            h.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
            h.SetMarkerStyle(24)
            
        
        cdiff = graphics.canvas2(key)
        
        leg = ROOT.TLegend(0.15, 0.15, 0.6, 0.35)
        leg.SetHeader('Compare triggered asym to:')
        leg.AddEntry(diff_m, 'MB', 'p')
        leg.AddEntry(diff_mw, 'p_{T} reweighted MB', 'p')
        
        cdiff.cd(1)
        if spin == 'anyspin':
            mtitle = '#pi^{-} JP1 - MB for %(key)s' % locals()
            ptitle = '#pi^{+} JP1 - MB for %(key)s' % locals()            
        else:
            mtitle = '#pi^{-} JP1 - MB for %(key)s -- %(spin)s processes only' \
                % locals()
            ptitle = '#pi^{+} JP1 - MB for %(key)s -- %(spin)s processes only' \
                % locals()
        
        diff_m.SetTitle(mtitle)
        diff_mw.SetTitle(mtitle)
        diff_m.Draw('e1')
        diff_mw.Draw('e1 same')
        line.Draw()
        leg.Draw()
        
        cdiff.cd(2)
        diff_p.SetTitle(ptitle)
        diff_pw.SetTitle(ptitle)
        diff_p.Draw('e1')
        diff_pw.Draw('e1 same')
        line.Draw()
        
        cdiff.Print('mcasym_%(key)s_diffs%(stitle)s.png' % locals())
        
        opt2 = i>0 and 'e1 same' or 'e1'
        
        alldiffs.cd(1)
        line.Draw()
        diff_m.Draw(opt2)
        
        alldiffs.cd(2)
        line.Draw()
        diff_p.Draw(opt2)
        
        alldiffsw.cd(1)
        line.Draw()
        diff_mw.Draw(opt2)
        
        alldiffsw.cd(2)
        line.Draw()
        diff_pw.Draw(opt2)
        
        diffs[key] = (diff_mw, diff_pw)
        
        keepme.extend([jp_m, jp_p, cdiff, leg])
    
    # now report the asymmetry difference for each scenario
    for key in keys:
        diff_m, diff_p = diffs[key]
        print '\nπ- asymmetry differences for %(key)s' % locals()
        for bin in range(1, diff_m.GetNbinsX()+1):
            print '[%.2f-%.2f]  % .4f ± %.4f' % (
                diff_m.GetBinLowEdge(bin), diff_m.GetBinLowEdge(bin+1), 
                diff_m.GetBinContent(bin), diff_m.GetBinError(bin) )
                
        print '\nπ+ asymmetry differences for %(key)s' % locals()
        for bin in range(1, diff_m.GetNbinsX()+1):
            print '[%.2f-%.2f]  % .4f ± %.4f' % (
                diff_p.GetBinLowEdge(bin), diff_p.GetBinLowEdge(bin+1), 
                diff_p.GetBinContent(bin), diff_p.GetBinError(bin) )
    
    raw_input('wait here:')
    
    cmb.Print('mcasym_minbias%(stitle)s.png' % locals())
    cmbw.Print('mcasym_minbias_reweight%(stitle)s.png' % locals())
    cjp.Print('mcasym_jetpatch%(stitle)s.png' % locals())
    alldiffs.Print('mcasym_diff%(stitle)s.png' % locals())
    alldiffsw.Print('mcasym_diff_reweight%(stitle)s.png' % locals())


def subprocess_shift():
    import histos
    histos.simu = True
    f = ROOT.TFile(datamcFile)
    mgr = HistogramManager(f, ['hardP'])
    mb = {
        'all': mgr.anyspin['117001']['hardP'],
        'gg': mgr.gg['117001']['hardP'],
        'qg': mgr.qg['117001']['hardP'],
        'qq': mgr.qq['117001']['hardP']
    }
    jp = {
        'all': mgr.anyspin['jetpatch']['hardP'],
        'gg': mgr.gg['jetpatch']['hardP'],
        'qg': mgr.qg['jetpatch']['hardP'],
        'qq': mgr.qq['jetpatch']['hardP']
    }
    print mb
    print jp
    
    for key in ('gg','qg','qq'):
        mb[key].Divide(mb['all'])
        jp[key].Divide(jp['all'])
    
    for h in (mb,jp):
        h['gg'].SetLineColor(ROOT.kRed)
        h['qg'].SetLineColor(ROOT.kBlue)
        h['qq'].SetLineColor(ROOT.kGreen)
    
    c = graphics.canvas2()
    c.cd(1)
    mb['gg'].Draw()
    mb['gg'].SetTitle('MB')
    mb['gg'].GetYaxis().SetRangeUser(0., 0.8)
    mb['gg'].GetXaxis().SetRangeUser(0, 30)
    mb['qg'].Draw('same')
    mb['qq'].Draw('same')
    
    c.cd(2)
    jp['gg'].Draw()
    jp['gg'].SetTitle('JP')
    jp['gg'].SetXTitle('hardP')
    jp['gg'].GetYaxis().SetRangeUser(0., 0.8)
    jp['gg'].GetXaxis().SetRangeUser(0, 30)
    jp['qg'].Draw('same')
    jp['qq'].Draw('same')
    
    raw_input('wait here:')
    


def subprocess_fraction_z(charge = 1):
    import histos
    histos.simu = True
    ROOT.gStyle.SetErrorX()
    f = ROOT.TFile(datamcFile)
    mgr = HistogramManager(f, ['z_away2'])
    mb = {
        'all': mgr.anyspin['117001'].trackHistograms(charge)['z_away2'],
        'gg': mgr.gg['117001'].trackHistograms(charge)['z_away2'],
        'qg': mgr.qg['117001'].trackHistograms(charge)['z_away2'],
        'qq': mgr.qq['117001'].trackHistograms(charge)['z_away2']
    }
    jp = {
        'all': mgr.anyspin['jetpatch'].trackHistograms(charge)['z_away2'],
        'gg': mgr.gg['jetpatch'].trackHistograms(charge)['z_away2'],
        'qg': mgr.qg['jetpatch'].trackHistograms(charge)['z_away2'],
        'qq': mgr.qq['jetpatch'].trackHistograms(charge)['z_away2']
    }
    
    for key in ('gg','qg','qq'):
        mb[key].Divide(mb['all'])
        jp[key].Divide(jp['all'])
    
    for h in (mb,jp):
        h['gg'].SetLineColor(ROOT.kRed)
        h['qg'].SetLineColor(ROOT.kBlue)
        h['qq'].SetLineColor(ROOT.kGreen)
        [h[k].SetMarkerStyle(24) for k in ('gg','qg','qq')]
    
    leg = ROOT.TLegend(0.65, 0.7, 0.85, 0.88)
    leg.AddEntry(mb['gg'], 'gg')
    leg.AddEntry(mb['qg'], 'qg')
    leg.AddEntry(mb['qq'], 'qq')
    
    c = graphics.canvas2()
    c.cd(1)
    mb['gg'].Draw('e1')
    mb['gg'].SetTitle('MB for charge=%d' % charge)
    mb['gg'].GetYaxis().SetRangeUser(0., 0.8)
    mb['gg'].GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    mb['qg'].Draw('e1 same')
    mb['qq'].Draw('e1 same')
    leg.Draw()
    
    c.cd(2)
    jp['gg'].Draw('e1')
    jp['gg'].SetTitle('JP for charge=%d' % charge)
    jp['gg'].GetYaxis().SetRangeUser(0., 0.8)
    jp['gg'].GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    jp['qg'].Draw('e1 same')
    jp['qq'].Draw('e1 same')
    
    c2 = graphics.canvas1()
    delta = {
        'gg': jp['gg'].Clone(),
        'qg': jp['qg'].Clone(),
        'qq': jp['qq'].Clone()
    }
    for k in delta.keys():
        delta[k].Add(mb[k], -1)
        delta[k].SetMarkerStyle(24)
    
    delta['gg'].SetTitle('Subprocess fraction difference (JP - MB) : charge=%d'\
        % charge)
    delta['gg'].GetYaxis().SetRangeUser(-0.4, 0.4)
    delta['gg'].Draw('e1')
    delta['qg'].Draw('e1 same')
    delta['qq'].Draw('e1 same')
    
    leg2 = ROOT.TLegend(0.15, 0.7, 0.25, 0.88)
    leg2.AddEntry(mb['gg'], 'gg')
    leg2.AddEntry(mb['qg'], 'qg')
    leg2.AddEntry(mb['qq'], 'qq')
    leg2.Draw()
    
    line = ROOT.TLine(zbins[0], 0.0, zbins[-1], 0.0)
    line.SetLineStyle(2)
    line.Draw()
    
    raw_input('wait here:')
    
    if charge == 1:
        c.Print('subprocess_fraction_trigger_plus.png')
        c2.Print('subprocess_fraction_difference_plus.png')
    else:
        c.Print('subprocess_fraction_trigger_minus.png')
        c2.Print('subprocess_fraction_difference_minus.png')


def ffcomp():
    """
    this function still needs some work ... seems very buggy
    """
    from analysis.util import tf1
    from analysis.ff import dss
    from math import sqrt
    
    f = ROOT.TFile('/Users/kocolosk/work/2008-09-21-prelim-prep/pythia_ff.hist.root')
    ROOT.gStyle.SetOptLogy()
    ROOT.gStyle.SetErrorX()
    charge = 'minus'
    
    line = ROOT.TLine(0,1,1,1)
    line.SetLineStyle(2)
    def dostuff(c, h, f):
        c.cd(1)
        ROOT.gPad.SetLogy()
        scalefactor = f.Integral(0.2, 0.8) / h.Integral(40, 160, 'width')
        h.Scale(scalefactor)
        h.GetXaxis().SetTitle('z')
        ratio = h.Clone()
        h.Draw()
        f.SetLineColor(ROOT.kRed)
        f.Draw('same')
        c.cd(2)
        ROOT.gPad.SetLogy(0)
        ratio.SetTitle('')
        ratio.SetXTitle('')
        ratio.Sumw2()
        ratio.Divide(f)
        for bin in range(1, ratio.GetNbinsX()+1):
            ratio.SetBinError(bin, ratio.GetBinError(bin)*sqrt(scalefactor))
        ratio.GetYaxis().SetRangeUser(0,2)
        ratio.GetYaxis().SetTitle('PYTHIA/DSS')
        ratio.Draw()
        line.Draw('same')
    
    cu = graphics.canvas1e('u+ubar')
    uubar = f.Get('u_%(charge)s' % locals())
    uubar.Add(f.Get('ubar_%(charge)s' % locals()))
    uubar.SetTitle('u+ubar => #pi^{-}')
    dssu = tf1(dss, 0, 1, flavor='u+ubar', charge=charge)
    dostuff(cu, uubar, dssu)
    
    leg = ROOT.TLegend(0.7, 0.7, 0.85, 0.85)
    leg.AddEntry(uubar, 'PYTHIA')
    leg.AddEntry(dssu, 'DSS')
    
    cu.cd(1)
    leg.Draw()
    cu.Print('uubar.png')
    
    cd = graphics.canvas1e('d+dbar')
    ddbar = f.Get('d_%(charge)s' % locals())
    ddbar.Add(f.Get('dbar_%(charge)s' % locals()))
    ddbar.SetTitle('d+dbar => #pi^{-}')
    dssd = tf1(dss, 0, 1, flavor='d+dbar', charge=charge)
    dostuff(cd, ddbar, dssd)
    
    cd.cd(1)
    leg.Draw()
    cd.Print('ddbar.png')
    
    cg = graphics.canvas1e('g')
    g = f.Get('g_%(charge)s' % locals())
    g.SetTitle('g => #pi^{-}')
    dssg = tf1(dss, 0, 1, flavor='g', charge=charge)
    dostuff(cg, g, dssg)
    
    cg.cd(1)
    leg.Draw()
    cg.Print('g.png')
    
    raw_input('wait here')


def meanpt():
    """plots mean pT in each z bin for for pions and tracks"""
    meanpt_p = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 
        'plus', 'meanpt')
    meanpt_m = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 
        'minus', 'meanpt')
    
    meanjetpt_p = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 
        'plus', 'meanjetpt')
    meanjetpt_m = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 
        'minus', 'meanjetpt')
    
    ## now get some results from simulations
    f = ROOT.TFile(datamcFile)
    mgr = HistogramManager(f, keys=['meanpt', 'meanjetpt'])
    meansim_p = mgr.anyspin['jetpatch'].tracks_plus['meanpt']
    meansim_m = mgr.anyspin['jetpatch'].tracks_minus['meanpt']
    meanjetsim_p = mgr.anyspin['jetpatch'].tracks_plus['meanjetpt']
    meanjetsim_m = mgr.anyspin['jetpatch'].tracks_minus['meanjetpt']
    
    mbsim_p = mgr.anyspin['117001'].tracks_plus['meanpt']
    mbsim_m = mgr.anyspin['117001'].tracks_minus['meanpt']
    mbjetsim_p = mgr.anyspin['117001'].tracks_plus['meanjetpt']
    mbjetsim_m = mgr.anyspin['117001'].tracks_minus['meanjetpt']
    
    meanjetpt_m.SetTitle('JP Data / Monte Carlo comparison, #pi -')
    meanjetpt_p.SetTitle('JP Data / Monte Carlo comparison, #pi +')
    
    meanjetsim_m.SetTitle('Monte Carlo #pi -')
    meanjetsim_p.SetTitle('Monte Carlo #pi +')
    
    for h in (meanjetpt_m, meanjetpt_p, meanjetsim_m, meanjetsim_p):
        h.SetMarkerStyle(21)
        h.SetXTitle('z')
        h.SetYTitle('< pT >')
    
    [h.SetMarkerStyle(20) for h in (mbjetsim_m, mbjetsim_p)]
    [h.SetMarkerStyle(24) for h in (mbsim_m, mbsim_p)]
    
    for h in (meanpt_m, meanpt_p, meansim_m, meansim_p):
        h.SetMarkerStyle(25)
    
    for h in (meansim_m, meansim_p, meanjetsim_m, meanjetsim_p):
        h.SetMarkerColor(ROOT.kRed)
        h.SetLineColor(ROOT.kRed)
    
    leg = ROOT.TLegend(.6, .7, .8, .88)
    leg.AddEntry(meanjetpt_p, 'jet')
    leg.AddEntry(meanpt_p, '#pi')
    
    ## first compare JP data/MC
    c = graphics.canvas2('Data / Monte Carlo comparison for JP')
    c.cd(1)
    meanjetpt_m.Draw()
    meanjetpt_m.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    meanjetpt_m.GetYaxis().SetRangeUser(0, 25)
    meanjetsim_m.Draw('][ hist same')
    meanpt_m.Draw('same')
    meansim_m.Draw('][ hist same')
    
    c.cd(2)
    meanjetpt_p.Draw()
    meanjetpt_p.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    meanjetpt_p.GetYaxis().SetRangeUser(0, 25)
    meanjetsim_p.Draw('][ hist same')
    meanpt_p.Draw('same')
    meansim_p.Draw('][ hist same')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('jp-means.png')
    
    ## now compare JP and MB simulations
    c2 = graphics.canvas2('Comparison of MB and JP simulations')
    
    for h in (meanjetsim_m, meanjetsim_p, meansim_m, meansim_p):
        h.SetMarkerColor(ROOT.kBlue)
        h.SetLineColor(ROOT.kBlue)
    for h in (mbjetsim_m, mbjetsim_p, mbsim_m, mbsim_p):
        h.SetMarkerColor(ROOT.kRed)
        h.SetLineColor(ROOT.kRed)
    
    c2.cd(1)
    meanjetsim_m.Draw('hist p')
    meanjetsim_m.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    meanjetsim_m.GetYaxis().SetRangeUser(0, 25)
    mbjetsim_m.Draw('hist p same')
    mbsim_m.Draw('hist p same')
    meansim_m.Draw('hist p same')
    
    c2.cd(2)
    meanjetsim_p.Draw('hist p')
    meanjetsim_p.GetXaxis().SetRangeUser(zbins[0], zbins[-1])
    meanjetsim_p.GetYaxis().SetRangeUser(0, 25)
    mbjetsim_p.Draw('hist p same')
    mbsim_p.Draw('hist p same')
    meansim_p.Draw('hist p same')
    
    leg2 = ROOT.TLegend(.68, .7, .88, .88)
    leg2.AddEntry(meanjetsim_p, 'JP')
    leg2.AddEntry(mbjetsim_p, 'MB')
    leg2.Draw()
    
    raw_input('wait here:')
    
    c.Print('jp-means.png')
    c2.Print('simu-means.png')


def minbias_pt_reweight():
    """plots ratio of JP and MB jet spectrum and fits it"""
    f = ROOT.TFile(datamcFile)
    mgr = HistogramManager(f, keys=['true_jet_pt'])
    mb = mgr.anyspin['117001']['true_jet_pt']
    jp = mgr.anyspin['jetpatch']['true_jet_pt']
    jp.Divide(mb)
    
    jp.SetStats(True)
    jp.GetXaxis().SetRangeUser(5,30)
    jp.GetYaxis().SetRangeUser(0.,1.)
    jp.SetXTitle('corrected jet pT')
    jp.SetYTitle('nJets (JP) / nJets (MB)')
    
    fit = ROOT.TF1('fit', 'pol1', 9.8, 25.3)
    
    # sigmoid = ROOT.TF1('sigmoid', '[0]*log([1]*x+[2])', 9.8 , 25.3)
    # sigmoid.SetParameter(0, 1.)
    # sigmoid.SetParameter(1, 0.08)
    # sigmoid.SetParameter(2, 0.)
    c1 = graphics.canvas1()
    jp.Fit(fit, 'r')
    ROOT.gStyle.SetOptFit(111)
    raw_input('wait here:')
    c1.Print('minbias_pt_reweight.png')
    
    jp.GetYaxis().SetRangeUser(1.E-5,1.)
    ROOT.gPad.SetLogy()
    raw_input('wait here:')
    c1.Print('minbias_pt_reweight_log.png')


def pt_shift_uncertainty():
    ## this is my guess based on inverting Dave's fit
    measured_pt = [10.31, 12.91, 15.58, 19.06, 23.20, 28.45]
    
    ## http://cyclotron.tamu.edu/star/2005n06Jets/PRDweb/
    ## "numbers are from the preliminary analysis"
    total_uncertainty = [ 0.72, 0.90, 0.99, 1.09, 1.27, 1.52 ]
    
    c = graphics.canvas1()
    
    low = ROOT.TGraph(len(measured_pt))
    for i,(pt,error) in enumerate(zip(measured_pt, total_uncertainty)):
        low.SetPoint(i, pt, shifted(pt)-error)
    lowfit = ROOT.TF1('lowfit', 'pol2', 10, 30)
    lowfit.SetLineStyle(2)
    low.Fit(lowfit)
    low.GetXaxis().SetTitle('measured jet p_{T}')
    low.GetXaxis().SetRangeUser(10,30)
    low.GetYaxis().SetTitle('corrected jet p_{T}')
    low.GetYaxis().SetRangeUser(9,26)
    low.SetTitle('Uncertainty on p_{T} shift')
    low.SetLineStyle(2)
    low.Draw('ap2')
    
    high = ROOT.TGraph(len(measured_pt))
    for i,(pt,error) in enumerate(zip(measured_pt, total_uncertainty)):
        high.SetPoint(i, pt, shifted(pt)+error)
    highfit = ROOT.TF1('lowfit', 'pol2', measured_pt[0], measured_pt[-1])
    highfit.SetLineStyle(2)
    high.Fit(highfit)
    high.SetLineStyle(2)
    high.Draw('same')
    
    mid = ROOT.TF1('mid', lambda x: shifted(x[0]), 10.31, 28.45)
    mid.Draw('same')
    
    raw_input('wait here:')

def pt_shift_uncertainty_asymmetry():
    """
    calculate A_{LL} assuming different jet pt shifts
    """
    asym_pl = AsymmetryGenerator('asym_pl', bins=zbins, key='z_away2_low')
    asym_pm = AsymmetryGenerator('asym_pm', bins=zbins, key='z_away2')
    asym_ph = AsymmetryGenerator('asym_ph', bins=zbins, key='z_away2_high')
    asym_ml = AsymmetryGenerator('asym_ml', bins=zbins, key='z_away2_low')
    asym_mm = AsymmetryGenerator('asym_mm', bins=zbins, key='z_away2')
    asym_mh = AsymmetryGenerator('asym_mh', bins=zbins, key='z_away2_high')
    
    scalars = ScalarCounts(os.environ['STAR'] + 
        '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt')
    
    polarizations = Polarizations.Final
    
    ## generate the asymmetries
    allFiles = glob(histDir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = getRun(fname)
        if run in runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname), ['z_away2', 
                'z_away2_low', 'z_away2_high'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                bin6 = scalars[str(run) + '-6-6']
                bin7 = scalars[str(run) + '-6-7']
                bin8 = scalars[str(run) + '-6-8']
                bin9 = scalars[str(run) + '-6-9']
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            pol = polarizations[bin7.fill]
            
            asym_ml.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_mm.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_mh.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_pl.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_pm.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_ph.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
    
    
    c = graphics.canvas2()
    c.SetLogy(0)
    ROOT.gStyle.SetErrorX(0.0)
    
    c.cd(1)
    hml = asym_ml.GetAsymmetry('ll')
    hmm = asym_mm.GetAsymmetry('ll')
    hmh = asym_mh.GetAsymmetry('ll')
    hmm.SetMarkerStyle(20)
    hmm.SetTitle('p_{T} shift uncertainty on A_{LL} #pi^{-}')
    # hml.Fit('pol0', 'q')
    hmm.Draw('e1')
    gml = ROOT.TGraphErrors(hml)
    gmh = ROOT.TGraphErrors(hmh)
    for i in range(hmm.GetNbinsX()):
        gml.SetPoint(i, hml.GetBinCenter(i+1)-0.02, hml.GetBinContent(i+1))
        gmh.SetPoint(i, hmh.GetBinCenter(i+1)+0.02, hmh.GetBinContent(i+1))
    gml.Draw('p')
    gmh.Draw('p')
    
    c.cd(2)
    hpl = asym_pl.GetAsymmetry('ll')
    hpm = asym_pm.GetAsymmetry('ll')
    hph = asym_ph.GetAsymmetry('ll')
    hpm.SetTitle('p_{T} shift uncertainty on A_{LL} #pi^{+}')
    hpm.SetMarkerStyle(21)
    # hpl.Fit('pol0', 'q')
    hpm.Draw('e1')
    gpl = ROOT.TGraphErrors(hpl)
    gph = ROOT.TGraphErrors(hph)
    for i in range(hpm.GetNbinsX()):
        gpl.SetPoint(i, hpl.GetBinCenter(i+1)-0.02, hpl.GetBinContent(i+1))
        gph.SetPoint(i, hph.GetBinCenter(i+1)+0.02, hph.GetBinContent(i+1))
    gpl.Draw('p')
    gph.Draw('p')
    
    for g in (gml, gmh):
        g.SetMarkerStyle(24)
    for g in (gpl, gph):
        g.SetMarkerStyle(25)
    
    for h in (hmm,hpm):
        h.GetXaxis().SetTitle('p_{T}(#pi)/p_{T}(jet)')
        h.GetYaxis().SetRangeUser(-0.06, 0.08)
    
    raw_input('wait here:')
    c.Print('pt_shift_uncertainty_asymmetry.png')
    
    print '\nπ- shift asymmetry'
    for bin in range(1, hmm.GetNbinsX()+1):
        low = hml.GetBinContent(bin)
        mid = hmm.GetBinContent(bin)
        high = hmh.GetBinContent(bin)
        diffl = abs(mid-low)
        diffh = abs(mid-high)
        diff = (diffl+diffh)/2
        print '[%.2f-%.2f]  % .3f low, %.3f high, %.3f avg' % \
            (hmm.GetBinLowEdge(bin), hmm.GetBinLowEdge(bin+1),diffl,diffh,diff)
    
    print '\nπ+ shift asymmetry'
    for bin in range(1, hpm.GetNbinsX()+1):
        low = hpl.GetBinContent(bin)
        mid = hpm.GetBinContent(bin)
        high = hph.GetBinContent(bin)
        diffl = abs(mid-low)
        diffh = abs(mid-high)
        diff = (diffl+diffh)/2
        print '[%.2f-%.2f]  % .3f low, %.3f high, %.3f avg' % \
            (hpm.GetBinLowEdge(bin), hpm.GetBinLowEdge(bin+1),diffl,diffh,diff)


def dphi():
    data = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 'sum', \
        'dphi')
    mgr = HistogramManager(ROOT.TFile(datamcFile), keys=['dphi'])
    simu = mgr.anyspin['jetpatch'].tracks_sum['dphi']
    c = graphics.canvas1()
    simu.SetLineColor(ROOT.kRed)
    simu.GetXaxis().SetTitle('#Delta#phi')
    simu.GetYaxis().SetTitle('normalized yield')
    simu.DrawNormalized('hist')
    
    data.SetMarkerStyle(20)
    data.DrawNormalized('same')
    raw_input('wait here:')

