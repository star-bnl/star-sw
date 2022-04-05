# encoding: utf-8

from array import array
from glob import glob
from math import sqrt
from uuid import uuid1 as uuid
import sqlite3 as sqlite

import ROOT
import analysis.pid
from analysis.asym import AsymmetryGenerator, Polarizations
from analysis.plots import graphics
from analysis.histos2 import HistogramManager
from analysis.util import hadd_interactive, getRun
# from analysis.runlists import final_runlist_run5 as runlist
from analysis.runlists import golden_runlist_c as runlist

simuFile = '/Users/kocolosk/data/run5-simu/hist/merged.cphist.root'
# mcasymFile = '/Users/kocolosk/data/run6-simu/hist/mcasym_10.cphist.root'
# mcasymFile = '/Users/kocolosk/work/2009-03-18-run5-mcasym/mcasym.root'
# mcasymFile = '/Users/kocolosk/data/run5-simu/mcasym-true.root'
# mcasymFile = '/Users/kocolosk/data/run5-simu/mcasym-pythia.root'
mcasymFile = '/Users/kocolosk/work/2009-03-28-mcasym-reweight/mcasym_reweighted.root'
histDir = '/Users/kocolosk/data/run5/hist'
# pidFile = '/Users/kocolosk/work/2009-03-13-pid-3d/nsigma_fits.root'
pidFile = '/Users/kocolosk/nsigma_fits.root'
finalResultFile = '/Users/kocolosk/data/run5/final_result.root'

ptbins = [2.00, 3.18, 4.56, 6.32, 8.80, 12.84]

def final_result():
    ROOT.gStyle.SetErrorX(0)
    ROOT.gStyle.SetOptDate(0)
    
    f = ROOT.TFile(finalResultFile)
    hm = f.Get('final_minus')
    hp = f.Get('final_plus')
    
    ## systematic uncertainties x 10^-3 
    
    ## trigger bias dropping MAX
    trig_m_l = [12.00, 13.00, 16.00, 21.00,  9.00]
    trig_m_h = [ 8.00, 10.00, 12.00, 13.00, 18.00]
    trig_p_l = [ 8.00, 35.00, 27.00, 41.00, 10.00]
    trig_p_h = [ 8.00, 14.00, 12.00, 31.00, 10.00]
    
    ## trigger bias dropping MIN, MAX
    trig_m_l = [12.00, 13.00, 16.00, 21.00,  9.00]
    trig_m_h = [ 5.00,  8.00,  8.00, 10.00, 11.00]
    trig_p_l = [ 5.00, 35.00, 27.00, 41.00, 10.00]
    trig_p_h = [ 5.00, 14.00, 11.00, 12.00, 10.00]
    
    ## trigger bias dropping MIN, MAX, P*
    trig_m_l = [5.00,  5.00,  7.00,  5.00,  4.00]
    trig_m_h = [5.00,  8.00,  7.00,  3.00, 11.00]
    trig_p_l = [4.00, 13.00, 11.00, 21.00,  7.00]
    trig_p_h = [4.00, 14.00, 11.00, 12.00,  4.00]
    
    ## trig+reco dropping MIN, MAX, P*
    trig_m_l = [5.30,  4.90,  6.70,  4.50,  3.40]
    trig_m_h = [5.30,  9.10,  6.70,  3.00,  6.50]
    trig_p_l = [4.20, 13.20,  9.30, 20.30, 13.70]
    trig_p_h = [4.20, 14.30,  2.40,  9.10,  4.00]
    
    ## reco dropping MIN, MAX, P*
    # trig_m_l = [0.30,  1.30,  1.20,  4.70,  6.40]
    # trig_m_h = [0.30,  1.50,  1.20,  4.70,  6.40]
    # trig_p_l = [0.30,  1.10,  8.60,  3.80,  6.90]
    # trig_p_h = [0.30,  1.10,  5.40,  3.80,  4.50]
    
    ## trig+reco after rescaling, dropping MAX,MIN,P*
    trig_m_l = [5.90,  8.30,  9.30,  7.20,  4.80]
    trig_m_h = [2.70,  7.20,  3.40,  3.60,  5.70]
    trig_p_l = [6.10, 12.80, 17.60, 20.90, 15.20]
    trig_p_h = [6.10,  6.60, 10.10, 18.60,  6.20]
    
    asigma_m = [1.80, 1.80, 1.80, 1.80, 1.80]
    asigma_p = [1.80, 1.80, 1.80, 1.80, 1.80]
    
    pid_m = [0.00, 0.00, 0.00, 0.00, 0.00]
    pid_p = [0.00, 0.00, 0.00, 0.00, 0.00]
    
    rellumi = [0.98, 0.98, 0.98, 0.98, 0.98]
    
    sum_quadrature = lambda *args: sqrt( sum([a**2 for a in args]) )/1E3
    
    syst_m = ROOT.TGraphAsymmErrors(5, 
        array('d', [hm.GetBinCenter(bin) for bin in range(1,6)]),
        array('d', [hm.GetBinContent(bin) for bin in range(1,6)]),
        array('d', [0.2 for i in range(5)]),      ## exl
        array('d', [0.2 for i in range(5)]),      ## exh
        array('d', map(sum_quadrature, trig_m_l, asigma_m, pid_m, rellumi)),
        array('d', map(sum_quadrature, trig_m_h, asigma_m, pid_m, rellumi))
    )
    
    syst_p = ROOT.TGraphAsymmErrors(5,
        array('d', [hp.GetBinCenter(bin) for bin in range(1,6)]),
        array('d', [hp.GetBinContent(bin) for bin in range(1,6)]),
        array('d', [0.2 for i in range(5)]),      ## exl
        array('d', [0.2 for i in range(5)]),      ## exh
        array('d', map(sum_quadrature, trig_p_l, asigma_p, pid_p, rellumi)),
        array('d', map(sum_quadrature, trig_p_h, asigma_p, pid_p, rellumi))
    )
    
    ## auto-generate LaTeX tables
    for i in range(5):
        print '[%.2f - %.2f] & %.4f $\pm$ %.4f (stat) -%.4f +%.4f (syst)\\\\'\
            % (ptbins[i], ptbins[i+1], 
            hm.GetBinContent(i+1), hm.GetBinError(i+1),
            syst_m.GetErrorYlow(i), syst_m.GetErrorYhigh(i))
        print '\\hline'
    
    for i in range(5):
        print '[%.2f - %.2f] & %.4f $\pm$ %.4f (stat) -%.4f +%.4f (syst)\\\\'\
            % (ptbins[i], ptbins[i+1],
            hp.GetBinContent(i+1), hp.GetBinError(i+1),
            syst_p.GetErrorYlow(i), syst_p.GetErrorYhigh(i))
        print '\\hline'
    
    ## NLO curves
    from analysis.asym import theoryCurves
    nlo_m = {
        'STD': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_std, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        'ZERO': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_zero, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        # 'MAX': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_max, 
        #     analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        'MIN': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_min, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),    
        'GSC': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_gsc, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph()
    }
    nlo_p = {
        'STD': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_std, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'ZERO': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_zero, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        # 'MAX': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_max, 
        #     analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'MIN': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_min, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'GSC': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_gsc, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph()
    }
    
    for nlo in (nlo_m, nlo_p):
        nlo['ZERO'].SetLineStyle(3)
        nlo['ZERO'].SetLineColor(ROOT.kBlue)
        # nlo['MAX'].SetLineStyle(4)
        # nlo['MAX'].SetLineColor(ROOT.kRed)
        nlo['MIN'].SetLineStyle(2)
        nlo['MIN'].SetLineColor(ROOT.kGreen)
        nlo['GSC'].SetLineStyle(5)
        nlo['GSC'].SetLineColor(ROOT.kMagenta)
        [gr.SetLineWidth(3) for gr in nlo.values()]
    
    ## graphics setup
    for h in (hm,hp):
        # h.GetYaxis().SetRangeUser(-0.14, 0.09)
        h.GetYaxis().SetRangeUser(-0.12, 0.04)
        h.GetYaxis().SetNdivisions(507)
        h.GetYaxis().SetTitle('A_{LL}  ')
        h.GetYaxis().SetTitleSize(0.06)
        h.GetYaxis().SetTitleOffset(0.9)
        h.GetXaxis().SetTitle('p_{T}  [GeV/c]  ')
        h.GetXaxis().SetTitleSize(0.045)
        h.GetXaxis().SetTitleOffset(1.16)
    
    for g in (syst_m,syst_p):
        g.SetLineColor(ROOT.kRed)
        g.SetFillColor(15)
    
    line = ROOT.TLine(2.00, 0.0, 12.84, 0.0)
    line.SetLineStyle(2)
    
    # leg = ROOT.TLegend(0.17, 0.17, 0.52, 0.47)
    leg = ROOT.TLegend(0.17, 0.17, 0.7, 0.3)
    leg.SetNColumns(2)
    leg.SetBorderSize(0)
    leg.AddEntry(nlo_m['STD'], 'STD', 'l')
    # leg.AddEntry(nlo_m['MAX'], 'MAX', 'l')
    leg.AddEntry(nlo_m['MIN'], 'MIN', 'l')
    leg.AddEntry(nlo_m['GSC'], 'GS Set C', 'l')
    leg.AddEntry(nlo_m['ZERO'], 'ZERO', 'l')
    
    latex = ROOT.TLatex()
    latex.SetTextSize(0.24)
    
    poltext = ROOT.TLatex()
    poltext.SetTextSize(0.0415)
    
    c = graphics.canvas2()
    
    for i in (1, 2):
        pad = c.cd(i)
        pad.SetTopMargin(0.03)
        pad.SetBottomMargin(0.12)
        pad.SetLeftMargin(0.12)
        pad.SetRightMargin(0.02)
        
    c.cd(1)
    hm.Draw('e1')
    [ g.Draw('l') for g in nlo_m.values() ]
    line.Draw()
    # latex.DrawLatex(2.5, 0.05, '#pi^{-}')
    latex.DrawLatex(3.0, -0.07, '#pi^{-}')
    text = '#pm 9.4% polarization scale uncertainty not shown'
    # poltext.DrawLatex(2.5, -0.125, text)
    poltext.DrawLatex(2.5, -0.105, text)
    syst_m.Draw('2p')
    hm.Draw('e1 same')
    
    c.cd(2)
    hp.Draw('e1')
    [ g.Draw('l') for g in nlo_p.values() ]
    line.Draw()
    leg.Draw()
    # latex.DrawLatex(2.5, 0.05, '#pi^{+}')
    latex.DrawLatex(3.0, -0.07, '#pi^{+}')
    syst_p.Draw('2p')
    hp.Draw('e1 same')
    
    graphics.maybe_save(c)


def mcjet_ptprofile(fname='ptprofiles.cphist.root'):
    ROOT.gStyle.SetErrorX(False)
    
    mgr = HistogramManager(ROOT.TFile(fname))
    minbias = mgr['anyspin']['96011']['sum']['ptprofiles_mcjetpt']
    jp1     = mgr['anyspin']['96221']['sum']['ptprofiles_mcjetpt']
    jp2     = mgr['anyspin']['96233']['sum']['ptprofiles_mcjetpt']
    
    jp1.SetMarkerStyle(27)
    jp1.SetMarkerColor(ROOT.kBlue)
    jp1.SetLineColor(ROOT.kBlue)
    
    jp2.SetMarkerStyle(25)
    jp2.SetMarkerColor(ROOT.kRed)
    jp2.SetLineColor(ROOT.kRed)
    
    minbias.SetTitle('')
    minbias.SetMarkerStyle(21)
    minbias.GetXaxis().SetRangeUser(1, 12)
    minbias.GetYaxis().SetRangeUser(3, 25)
    
    c = graphics.canvas1()
    minbias.Draw('e1')
    jp2.Draw('same e1')
    jp1.Draw('same e1')
    
    leg = ROOT.TLegend(0.15, 0.65, 0.3, 0.85)
    leg.AddEntry(minbias.obj, 'MB')
    leg.AddEntry(jp1.obj, 'JP1')
    leg.AddEntry(jp2.obj, 'JP2')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('jetpt-profile.png')

def jet_ptprofile(fname='ptprofiles.cphist.root'):
    ROOT.gStyle.SetErrorX(False)
    
    mgr = HistogramManager(ROOT.TFile(fname))
    minbias = mgr['anyspin']['96011']['sum']['ptprofiles_jetpt']
    jp1     = mgr['anyspin']['96221']['sum']['ptprofiles_jetpt']
    jp2     = mgr['anyspin']['96233']['sum']['ptprofiles_jetpt']
    
    jp1.SetMarkerStyle(27)
    jp1.SetMarkerColor(ROOT.kBlue)
    jp1.SetLineColor(ROOT.kBlue)
    
    jp2.SetMarkerStyle(25)
    jp2.SetMarkerColor(ROOT.kRed)
    jp2.SetLineColor(ROOT.kRed)
    
    minbias.SetTitle('')
    minbias.SetMarkerStyle(21)
    minbias.GetXaxis().SetRangeUser(1, 12)
    minbias.GetYaxis().SetRangeUser(3, 25)
    
    c = graphics.canvas1()
    minbias.Draw('e1')
    jp2.Draw('same e1')
    jp1.Draw('same e1')
    
    leg = ROOT.TLegend(0.15, 0.65, 0.3, 0.85)
    leg.AddEntry(minbias, 'MB')
    leg.AddEntry(jp1, 'JP1')
    leg.AddEntry(jp2, 'JP2')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('mcjet-profile.png')

def x_ptprofile(fname='ptprofiles.cphist.root'):
    ROOT.gStyle.SetErrorX(False)
    
    mgr = HistogramManager(ROOT.TFile(fname))
    minbias = mgr['anyspin']['96011']['sum']['ptprofiles_x']
    jp1     = mgr['anyspin']['96221']['sum']['ptprofiles_x']
    jp2     = mgr['anyspin']['96233']['sum']['ptprofiles_x']
    
    jp1.SetMarkerStyle(27)
    jp1.SetMarkerColor(ROOT.kBlue)
    jp1.SetLineColor(ROOT.kBlue)
    
    jp2.SetMarkerStyle(25)
    jp2.SetMarkerColor(ROOT.kRed)
    jp2.SetLineColor(ROOT.kRed)
    
    minbias.SetTitle('')
    minbias.SetMarkerStyle(21)
    minbias.GetXaxis().SetRangeUser(1, 12)
    minbias.GetYaxis().SetRangeUser(0.03, 0.25)
    
    c = graphics.canvas1()
    minbias.Draw('e1')
    jp2.Draw('same e1')
    jp1.Draw('same e1')
    
    leg = ROOT.TLegend(0.15, 0.65, 0.3, 0.85)
    leg.AddEntry(minbias, 'MB')
    leg.AddEntry(jp1, 'JP1')
    leg.AddEntry(jp2, 'JP2')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('x-profile.png')

def subprocess_shift(fname = simuFile):
    mgr = HistogramManager(ROOT.TFile(fname))
    mb = {
        'all': mgr.anyspin['96011']['hardP'],
        'gg': mgr.gg['96011']['hardP'],
        'qg': mgr.qg['96011']['hardP'],
        'qq': mgr.qq['96011']['hardP']
    }
    jp = {
        'all': mgr.anyspin['96233']['hardP'],
        'gg': mgr.gg['96233']['hardP'],
        'qg': mgr.qg['96233']['hardP'],
        'qq': mgr.qq['96233']['hardP']
    }
    
    for key in ('gg','qg','qq'):
        mb[key].Divide(mb['all'])
        jp[key].Divide(jp['all'])
    
    for h in mb.values():
        h.SetMarkerStyle(20)
        h.SetMarkerSize(0.8)
    
    for h in jp.values():
        h.SetMarkerStyle(24)
        h.SetMarkerSize(0.8)
    
    for h in (mb,jp):
        h['gg'].SetLineColor(ROOT.kRed)
        h['gg'].SetMarkerColor(ROOT.kRed)
        h['qg'].SetLineColor(ROOT.kBlue)
        h['qg'].SetMarkerColor(ROOT.kBlue)
        h['qq'].SetLineColor(ROOT.kGreen)
        h['qq'].SetMarkerColor(ROOT.kGreen)
    
    mb['gg'].SetTitle('Subprocess Fraction')
    mb['gg'].SetXTitle('partonic p_{T}')
    mb['gg'].GetYaxis().SetRangeUser(0., 0.7)
    mb['gg'].GetXaxis().SetRangeUser(3, 30)
    
    c = graphics.canvas1()
    mb['gg'].Draw('e1')
    mb['qg'].Draw('e1 same')
    mb['qq'].Draw('e1 same')
    
    jp['gg'].Draw('e1 same')
    jp['qg'].Draw('e1 same')
    jp['qq'].Draw('e1 same')
    
    leg = ROOT.TLegend(0.6, 0.8, 0.88, 0.88, 'filled = MB, open = JP2')
    leg.SetNColumns(3)
    leg.AddEntry(mb['gg'], 'gg')
    leg.AddEntry(mb['qg'], 'qg')
    leg.AddEntry(mb['qq'], 'qq')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('subprocess-shift.png')

def track_cuts_summary():
    canvas = [ graphics.canvas1() for i in range(4) ]
    
    def dostuff(name, xmin, xmax):
        h = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 'sum', name)
        integral = h.Integral()
        bin1 = h.FindBin(xmin)
        bin2 = h.FindBin(xmax)
        h2 = h.Clone()
        for bin in range(1, bin1):
            h2.SetBinContent(bin, 0)
        for bin in range(bin2+1, h.GetNbinsX()):
            h2.SetBinContent(bin, 0)
        h2.SetFillColor(ROOT.kGreen)
        efficiency = 100*h2.Integral()/integral
        h.Draw()
        h.SetTitle(h.GetTitle() + ' -- %.0f%% pass' % (efficiency,))
        h2.Draw('same')
    
    canvas[0].cd()
    dostuff('eta', -0.99, 0.99)
    
    canvas[1].cd()
    dostuff('dcaG', 0., 0.99)
    
    canvas[2].cd()
    dostuff('nHitsFit', 25, 100)
    
    canvas[3].cd()
    dostuff('nSigmaPion', -0.99, 1.99)
    
    raw_input('wait here:')
    canvas[0].Print('eta-cut.png')
    canvas[1].Print('nHitsFit-cut.png')
    canvas[2].Print('dcaG-cut.png')
    canvas[3].Print('nSigmaPion-cut.png')

def z_profile():
    ROOT.gStyle.SetErrorX(False)
    
    mgr = HistogramManager(ROOT.TFile('zprofiles.cphist.root'))
    minbias = mgr['anyspin']['96011']['sum']['ptprofiles_z']
    jp1     = mgr['anyspin']['96221']['sum']['ptprofiles_z']
    jp2     = mgr['anyspin']['96233']['sum']['ptprofiles_z']
    
    jp1.SetMarkerStyle(27)
    jp1.SetMarkerColor(ROOT.kBlue)
    jp1.SetLineColor(ROOT.kBlue)
    
    jp2.SetMarkerStyle(25)
    jp2.SetMarkerColor(ROOT.kRed)
    jp2.SetLineColor(ROOT.kRed)
    
    minbias.SetTitle('')
    minbias.SetMarkerStyle(21)
    minbias.GetXaxis().SetRangeUser(1, 12)
    minbias.GetYaxis().SetRangeUser(0.1, 1.0)
    
    c = graphics.canvas1()
    minbias.Draw('e1')
    jp2.Draw('same e1')
    jp1.Draw('same e1')
    
    leg = ROOT.TLegend(0.15, 0.65, 0.3, 0.85)
    leg.AddEntry(minbias, 'MB')
    leg.AddEntry(jp1, 'JP1')
    leg.AddEntry(jp2, 'JP2')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('z-profile.png')

def mcz_profile():
    ROOT.gStyle.SetErrorX(False)
    
    mgr = HistogramManager(ROOT.TFile('zprofiles.cphist.root'))
    minbias = mgr['anyspin']['96011']['sum']['ptprofiles_mcz']
    jp1     = mgr['anyspin']['96221']['sum']['ptprofiles_mcz']
    jp2     = mgr['anyspin']['96233']['sum']['ptprofiles_mcz']
    
    jp1.SetMarkerStyle(27)
    jp1.SetMarkerColor(ROOT.kBlue)
    jp1.SetLineColor(ROOT.kBlue)
    
    jp2.SetMarkerStyle(25)
    jp2.SetMarkerColor(ROOT.kRed)
    jp2.SetLineColor(ROOT.kRed)
    
    minbias.SetTitle('')
    minbias.SetMarkerStyle(21)
    minbias.GetXaxis().SetRangeUser(1, 12)
    minbias.GetYaxis().SetRangeUser(0.1, 1.0)
    
    c = graphics.canvas1()
    minbias.Draw('e1')
    jp2.Draw('same e1')
    jp1.Draw('same e1')
    
    leg = ROOT.TLegend(0.15, 0.65, 0.3, 0.85)
    leg.AddEntry(minbias, 'MB')
    leg.AddEntry(jp1, 'JP1')
    leg.AddEntry(jp2, 'JP2')
    leg.Draw()
    
    raw_input('wait here:')
    c.Print('mcz-profile.png')

def mcasym(scenario='trig+reco', spin = 'anyspin'):
    """
    comparison of MC asymmetries for MB and JP2
    """
    stitle = ''
    if spin != 'anyspin': stitle = '_%(spin)s' % locals()
    
    keys = ['STD','MIN','ZERO','MAX', 'DSSV', 'M015', 'M030', 'M045', 'M060',
        'M075', 'M090', 'M105', 'P030', 'P045', 'P060', 'P070']
    mgr = HistogramManager(ROOT.TFile(mcasymFile))
    
    line = ROOT.TLine(2.0, 0.0, 12.84, 0.0)
    line.SetLineStyle(2)
    ROOT.gStyle.SetErrorX()
    
    color = {
        'STD': ROOT.kBlack,
        'MAX': ROOT.kRed,
        'MIN': ROOT.kGreen,
        'ZERO': ROOT.kBlue,
        'DSSV': 8,
        'M015': ROOT.kMagenta,
        'M030': ROOT.kMagenta,
        'M045': ROOT.kMagenta,
        'M060': ROOT.kMagenta,
        'M075': ROOT.kMagenta,
        'M090': ROOT.kMagenta,
        'M105': ROOT.kMagenta,
        'P030': 7,
        'P045': 7,
        'P060': 7,
        'P070': 7,
        'GS_NLOC': ROOT.kMagenta
    }
    
    alldiffs = graphics.canvas2('Asymmetry Differences')
    allsigmas = graphics.canvas2('Asymmetry Uncertainties')
    
    keepme = []
    diffs = {}
    sigmas = {}
    for i,key in enumerate(keys):
        if 'reco' in scenario:
            mdenom = mgr[spin]['96011'].tracks_minus['pt_true_'+key]
            pdenom = mgr[spin]['96011'].tracks_plus['pt_true_'+key]
            denom_title = 'true'
        else:
            mdenom = mgr[spin]['96011'].tracks_minus['pt_'+key]
            pdenom = mgr[spin]['96011'].tracks_plus['pt_'+key]
            denom_title = 'MB'
        
        if 'trig' in scenario:
            mnum = mgr[spin]['96233'].tracks_minus['pt_'+key]
            pnum = mgr[spin]['96233'].tracks_plus['pt_'+key]
            num_title = 'JP2'
        else:
            mnum = mgr[spin]['96011'].tracks_minus['pt_'+key]
            pnum = mgr[spin]['96011'].tracks_plus['pt_'+key]
            num_title = 'MB'
        
        opt = i>0 and 'e2 same' or 'e2'
        
        for h in (mdenom, pdenom, mnum, pnum):
            h.GetYaxis().SetRangeUser(-0.1, 0.1)
            h.SetYTitle('')
            h.GetXaxis().SetTitle('p_{T}')
            h.SetLineColor(color[key])
            h.SetMarkerColor(color[key])
        
        [h.SetMarkerStyle(20) for h in (mdenom, pdenom)]
        [h.SetMarkerStyle(24) for h in (mnum, pnum)]
        
        leg = ROOT.TLegend(0.15, 0.15, 0.4, 0.35)
        leg.AddEntry(mdenom, denom_title)
        leg.AddEntry(mnum, num_title)
        
        diff_m = mdenom.Clone()
        diff_m.Add(mnum, -1)
        
        diff_p = pdenom.Clone()
        diff_p.Add(pnum, -1)
        
        for h in (diff_m, diff_p):
            h.GetYaxis().SetRangeUser(-0.06, 0.04)
            h.GetYaxis().SetTitle('')
            h.SetMarkerStyle(20)
        
        mtitle = '#pi- A_{LL}(%s), A_{LL}(%s) for %s' % (denom_title, 
            num_title, key)
        ptitle = '#pi+ A_{LL}(%s), A_{LL}(%s) for %s' % (denom_title, 
            num_title, key)
        if spin != 'anyspin':
            mtitle += ' -- %(spin)s processes only' % locals()
            ptitle += ' -- %(spin)s processes only' % locals()
        
        mdenom.SetTitle(mtitle)
        diff_m.SetTitle(mtitle)
        pdenom.SetTitle(ptitle)
        diff_p.SetTitle(ptitle)
        
        ## print the individual scenario
        cdiff = graphics.canvas2(key)
        cdiff.cd(1)
        mdenom.Draw('e1')
        mnum.Draw('e1 same')
        line.Draw()
        leg.Draw()
        
        cdiff.cd(2)
        pdenom.Draw('e1')
        pnum.Draw('e1 same')
        line.Draw()
        
        cdiff.Print('mcasym_%(key)s%(stitle)s.png' % locals())
        
        ## now add it to the canvas showing all differences
        opt = i>0 and 'hist c same' or 'hist c'
        
        alldiffs.cd(1)
        line.Draw()
        diff_m.SetTitle('#pi- A_{LL}(%s) - A_{LL}(%s)' % (denom_title, 
            num_title))
        diff_m.Draw(opt)
        
        alldiffs.cd(2)
        line.Draw()
        diff_p.SetTitle('#pi+ A_{LL}(%s) - A_{LL}(%s)' % (denom_title, 
            num_title))
        diff_p.Draw(opt)
        
        diffs[key] = (diff_m, diff_p)
        
        ## build graphs from the histogram uncertainties
        nbins = diff_m.GetNbinsX()
        x = [diff_m.GetBinCenter(bin) for bin in range(1, nbins+1)]
        sigma_m = [mnum.GetBinError(bin) for bin in range(1, nbins+1)]
        sigma_p = [pnum.GetBinError(bin) for bin in range(1, nbins+1)]        
        gsigma_m = ROOT.TGraph(5, array('d', x), array('d', sigma_m))
        gsigma_p = ROOT.TGraph(5, array('d', x), array('d', sigma_p))
        
        for g in (gsigma_m, gsigma_p):
            g.GetXaxis().SetRangeUser(2.00, 12.84)
            g.GetXaxis().SetTitle('p_{T}')
            g.GetYaxis().SetRangeUser(-0.001, 0.03)
            g.GetYaxis().SetTitle('')
        gsigma_m.SetTitle('#pi- %s Asymmetry Uncertainties' % (num_title,))
        gsigma_p.SetTitle('#pi+ %s Asymmetry Uncertainties' % (num_title,))
        
        ## add scenario uncertainty to the combo canvas
        opt = i>0 and 'c same' or 'a c'
        
        allsigmas.cd(1)
        gsigma_m.SetLineColor(color[key])
        gsigma_m.Draw(opt) 
        
        allsigmas.cd(2)
        gsigma_p.SetLineColor(color[key])
        gsigma_p.Draw(opt) 
        
        sigmas[key] = (sigma_m, sigma_p)
        
        keepme.extend([cdiff, gsigma_m, gsigma_p])
    
    raw_input('wait here:')
    
    alldiffs.Update()
    alldiffs.Print('mcasym_run5_diff%(stitle)s.png' % locals())
    allsigmas.Update()
    allsigmas.Print('mcasym_run5_sigma%(stitle)s.png' % locals())
    
    [d.pop('MAX') for d in (diffs, sigmas)]
    [d.pop('MIN') for d in (diffs, sigmas)]
    pkeys = filter(lambda key: key.startswith('P'), keys)
    [diffs.pop(pkey) for pkey in pkeys]
    [sigmas.pop(pkey) for pkey in pkeys]
    
    h = diffs['STD'][0]
    
    def calculate_systematic(h, diffs, sigmas):
        for bin in range(1, h.GetNbinsX()+1):
            values = [ d.GetBinContent(bin) for d in diffs ]
            stats = [ s[bin-1] for s in sigmas ]
            low_edge = h.GetBinLowEdge(bin)
            high_edge = h.GetBinLowEdge(bin+1)
            stat = max(stats)
            lower = min(values)
            upper = max(values)
            if upper > stat:
                systh = upper
                reasonh = 'value'
            else:
                systh = stat
                reasonh = 'stats'
            
            if abs(lower) > stat:
                systl = lower
                reasonl = 'value'
            else:
                systl = stat
                reasonl = 'stats'
            print '[%.2f - %.2f]\t-%.4f (%s)\t+%.4f (%s)' % (low_edge, high_edge,
                abs(systl), reasonl, systh, reasonh)
    
    print 'Calculating Systematic Uncertainty for π-'
    calculate_systematic(h, [d[0] for d in diffs.values()], 
        [s[0] for s in sigmas.values()])
    print 'Calculating Systematic Uncertainty for π+'
    calculate_systematic(h, [d[1] for d in diffs.values()], 
        [s[1] for s in sigmas.values()])
    

def nsigma_fits(outname = 'nsigma_fits.root'):
    # histDir = '/Users/kocolosk/work/2009-03-06-bg-subtract'
    # histDir = '/Users/kocolosk/work/2009-03-06-bg-subtract/old-6'
    histDir = '/Users/kocolosk/work/2009-03-13-pid-3d'
    
    f = ROOT.TFile(outname, 'recreate')
    
    h3 = hadd_interactive(histDir, runlist, 'jetpatch', 'anyspin', 'sum',
        'pt_p_nSigmaPion')
    
    f.cd()
    h3.Write()
    
    c = []
    keepme = []
    
    ROOT.gStyle.SetStatW(0.1)
    
    ps = ROOT.TPostScript('nsigma_fits.ps')
    c_ps = graphics.ps_canvas(x=2, y=3)
    counter = 0
    pad = 1
    
    specials = {
        (2,10) :    11,
        (3,15) :    16,
        (4, 9) :    10,
        (4,20) :    25,
        (5,14) :    15,
        (5,25) :    26,
        (5,27) :    36
    }
    xaxis = h3.GetXaxis()
    yaxis = h3.GetYaxis()
    for xbin in range(1, h3.GetNbinsX()+1):
        lastbin = 0
        while lastbin < h3.GetNbinsY():
            ybin = lastbin+1
            nexty = specials.get( (xbin,ybin), ybin )
            proj = h3.ProjectionZ(str(uuid()), xbin, xbin, ybin, nexty)
            lastbin = nexty
            if proj.GetEntries() < 1.0:
                continue
            proj.Rebin()
            
            xmin = xaxis.GetBinLowEdge(xbin)
            xmax = xaxis.GetBinLowEdge(xbin+1)
            ymin = max(xaxis.GetBinLowEdge(xbin), yaxis.GetBinLowEdge(ybin))
            ymax = min(1.54*xmax, yaxis.GetBinLowEdge(nexty+1))
            name = 'pt_%d_%d_p_%d_%d_' % (int(100*xmin), int(100*xmax),
                int(100*ymin), int(100*ymax))
            
            if counter % 6 == 0:
                c_ps.Update()
                ps.NewPage()
                c_ps.Clear()
                c_ps.Divide(2,3)
                pad = 1
            counter += 1 
            c_ps.cd(pad)
            
            proj.SetTitle('p_{T} %.2f - %.2f, |p| %.2f - %.2f' % (xmin, xmax,
                ymin, ymax))
            proj.SetXTitle('n#sigma(#pi) + 6*track.charge()')
            proj.SetMarkerStyle(24)
            proj.SetMarkerSize(0.4)
            # c.append(graphics.canvas1())
            proj.Draw()
            
            fitter = analysis.pid.fit(proj, yaxis.GetBinCenter(ybin))
            
            f.cd()
            
            leg_entries = {}
            for key,value in fitter.items():
                if key in ('pi_minus', 'K_minus', 'pbar', 'electron'):
                    fitrange = (-12, 0)
                else:
                    fitrange = (0, 12)
                fit = ROOT.TF1(name + key, value, *fitrange)
                if key.startswith('pi'):
                    fit.SetLineColor(ROOT.kRed)
                    fit.SetLineStyle(2)
                    leg_entries['#pi'] = fit
                elif key.startswith('K'):
                    fit.SetLineColor(ROOT.kBlue)
                    fit.SetLineStyle(4)
                    leg_entries['K'] = fit
                elif key in ('proton', 'pbar'):
                    fit.SetLineColor(8)
                    fit.SetLineStyle(3)
                    leg_entries['p'] = fit
                elif key in ('electron', 'positron'):
                    fit.SetLineColor(ROOT.kGreen)
                    leg_entries['e'] = fit
                fit.SetLineWidth(2)
                fit.Draw('same')
                keepme.append(fit)
                f.cd()
                fit.Write()
        
            leg = ROOT.TLegend(0.4, 0.7, 0.6, 0.88)
            leg.SetMargin(0.5)
            leg.SetNColumns(2)
            leg.AddEntry(leg_entries['#pi'], '#pi')
            leg.AddEntry(leg_entries['K'], 'K')
            leg.AddEntry(leg_entries['p'], 'p')
            leg.AddEntry(leg_entries['e'], 'e')
            leg.Draw()
        
            # c[-1].Print('.png')
            pad += 1
            keepme.append( fitter )
            keepme.append( leg )
    
    ps.Close()

def background_fractions(pt_min=3.18, pid = (-1.0,2.0), charge = 'plus',
    pk_max = -3.0, e_min = 2.63):
    """
    calculates efficiencies and purities for a given pT bin by summing the PID
    integrals from each momentum slice
    """
    f = ROOT.TFile(pidFile)
    keys = filter(lambda k: k.GetName().startswith('pt_%d' % int(100*pt_min)),\
        f.GetListOfKeys())
    fits = [ key.ReadObj() for key in keys ]
    
    if charge == 'plus':
        pi_fits = filter(lambda fit: 'pi_plus' in fit.GetName(), fits)
        K_fits  = filter(lambda fit: 'K_plus' in fit.GetName(), fits)
        p_fits  = filter(lambda fit: 'proton' in fit.GetName(), fits)
        e_fits  = filter(lambda fit: 'positron' in fit.GetName(), fits)
        pi_cut = [cut + 6 for cut in pid]
        pk_cut = (-6.00 + 6, pk_max + 6)
        e_cut  = ( e_min + 6,  6.00 + 6)
    elif charge == 'minus':
        pi_fits = filter(lambda fit: 'pi_minus' in fit.GetName(), fits)
        K_fits  = filter(lambda fit: 'K_minus' in fit.GetName(), fits)
        p_fits  = filter(lambda fit: 'pbar' in fit.GetName(), fits)
        e_fits  = filter(lambda fit: 'electron' in fit.GetName(), fits)
        pi_cut = [cut - 6 for cut in pid]
        pk_cut = (-6.00 - 6, pk_max - 6)
        e_cut  = ( e_min - 6,  6.00 - 6)
    
    pi    = sum(fit.Integral(*pi_cut) for fit in pi_fits)
    K_bg  = sum(fit.Integral(*pi_cut) for fit in K_fits)
    p_bg  = sum(fit.Integral(*pi_cut) for fit in p_fits)
    e_bg  = sum(fit.Integral(*pi_cut) for fit in e_fits)

    total_pi = sum(fit.Integral(-12, 12) for fit in pi_fits)
    total_K  = sum(fit.Integral(-12, 12) for fit in K_fits)
    total_p  = sum(fit.Integral(-12, 12) for fit in p_fits)
    total_e  = sum(fit.Integral(-12, 12) for fit in e_fits)

    K = sum(fit.Integral(*pk_cut) for fit in K_fits)
    p = sum(fit.Integral(*pk_cut) for fit in p_fits)
    e = sum(fit.Integral(*e_cut) for fit in e_fits)

    pi_bg_low = sum(fit.Integral(*pk_cut) for fit in pi_fits)
    pi_bg_high = sum(fit.Integral(*e_cut) for fit in pi_fits)
    
    from math import sqrt
    # old_pi = sum(fit.Integral(-1.0+6, 2.0+6) for fit in pi_fits)
    # old_K  = sum(fit.Integral(-1.0+6, 2.0+6) for fit in K_fits)
    # old_p  = sum(fit.Integral(-1.0+6, 2.0+6) for fit in p_fits)
    # old_e  = sum(fit.Integral(-1.0+6, 2.0+6) for fit in e_fits)
    # old_stat = 1.0 / sqrt(old_pi + old_K + old_p + old_e)
    
    [fit.Delete() for fit in fits]
    f.Close()
    
    total_yield = pi + K_bg + p_bg + e_bg
    
    d = {
        'pi': {
            'efficiency': pi/total_pi,
            'purity': pi/total_yield,
            'pK': (p_bg+K_bg)/total_yield,
            'e': e_bg/total_yield,
            'yield': total_yield
        },
        'pK': {
            'efficiency': (p+K)/(total_K+total_p),
            'purity': (p+K)/(p+K+pi_bg_low),
            'yield': p+K+pi_bg_low
        },
        'e': {
            'efficiency': e/(total_e),
            'purity': e/(e+pi_bg_high),
            'yield': e+pi_bg_high
        }
    }
    
    ## now calculate the uncertainty on A_{LL}
    from math import sqrt
    pure_bg = sqrt( 
        1.0/d['pi']['yield'] + 
        (d['pi']['pK'])**2 / d['pK']['yield'] +
        (d['pi']['e'])**2 / d['e']['yield']
    ) / d['pi']['purity']
    
    ## new calculation that acknowledges signal in sidebands
    g_pK = d['pi']['pK']/d['pK']['purity']
    g_e  = d['pi']['e']/d['e']['purity']
    stat = sqrt(
        1.0/d['pi']['yield'] +
        g_pK**2 / d['pK']['yield'] + 
        g_e**2 / d['e']['yield']
    ) / (1.0 - g_pK - g_e)
    d['g_pK'] = g_pK
    d['g_e'] = g_e
    d['pure_bg'] = pure_bg
    d['sigma'] = stat
    # d['old_sigma']  = old_stat
    return d

global_pt_min = 3.18
def background_fractions_minuit(pi_min, pi_max, pk_max, e_min):
    d = background_fractions(global_pt_min, (pi_min, pi_max), pk_max=pk_max, e_min=e_min)
    stat = d['sigma']
    
    ## "encouragement" for the minimizer to not overlap sidebands with signal
    if pk_max > pi_min: 
        stat += 1.0 + pk_max - pi_min
    if e_min < pi_max: 
        stat += 1.0 + pi_max - e_min
    
    ## more encouragement -- keep purity of sidebands above 90%
    # if d['e']['purity'] < 0.9:
    #     stat += 100*(0.9-d['e']['purity'])
    # if d['pK']['purity'] < 0.9:
    #     stat += 100*(0.9-d['pK']['purity'])

    return stat


def roc(pt_min=3.18, charge='plus'):
    from sys import stdout
    
    pt_max = {
        2.00: 3.18,
        3.18: 4.56,
        4.56: 6.32,
        6.32: 8.80,
        8.80: 12.84
    }
    
    max_window = {
        3.18: (-1.90, 2.40),
        4.56: (-1.90, 2.25),
        6.32: (-1.90, 2.00),
        8.80: (-1.90, 1.50)
    }
    
    hroc = ROOT.TH2D('hroc', 'ROC curve for #pi PID (p_{T} %.2f - %.2f)' % (
        pt_min, pt_max[pt_min]), 1000, 0.5, 1., 1000, 0.6 ,1.)
    hroc.SetXTitle('#pi ID efficiency')
    hroc.SetYTitle('purity')
    
    hstat = ROOT.TH1D('hstat', '', 1000, 0.5, 1.0)
    hstat.SetYTitle('stat. (arbitrary scale)')
    
    for min in range(-20,0):
        stdout.write('trying min @ %.2f ' % (float(min)/10,))
        for max in range(10, 30):
            stdout.write('.')
            stdout.flush()
            fmin = float(min)/10
            fmax = float(max)/10
            results = background_fractions(pt_min, (fmin, fmax), charge)
            if results['pi']['efficiency'] > 0.5:
                hroc.Fill(results['pi']['efficiency'], results['pi']['purity'])
                
                ## maybe update stat. uncertainty
                bin = hstat.FindBin(results['pi']['efficiency'])
                content = hstat.GetBinContent(bin)
                if content == 0 or content > results['sigma']:
                    hstat.SetBinContent(bin, results['sigma'])
                
        stdout.write(' done\n')
    
    hnow = ROOT.TH2D('now', '', 1000, 0.5, 1., 1000, 0.6, 1.)
    hnow.SetMarkerStyle(29)
    hnow.SetMarkerColor(ROOT.kRed)
    hnow.SetMarkerSize(2.0)
    results = background_fractions(pt_min, (-1.0,2.0), charge)
    hnow.Fill(results['pi']['efficiency'], results['pi']['purity'])
    
    ## loop over bins and find the point that maximizes area under ROC
    # max_area = 0.0
    
    # max_coords = (-1, -1)
    # xaxis = hroc.GetXaxis()
    # yaxis = hroc.GetYaxis()
    # # for xbin in range(1, hroc.GetNbinsX()+1):
    # #     for ybin in range(1, hroc.GetNbinsY()+1):
    # #         if hroc.GetBinContent(xbin,ybin) > 0.1:
    # #             x = xaxis.GetBinCenter(xbin)
    # #             y = yaxis.GetBinCenter(ybin)
    # #             area = x * y
    # #             if area > max_area:
    # #                 max_area = area
    # #                 max_coords = (x, y)
    # # print 'area under curve maximized @', max_area
    # 
    # ## find bin that minimizes the uncertainty
    # minimum = 1.0
    # minbin = 0
    # for bin in range(1, hstat.GetNbinsX()+1):
    #     content = hstat.GetBinContent(bin)
    #     if 0 < content < minimum:
    #         minimum = content
    #         minbin = bin
    # print 'stat. uncertainty minimized @', hstat.GetBinCenter(minbin)
    # 
    # ## find best purity that goes along with this uncertainty
    # for ybin in range(1, hroc.GetNbinsY()+1):
    #     if hroc.GetBinContent(minbin,ybin) > 0.1:
    #         x = xaxis.GetBinCenter(minbin)
    #         y = yaxis.GetBinCenter(ybin)
    #         max_coords = (x, y)
    # 
    # print max_coords
    hmax = ROOT.TH2D('max', '', 1000, 0.5, 1., 1000, 0.6, 1.)
    hmax.SetMarkerStyle(29)
    hmax.SetMarkerColor(ROOT.kGreen)
    hmax.SetMarkerSize(2.0)
    results = background_fractions(pt_min, max_window[pt_min], charge)
    hmax.Fill(results['pi']['efficiency'], results['pi']['purity'])
    
    c = graphics.canvas1()
    # c.cd(1)
    hroc.Draw()
    hnow.Draw('same')
    hmax.Draw('same')
    
    leg = ROOT.TLegend(0.15, 0.2, 0.5, 0.35)
    leg.AddEntry(hnow, 'Previous Analysis Cut', 'p')
    leg.AddEntry(hmax, 'Minimizes Stat. Uncertainty', 'p')
    leg.Draw()
    
    ## now plot the statistical uncertainty versus efficiency
    # c.cd(2)
    # hstat.GetXaxis().SetLabelSize(0.07)
    # hstat.GetYaxis().SetLabelSize(0.07)
    # hstat.GetYaxis().SetTitleSize(0.07)
    # hstat.GetYaxis().SetTitleOffset(0.7)
    # hstat.Draw()
    
    graphics.maybe_save(c)

def asymmetry(pi, pK, e, g_pK, g_e):
    """
    calculates background-subtracted asymmetry given
    pi   = (content,error)
    pK   = (content,error)
    e    = (content,error)
    g_pK = (pK contamination in pi window) / purity of pK sideband
    g_e  = (e  contamination in pi window) / purity of e sideband
    
    returns (content,error)
    """
    from math import sqrt
    content = (pi[0] - g_pK*pK[0] - g_e*e[0]) / (1 - g_pK - g_e)
    error = sqrt(pi[1]**2 + (g_pK*pK[1])**2 + (g_e*e[1])**2) / (1 - g_pK - g_e)
    return (content,error)


def get_g(ptbin=3.18, charge = 'plus'):
    ptbin = round(ptbin, 2)
    pi_window = {
        2.00: (-1.10, 2.30),
        3.18: (-1.40, 2.10),
        4.56: (-1.40, 1.80),
        6.32: (-1.40, 1.80),
        8.80: (-1.30, 1.40)
    }
    e_window = {
        2.00: 2.60,
        3.18: 2.40,
        4.56: 2.40,
        6.32: 2.40,
        8.80: 2.10
    }
    d = background_fractions(ptbin, pi_window[ptbin], charge, pk_max=-2.10, \
        e_min=e_window[ptbin])
    return (d['g_pK'],d['g_e'])
    
def result():
    histDir = '/Users/kocolosk/data/run5/hist-asym-with-bg'
    
    db = sqlite.connect('/Users/kocolosk/data/analysis.db')
    dbc = db.cursor()
    
    asym_p = AsymmetryGenerator('asym_p', bins=ptbins, key='pt')
    asym_m = AsymmetryGenerator('asym_m', bins=ptbins, key='pt')
    
    asym_multi_p = AsymmetryGenerator('asym_multi_p', bins=ptbins, key='pt')
    asym_multi_m = AsymmetryGenerator('asym_multi_m', bins=ptbins, key='pt')
    asym_multi_m.multi_stats = True
    asym_multi_p.multi_stats = True
    
    asym_pK_p = AsymmetryGenerator('asym_pK_p', bins=ptbins, key='pt_pk')
    asym_pK_m = AsymmetryGenerator('asym_pK_m', bins=ptbins, key='pt_pk')
    asym_pK_m.multi_stats = True
    asym_pK_p.multi_stats = True

    asym_e_p = AsymmetryGenerator('asym_e_p', bins=ptbins, key='pt_e')
    asym_e_m = AsymmetryGenerator('asym_e_m', bins=ptbins, key='pt_e')
    asym_e_p.multi_stats = True
    asym_e_m.multi_stats = True
    
    polarizations = Polarizations.Final
    
    all_files = glob(histDir + '/chargedPions_*.hist.root')
    for fname in all_files[:]:
        run = getRun(fname)
        if run in runlist:
            print fname, run
            mgr = HistogramManager(ROOT.TFile(fname))
            
            c = dbc.execute('''SELECT uu,ud,du,dd 
                FROM scalers 
                WHERE run=? AND board=5 AND timebin>6 AND timebin<10 
                ORDER BY timebin''', (run,))
            scalers = c.fetchall()
            if len(scalers) == 0:
                c = dbc.execute('''SELECT uu,ud,du,dd 
                    FROM scalers 
                    WHERE run=? AND board=6 AND timebin>6 AND timebin<10 
                    ORDER BY timebin''', (run,))
                scalers = c.fetchall()
            uu = scalers[0][0] + scalers[1][0] + scalers[2][0]
            ud = scalers[0][1] + scalers[1][1] + scalers[2][1]
            du = scalers[0][2] + scalers[1][2] + scalers[2][2]
            dd = scalers[0][3] + scalers[1][3] + scalers[2][3]
            
            # try:
            #     pol = polarizations[analysis.util.fill(run)]
            # except KeyError:
            pol = Polarizations.Online[analysis.util.fill(run)]
            
            asym_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
            
            asym_multi_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd, \
                pol.py,pol.pb)
            asym_multi_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd, \
                pol.py,pol.pb)
            
            asym_pK_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd,\
                pol.py,pol.pb)
            asym_pK_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd,\
                pol.py,pol.pb)
            
            asym_e_p.FillFromHistogramManager(mgr, 'jetpatch', 1, uu,ud,du,dd,\
                pol.py,pol.pb)
            asym_e_m.FillFromHistogramManager(mgr, 'jetpatch', -1, uu,ud,du,dd,\
                pol.py,pol.pb)
    
    ROOT.gStyle.SetErrorX(0)
            
    c1 = graphics.canvas2()
    hm = asym_m.GetAsymmetry('ll')
    hp = asym_p.GetAsymmetry('ll')
    hm_multi = asym_multi_m.GetAsymmetry('ll')
    hp_multi = asym_multi_p.GetAsymmetry('ll')
    hm.GetYaxis().SetRangeUser(-0.2, 0.2)
    hp.GetYaxis().SetRangeUser(-0.2, 0.2)
    [h.SetMarkerStyle(20) for h in (hm,hp,hm_multi,hp_multi)]
    c1.cd(1)
    hm.Draw('e1')
    hm.SetLineColor(ROOT.kRed)
    hm_multi.Draw('e1 same')
    c1.cd(2)
    hp.Draw('e1')
    hp.SetLineColor(ROOT.kRed)
    hp_multi.Draw('e1same')
    
    graphics.maybe_save()
    
    hm_pK = asym_pK_m.GetAsymmetry('ll')
    hp_pK = asym_pK_p.GetAsymmetry('ll')
    
    hm_e = asym_e_m.GetAsymmetry('ll')
    hp_e = asym_e_p.GetAsymmetry('ll')
    
    c4 = graphics.canvas2()
    c4.cd(1)
    final_m = hm_multi.Clone()
    final_m.Reset('ice')
    for bin in range(1, hm_multi.GetNbinsX()+1):
        pt = hm_multi.GetBinLowEdge(bin)
        g_pK, g_e = get_g(pt, 'minus')
        pi = (hm_multi.GetBinContent(bin), hm_multi.GetBinError(bin))
        pK = (hm_pK.GetBinContent(bin), hm_pK.GetBinError(bin))
        e  = (hm_e.GetBinContent(bin), hm_e.GetBinError(bin))
        content,error = asymmetry(pi, pK, e, g_pK, g_e)
        final_m.SetBinContent(bin, content)
        final_m.SetBinError(bin, error)
    final_m.GetYaxis().SetRangeUser(-0.2, 0.2)
    final_m.Draw('e1')
    
    c4.cd(2)
    final_p = hp_multi.Clone()
    final_p.Reset('ice')
    for bin in range(1, hp_multi.GetNbinsX()+1):
        pt = hp_multi.GetBinLowEdge(bin)
        g_pK, g_e = get_g(pt, 'plus')
        pi = (hp_multi.GetBinContent(bin), hp_multi.GetBinError(bin))
        pK = (hp_pK.GetBinContent(bin), hp_pK.GetBinError(bin))
        e  = (hp_e.GetBinContent(bin), hp_e.GetBinError(bin))
        content,error = asymmetry(pi, pK, e, g_pK, g_e)
        final_p.SetBinContent(bin, content)
        final_p.SetBinError(bin, error)
    final_p.GetYaxis().SetRangeUser(-0.2, 0.2)
    final_p.Draw('e1')
    graphics.maybe_save()
    
    c5 = graphics.canvas2()
    graph_m_multi = ROOT.TGraphErrors(hm_multi)
    [ graph_m_multi.ShiftPoint(i, dx=0.4) for i in range(final_m.GetNbinsX()) ]
    graph_p_multi = ROOT.TGraphErrors(hp_multi)
    [ graph_p_multi.ShiftPoint(i, dx=0.4) for i in range(final_m.GetNbinsX()) ]
    
    from array import array
    prelim_x  = array('d', [3.0, 5.0, 7.0, 9.0])
    prelim_m  = array('d', [-0.0048, -0.0247, -0.0551,  0.0140])
    prelim_em = array('d', [ 0.0056,  0.0142,  0.0278,  0.0512])
    prelim_p  = array('d', [-0.0125,  0.0297,  0.0155, -0.0371])
    prelim_ep = array('d', [ 0.0054,  0.0135,  0.0263,  0.0477])
    zero = array('d', [0.0 for i in range(4)])
    
    gprelim_m = ROOT.TGraphErrors(4, prelim_x, prelim_m, zero, prelim_em)
    gprelim_p = ROOT.TGraphErrors(4, prelim_x, prelim_p, zero, prelim_ep)
    for g in (gprelim_m, gprelim_p):
        g.SetMarkerStyle(25)
        g.SetMarkerColor(ROOT.kRed)
        g.SetLineColor(ROOT.kRed)
    
    for h in (final_m, final_p):
        h.SetXTitle('p_{T}')
    
    [ g.SetMarkerStyle(24) for g in (graph_m_multi, graph_p_multi) ]
    
    line = ROOT.TLine(hm.GetBinLowEdge(1), 0.0, hm.GetBinLowEdge(hm.GetNbinsX()+1), 0.0)
    line.SetLineStyle(2)
    
    leg = ROOT.TLegend(0.15, 0.65, 0.5, 0.85)
    leg.AddEntry(final_m, 'Background Subtracted', 'p')
    leg.AddEntry(graph_m_multi, 'Raw A_{LL} (shifted +0.4)', 'p')
    # leg.AddEntry(gprelim_m, 'Preliminary Result', 'p')
    
    c5.cd(1)
    final_m.GetYaxis().SetRangeUser(-0.09, 0.09)
    final_m.Draw('e1')
    graph_m_multi.Draw('p')
    # gprelim_m.Draw('p')
    line.Draw('same')
    leg.Draw()
    
    c5.cd(2)
    final_p.GetYaxis().SetRangeUser(-0.09, 0.09)
    final_p.Draw('e1')
    graph_p_multi.Draw('p')
    # gprelim_p.Draw('p')
    line.Draw('same')
    
    graphics.maybe_save()
    
    ## save the histograms in an extra .root file for later
    f = ROOT.TFile('final_result.root', 'recreate')
    final_m.Write('final_minus')
    final_p.Write('final_plus')
    f.Close()


def bichsel_predictions(method='GetI70', resolution=0.08):
    """
    plots the Bichsel dE/dx predictions for pi/K/p/e
    """
    g_pi = analysis.pid.bichsel('pi', method=method, resolution=resolution)
    g_K  = analysis.pid.bichsel('K', method=method, resolution=resolution)
    g_p  = analysis.pid.bichsel('p', method=method, resolution=resolution)
    g_e  = analysis.pid.bichsel('e', method=method, resolution=resolution)
    
    g_pi.SetTitle('Bichsel I70 Predictions')
    g_pi.GetYaxis().SetTitle('dE/dx [keV/cm]')
    g_pi.GetYaxis().SetRangeUser(2.0, 5.0)
    g_pi.GetXaxis().SetTitle('p [GeV/c]')
    g_pi.GetXaxis().SetRangeUser(0.5, 40.)
    g_pi.SetFillColor(ROOT.kRed)
    g_pi.SetFillStyle(3001)
    
    g_K.SetFillColor(ROOT.kGreen)
    g_K.SetFillStyle(3002)
    
    g_p.SetFillColor(ROOT.kMagenta)
    g_p.SetFillStyle(3003)
    
    g_e.SetFillColor(ROOT.kBlue)
    g_e.SetFillStyle(3004)
    
    c = canvas1()
    g_pi.Draw('a 3')
    g_K.Draw('3')
    g_p.Draw('3')
    g_e.Draw('3')
    c.SetLogx()
    # c.SetLogy()
    
    leg = ROOT.TLegend(0.6, 0.65, 0.88, 0.88)
    leg.AddEntry(g_pi, '#pi #pm #sigma (%.1f%%)' % (100*resolution,), 'f')
    leg.AddEntry(g_K, 'K #pm #sigma (%.1f%%)' % (100*resolution,), 'f')
    leg.AddEntry(g_p, 'p #pm #sigma (%.1f%%)' % (100*resolution,), 'f')
    leg.AddEntry(g_e, 'e #pm #sigma (%.1f%%)' % (100*resolution,), 'f')
    leg.Draw()
    
    me = analysis.pid.bichsel_acceptance(method=method, resolution=resolution)
    me.SetFillStyle(3014)
    me.Draw('3')
    maybe_save()

def reconstruction_bias(lastkey='eta'):
    # recoBiasFile = '/Users/kocolosk/work/2009-02-04-hardP/hardP_etaMc.root'
    recoBiasFile = '/Users/kocolosk/work/2009-03-23-new-hardP/hardP_new.root'
    mgr = HistogramManager(ROOT.TFile(recoBiasFile))
    
    keys = ['raw', 'vertex', 'association', 'resolution', lastkey]
    
    mcoll = mgr.anyspin['96011'].tracks_minus
    pcoll = mgr.anyspin['96011'].tracks_plus
    
    canvases = []
    
    for i,key in enumerate(keys[1:]):
        nbins = mcoll[key].GetNbinsX()
        
        mnum = [ mcoll[key].ProjectionY('%s_m_%d' % (key,bin), 
            bin, bin) for bin in range(1,nbins+1) ]
        pnum = [ pcoll[key].ProjectionY('%s_p_%d' % (key,bin), 
            bin, bin) for bin in range(1,nbins+1) ]
        
        mdenom = [ mcoll[keys[i]].ProjectionY('%s_m_%d' % (keys[i],bin), 
            bin, bin) for bin in range(1,nbins+1) ]
        pdenom = [ pcoll[keys[i]].ProjectionY('%s_p_%d' % (keys[i],bin),
            bin, bin) for bin in range(1,nbins+1) ]
        
        h2 = mcoll[key]
        
        [ h.SetStats(True) for h in mnum ]
        [ h.SetStats(True) for h in pnum ]
        ROOT.gStyle.SetOptFit(111)
        
        for j in range(len(mnum)):
            mnum[j].Divide(mnum[j], mdenom[j], 1, 1, 'B')
            pnum[j].Divide(pnum[j], pdenom[j], 1, 1, 'B')
            
            low = h2.GetBinLowEdge(j+1)
            high = h2.GetBinLowEdge(j+2)
            title = '%s [%.2f - %.2f]' % (key, low, high)
            
            hm = mnum[j]
            hm.SetTitle('#pi- ' + title)
            
            hp = pnum[j]
            hp.SetTitle('#pi+ ' + title)
            
            for h in (hm, hp):
                h.SetMarkerStyle(25)
                h.SetMarkerSize(0.6)
                h.GetYaxis().SetRangeUser(0.8, 1.1)
            
            c = graphics.canvas2()
            c.cd(1)
            hm.Fit('pol1', '', 'e1')
            c.cd(2)
            hp.Fit('pol1', '', 'e1')
            canvases.append(c)
            
            raw_input(title)
            c.Print('%s_%d_%d.png' % (key, int(100*low), int(100*high)))
        

def mcasym_true(spin = 'anyspin'):
    """
    comparison of MC asymmetries for MC and reco tracks
    """
    trueFile = '/Users/kocolosk/data/run5-simu/mcasym-pythia.root'
    stitle = ''
    if spin != 'anyspin': stitle = '_%(spin)s' % locals()
    
    keys = ['STD','MIN','ZERO','MAX', 'DSSV', 'M015', 'M030', 'M045', 'M060',
        'M075', 'M090', 'M105', 'P030', 'P045', 'P060', 'P070']
    reco_mgr = HistogramManager(ROOT.TFile(mcasymFile))
    true_mgr = HistogramManager(ROOT.TFile(trueFile))
    
    line = ROOT.TLine(2.0, 0.0, 12.84, 0.0)
    line.SetLineStyle(2)
    ROOT.gStyle.SetErrorX()
    
    color = {
        'STD': ROOT.kBlack,
        'MAX': ROOT.kRed,
        'MIN': ROOT.kGreen,
        'ZERO': ROOT.kBlue,
        'DSSV': 8,
        'M015': ROOT.kMagenta,
        'M030': ROOT.kMagenta,
        'M045': ROOT.kMagenta,
        'M060': ROOT.kMagenta,
        'M075': ROOT.kMagenta,
        'M090': ROOT.kMagenta,
        'M105': ROOT.kMagenta,
        'P030': 7,
        'P045': 7,
        'P060': 7,
        'P070': 7,
        'GS_NLOC': ROOT.kMagenta
    }
    
    alldiffs = graphics.canvas2('Asymmetry Differences')
    allsigmas = graphics.canvas2('Asymmetry Uncertainties')
    
    keepme = []
    diffs = {}
    sigmas = {}
    for i,key in enumerate(keys):
        mb_m = reco_mgr[spin]['96011'].tracks_minus['pt_'+key]
        mb_p = reco_mgr[spin]['96011'].tracks_plus['pt_'+key]
        mc_m = true_mgr[spin]['96011'].tracks_minus['pt_true_'+key]
        mc_p = true_mgr[spin]['96011'].tracks_plus['pt_true_'+key]
        
        opt = i>0 and 'e2 same' or 'e2'
        
        for h in (mb_m, mb_p, mc_m, mc_p):
            h.GetYaxis().SetRangeUser(-0.1, 0.1)
            h.SetYTitle('')
            h.GetXaxis().SetTitle('p_{T}')
            h.SetLineColor(color[key])
            h.SetMarkerColor(color[key])
        
        [h.SetMarkerStyle(20) for h in (mb_m, mb_p)]
        [h.SetMarkerStyle(24) for h in (mc_m, mc_p)]
        
        leg = ROOT.TLegend(0.15, 0.15, 0.4, 0.35)
        leg.AddEntry(mb_m, 'reco')
        leg.AddEntry(mc_m, 'true')
        
        diff_m = mb_m.Clone()
        diff_m.Add(mc_m, -1)
        
        diff_p = mb_p.Clone()
        diff_p.Add(mc_p, -1)
        
        for h in (diff_m, diff_p):
            h.GetYaxis().SetRangeUser(-0.02, 0.02)
            h.GetYaxis().SetTitle('')
            h.SetMarkerStyle(20)
        
        mtitle = '#pi- A_{LL}(reco), A_{LL}(true) for %(key)s' % \
            locals()
        ptitle = '#pi+ A_{LL}(reco), A_{LL}(true) for %(key)s' % \
            locals()            
        if spin != 'anyspin':
            mtitle += ' -- %(spin)s processes only' % locals()
            ptitle += ' -- %(spin)s processes only' % locals()
        
        mb_m.SetTitle(mtitle)
        diff_m.SetTitle(mtitle)
        mb_p.SetTitle(ptitle)
        diff_p.SetTitle(ptitle)
        
        ## print the individual scenario
        cdiff = graphics.canvas2(key)
        cdiff.cd(1)
        mb_m.Draw('e1')
        mc_m.Draw('e1 same')
        line.Draw()
        leg.Draw()
        
        cdiff.cd(2)
        mb_p.Draw('e1')
        mc_p.Draw('e1 same')
        line.Draw()
        
        cdiff.Print('mcasym_%(key)s%(stitle)s.png' % locals())
        
        ## now add it to the canvas showing all differences
        opt = i>0 and 'hist c same' or 'hist c'
        
        alldiffs.cd(1)
        line.Draw()
        diff_m.SetTitle('#pi- A_{LL}(reco) - A_{LL}(true)')
        diff_m.Draw(opt)
        
        alldiffs.cd(2)
        line.Draw()
        diff_p.SetTitle('#pi+ A_{LL}(reco) - A_{LL}(true)')
        diff_p.Draw(opt)
        
        diffs[key] = (diff_m, diff_p)
        
        ## build graphs from the histogram uncertainties
        nbins = diff_m.GetNbinsX()
        x = [diff_m.GetBinCenter(bin) for bin in range(1, nbins+1)]
        sigma_m = [mc_m.GetBinError(bin) for bin in range(1, nbins+1)]
        sigma_p = [mc_p.GetBinError(bin) for bin in range(1, nbins+1)]        
        gsigma_m = ROOT.TGraph(5, array('d', x), array('d', sigma_m))
        gsigma_p = ROOT.TGraph(5, array('d', x), array('d', sigma_p))
        
        for g in (gsigma_m, gsigma_p):
            g.GetXaxis().SetRangeUser(2.00, 12.84)
            g.GetXaxis().SetTitle('p_{T}')
            g.GetYaxis().SetRangeUser(-0.001, 0.03)
            g.GetYaxis().SetTitle('')
        gsigma_m.SetTitle('#pi- MC Asymmetry Uncertainties')
        gsigma_p.SetTitle('#pi+ MC Asymmetry Uncertainties')
        
        ## add scenario uncertainty to the combo canvas
        opt = i>0 and 'c same' or 'a c'
        
        allsigmas.cd(1)
        gsigma_m.SetLineColor(color[key])
        gsigma_m.Draw(opt) 
        
        allsigmas.cd(2)
        gsigma_p.SetLineColor(color[key])
        gsigma_p.Draw(opt) 
        
        sigmas[key] = (sigma_m, sigma_p)
        
        keepme.extend([cdiff, gsigma_m, gsigma_p])
    
    raw_input('wait here:')
    
    alldiffs.Update()
    alldiffs.Print('mcasym_run5_diff%(stitle)s.png' % locals())
    allsigmas.Update()
    allsigmas.Print('mcasym_run5_sigma%(stitle)s.png' % locals())
    
    [d.pop('MAX') for d in (diffs, sigmas)]
    [d.pop('MIN') for d in (diffs, sigmas)]
    pkeys = filter(lambda key: key.startswith('P'), keys)
    [diffs.pop(pkey) for pkey in pkeys]
    [sigmas.pop(pkey) for pkey in pkeys]
    
    h = diffs['STD'][0]
    
    def calculate_systematic(h, diffs, sigmas):
        for bin in range(1, h.GetNbinsX()+1):
            values = [ d.GetBinContent(bin) for d in diffs ]
            stats = [ s[bin-1] for s in sigmas ]
            low_edge = h.GetBinLowEdge(bin)
            high_edge = h.GetBinLowEdge(bin+1)
            stat = max(stats)
            lower = min(values)
            upper = max(values)
            if upper > stat:
                systh = upper
                reasonh = 'value'
            else:
                systh = stat
                reasonh = 'stats'
            
            if abs(lower) > stat:
                systl = lower
                reasonl = 'value'
            else:
                systl = stat
                reasonl = 'stats'
            print '[%.2f - %.2f]\t-%.4f (%s)\t+%.4f (%s)' % (low_edge, high_edge,
                abs(systl), reasonl, systh, reasonh)
    
    print 'Calculating Systematic Uncertainty for π-'
    calculate_systematic(h, [d[0] for d in diffs.values()], 
        [s[0] for s in sigmas.values()])
    print 'Calculating Systematic Uncertainty for π+'
    calculate_systematic(h, [d[1] for d in diffs.values()], 
        [s[1] for s in sigmas.values()])

def compare_pid_to_preliminary():
    f = ROOT.TFile(finalResultFile)
    hm = f.Get('final_minus')
    hp = f.Get('final_plus')
    
    prelim_stat_m = [ 0.0056,  0.0142,  0.0278,  0.0512]
    prelim_stat_p = [ 0.0054,  0.0135,  0.0263,  0.0477]
    pid_syst = [ 0.0018, 0.0018, 0.0018, 0.0018 ]
    
    sum_quadrature = lambda *args: sqrt( sum([a**2 for a in args]) )
    
    from array import array
    prelim_x  = array('d', [3.0, 5.0, 7.0, 9.0])
    prelim_m  = array('d', [-0.0048, -0.0247, -0.0551,  0.0140])
    prelim_em = array('d', map(sum_quadrature, prelim_stat_m, pid_syst))
    prelim_p  = array('d', [-0.0125,  0.0297,  0.0155, -0.0371])
    prelim_ep = array('d', map(sum_quadrature, prelim_stat_p, pid_syst))
    zero = array('d', [0.0 for i in range(4)])
    
    print prelim_em
    
    gprelim_m = ROOT.TGraphErrors(4, prelim_x, prelim_m, zero, prelim_em)
    gprelim_p = ROOT.TGraphErrors(4, prelim_x, prelim_p, zero, prelim_ep)
    for g in (gprelim_m, gprelim_p):
        g.SetMarkerStyle(25)
        g.SetMarkerColor(ROOT.kRed)
        g.SetLineColor(ROOT.kRed)
    
    c = graphics.canvas2()
    
    c.cd(1)
    hm.Draw('e1')
    gprelim_m.Draw('p')
    
    c.cd(2)
    hp.Draw('e1')
    gprelim_p.Draw('p')
    
    graphics.maybe_save()

def compare_runlists_polarizations():
    f1 = ROOT.TFile(finalResultFile)
    h1m = f1.Get('final_minus')
    h1p = f1.Get('final_plus')
    
    f2 = ROOT.TFile('/Users/kocolosk/data/run5/final_code_prelim_runlist.root')
    h2m = f2.Get('final_minus')
    h2p = f2.Get('final_plus')
    
    for h in (h2m, h2p):
        h.SetMarkerColor(ROOT.kGreen)
        h.SetLineColor(ROOT.kGreen)
    
    # f3 = ROOT.TFile(path + 'final_code_prelim_runlist_online_pol.root')
    # f3.ls()
    # h3m = f3.Get('final_minus')
    # h3p = f3.Get('final_plus')
    
    # for h in (h3m, h3p):
    #     h.SetMarkerColor(ROOT.kBlue)
    #     h.SetLineColor(ROOT.kBlue)
    
    prelim_stat_m = [ 0.0056,  0.0142,  0.0278,  0.0512]
    prelim_stat_p = [ 0.0054,  0.0135,  0.0263,  0.0477]
    pid_syst = [ 0.0018, 0.0018, 0.0018, 0.0018 ]
    
    sum_quadrature = lambda *args: sqrt( sum([a**2 for a in args]) )
    
    from array import array
    prelim_x  = array('d', [3.0, 5.0, 7.0, 9.0])
    prelim_m  = array('d', [-0.0048, -0.0247, -0.0551,  0.0140])
    prelim_em = array('d', map(sum_quadrature, prelim_stat_m, pid_syst))
    prelim_p  = array('d', [-0.0125,  0.0297,  0.0155, -0.0371])
    prelim_ep = array('d', map(sum_quadrature, prelim_stat_p, pid_syst))
    zero = array('d', [0.0 for i in range(4)])
    
    gprelim_m = ROOT.TGraphErrors(4, prelim_x, prelim_m, zero, prelim_em)
    gprelim_p = ROOT.TGraphErrors(4, prelim_x, prelim_p, zero, prelim_ep)
    for g in (gprelim_m, gprelim_p):
        g.SetMarkerStyle(25)
        g.SetMarkerColor(ROOT.kRed)
        g.SetLineColor(ROOT.kRed)
    
    line = ROOT.TLine(2.0, 0.0, 12.84, 0.0)
    line.SetLineStyle(2)
    
    leg = ROOT.TLegend(0.12, 0.7, 0.57, 0.89)
    leg.AddEntry(h1m, 'final', 'p')
    leg.AddEntry(h2m, 'final code, prelim. runlist', 'p')
    leg.AddEntry(gprelim_m, 'preliminary', 'p')
    
    c = graphics.canvas2()
    
    c.cd(1)
    h1m.Draw('e1')
    h2m.Draw('e1 same')
    # h3m.Draw('e1 same')
    gprelim_m.Draw('p')
    leg.Draw()
    line.Draw()
    
    c.cd(2)
    h1p.Draw('e1')
    h2p.Draw('e1 same')
    # h3p.Draw('e1 same')
    gprelim_p.Draw('p')
    line.Draw()
    
    graphics.maybe_save()

def compare_mcasym_to_theory():
    rawFile = '/Users/kocolosk/data/run5-simu/mcasym-pythia.root'
    weightedFile = '/Users/kocolosk/work/2009-03-28-mcasym-reweight/mcasym_reweighted.root'
    
    keys = ['STD','MIN','ZERO','MAX']
    raw = HistogramManager(ROOT.TFile(rawFile))
    mgr = HistogramManager(ROOT.TFile(weightedFile))
    
    line = ROOT.TLine(2.0, 0.0, 12.84, 0.0)
    line.SetLineStyle(2)
    ROOT.gStyle.SetErrorX(0)
    
    color = {
        'STD': ROOT.kBlack,
        'MAX': ROOT.kRed,
        'MIN': ROOT.kGreen,
        'ZERO': ROOT.kBlue,
    }
    
    ## NLO curves
    from analysis.asym import theoryCurves
    nlo_m = {
        'STD': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_std, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        'ZERO': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_zero, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        'MAX': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_max, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
        'MIN': theoryCurves(analysis.asym.werner_minus_dss_cteqm5_min, 
            analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph()
    }
    nlo_p = {
        'STD': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_std, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'ZERO': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_zero, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'MAX': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_max, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
        'MIN': theoryCurves(analysis.asym.werner_plus_dss_cteqm5_min, 
            analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph()
    }
    
    for nlo in (nlo_m, nlo_p):
        nlo['ZERO'].SetLineStyle(3)
        nlo['ZERO'].SetLineColor(ROOT.kBlue)
        nlo['MAX'].SetLineStyle(4)
        nlo['MAX'].SetLineColor(ROOT.kRed)
        nlo['MIN'].SetLineStyle(2)
        nlo['MIN'].SetLineColor(ROOT.kGreen)
        [gr.SetLineWidth(3) for gr in nlo.values()]
    
    
    rawm = {}
    rawp = {}
    for key in keys:
        rawm[key] = raw['anyspin']['96011'].tracks_minus['pt_true_'+key]
        rawm[key].SetLineColor(color[key])
        rawm[key].SetMarkerColor(color[key])
        rawp[key] = raw['anyspin']['96011'].tracks_plus['pt_true_'+key]
        rawp[key].SetLineColor(color[key])
        rawp[key].SetMarkerColor(color[key])
    
    [ h.SetMarkerStyle(24) for h in rawm.values()+rawp.values() ]
    
    hm = {}
    hp = {}
    for key in keys:
        hm[key] = mgr['anyspin']['96011'].tracks_minus['pt_true_'+key]
        hm[key].SetLineColor(color[key])
        hm[key].SetMarkerColor(color[key])
        hp[key] = mgr['anyspin']['96011'].tracks_plus['pt_true_'+key]
        hp[key].SetLineColor(color[key])
        hp[key].SetMarkerColor(color[key])
    
    [ h.SetMarkerStyle(20) for h in hm.values()+hp.values() ]
    
    
    [ h.GetYaxis().SetTitle() for h in hm['STD'], hp['STD'] ]
    hm['STD'].GetYaxis().SetRangeUser(-0.04, 0.07)
    hp['STD'].GetYaxis().SetRangeUser(-0.08, 0.14)
    hm['STD'].SetTitle('Comparison of NLO and MC Asymmetries for #pi-')
    hp['STD'].SetTitle('Comparison of NLO and MC Asymmetries for #pi+')
    
    leg = ROOT.TLegend(0.15, 0.69, 0.44, 0.88)
    leg.AddEntry(rawp['STD'], 'raw Pythia')
    leg.AddEntry(hp['STD'], 'scale by #frac{DSS}{Kretzer}')
    leg.AddEntry(nlo_p['STD'], 'NLO+DSS', 'l')
    
    ## calculate chi2 between raw/reweighted and NLO
    fitsm = {}
    fitsp = {}
    chisq_m = {}
    chisq_p = {}
    chisq_rawm = {}
    chisq_rawp = {}
    for key in keys:
        fitsm[key] = ROOT.TF1('m'+key, lambda x,p: nlo_m[key].Eval(x[0]), 
            2.00, 4.56, 1)
        fitsp[key] = ROOT.TF1('p'+key, lambda x,p: nlo_p[key].Eval(x[0]), 
            2.00, 4.56, 1)
        hm[key].Fit(fitsm[key], 'RNQ')
        hp[key].Fit(fitsp[key], 'RNQ')
        chisq_m[key] = fitsm[key].GetChisquare()
        chisq_p[key] = fitsp[key].GetChisquare()
        
        rawm[key].Fit(fitsm[key], 'RNQ')
        rawp[key].Fit(fitsp[key], 'RNQ')
        chisq_rawm[key] = fitsm[key].GetChisquare()
        chisq_rawp[key] = fitsp[key].GetChisquare()
    
    print 'Scenario (-) | Chi2 (raw) | Chi2 (scaled)'
    for key in sorted(keys):
        print '%-12s | %10.2f | %13.2f ' % (key, chisq_rawm[key], chisq_m[key])
    
    print '\nScenario (+) | Chi2 (raw) | Chi2 (scaled)'
    for key in sorted(keys):
        print '%-12s | %10.2f | %13.2f ' % (key, chisq_rawp[key], chisq_p[key])
    
    c = graphics.canvas2('Comparison of MC and NLO Asymmetries')
    c.cd(1)
    hm['STD'].Draw()
    [ h.Draw('e1 same') for h in hm.values() ]
    [ h.Draw('e1 same') for h in rawm.values() ]
    [ g.Draw('l') for g in nlo_m.values() ]
    line.Draw()
    
    c.cd(2)
    hp['STD'].Draw()
    [ h.Draw('e1 same') for h in hp.values() ]
    [ h.Draw('e1 same') for h in rawp.values() ]
    [ g.Draw('l') for g in nlo_p.values() ]
    leg.Draw()
    line.Draw()
    
    graphics.maybe_save(c)
