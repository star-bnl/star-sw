import os
import math
from glob import glob

import ROOT
import analysis

run5_tree_dir = '/Users/kocolosk/data/run5/tree'
run5_hist_dir = '/Users/kocolosk/data/run5/hist'

run6_tree_dir = '/Users/kocolosk/data/run6/tree'
run6_hist_dir = '/Users/kocolosk/data/run6/hist'

run5_scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
run6_scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'

def spin2006_asymmetries():
    """asymmetries for charged pion production shown at SPIN 2006"""
    asym_plus = analysis.AsymmetryGenerator('asym_plus')
    asym_minus = analysis.AsymmetryGenerator('asym_minus')
    
    runlist = analysis.asym.golden_runlist_c
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Online
    
    theory = analysis.asym.theoryCurves()
    plusGraphs = [ theory.getGraph('plus',key) for key in ('std','zero','max','min') ]
    minusGraphs = [ theory.getGraph('minus',key) for key in ('std','zero','max','min') ]
    
    ## systematic uncertainties
    baseline = -0.1
    syst_x = [3.0, 5.0, 7.0, 9.0]
    syst = {'plus': [7.3, 8.4, 7.5, 5.1], 'minus': [5.7, 6.0, 5.7, 7.1] }
    systGraph = {'plus': ROOT.TGraph(len(syst_x)+3), 'minus': ROOT.TGraph(len(syst_x)+3) }
    for charge in ('plus','minus'):
        systGraph[charge].SetPoint(0, 3.0, baseline)
        systGraph[charge].SetPoint(5, 9.0, baseline)
        systGraph[charge].SetPoint(6, 3.0, baseline)
        for i,val in enumerate(syst[charge]):
            systGraph[charge].SetPoint(i+1, syst_x[i], baseline + (val/1000.0))
    
    ## generate the asymmetries
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['pt'])
            
            try:
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print fill, 'has no online polarization values'
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    ## fun with graphics
    h1 = asym_plus.GetAsymmetry('ll')
    g1 = ROOT.TGraphErrors(h1)
    h2 = asym_minus.GetAsymmetry('ll')
    g2 = ROOT.TGraphErrors(h2)
    
    ## set numbers to exactly the prelim result
    h1.SetBinContent(1, -0.0125)
    h1.SetBinContent(2,  0.0297)
    h1.SetBinContent(3,  0.0155)
    h1.SetBinContent(4, -0.0371)
    
    h1.SetBinError(1, 0.0054)
    h1.SetBinError(2, 0.0135)
    h1.SetBinError(3, 0.0263)
    h1.SetBinError(4, 0.0477)
    
    h2.SetBinContent(1, -0.0048)
    h2.SetBinContent(2, -0.0247)
    h2.SetBinContent(3, -0.0551)
    h2.SetBinContent(4,  0.0140)
    
    h2.SetBinError(1, 0.0056)
    h2.SetBinError(2, 0.0142)
    h2.SetBinError(3, 0.0278)
    h2.SetBinError(4, 0.0512)
    
    g1 = ROOT.TGraphErrors(h1)
    g2 = ROOT.TGraphErrors(h2)
    
    ## ignore bin width errors
    for gr in (g1,g2):
        for point in range(gr.GetN()):
            gr.SetPointError(point, 0.0, gr.GetErrorY(point))
    
    line = ROOT.TLine(2.0, 0.0, 10.0, 0.0)
    line.SetLineStyle(2)
    
    latex = ROOT.TLatex()
    
    #leg = ROOT.TLegend(0.13, 0.65, 0.35, 0.88)
    #leg.SetFillStyle(0)
    #leg.SetBorderSize(0)
    #leg.AddEntry(plusGraphs[0],' GRSV-STD', 'l')
    #leg.AddEntry(plusGraphs[1],' #Delta G =  0', 'l')
    #leg.AddEntry(plusGraphs[2],' #Delta G =  G', 'l')
    #leg.AddEntry(plusGraphs[3],' #Delta G = -G', 'l')
    
    bg = ROOT.TH1D(h1)
    bg.Reset()
    bg.SetYTitle(' A_{LL}')
    bg.GetYaxis().SetRangeUser(-0.11, 0.11)
    
    ## pi-plus
    c1 = ROOT.TCanvas('c1','A_{LL} for #pi^{+}', 1060, 800)
    bg.SetXTitle('#pi^{+} P_{T} (GeV/c)')
    bg.DrawCopy()
    g1.SetMarkerSize(0.9);
    g1.SetMarkerStyle(21)
    g1.Draw('p')
    [ g.Draw('l') for g in plusGraphs ]
    systGraph['plus'].SetLineColor(1)
    systGraph['plus'].SetFillColor(15)
    systGraph['plus'].Draw('fl')
    line.Draw('same')
    #leg.Draw('p')
    latex.DrawLatex(2.3,0.12," #vec{p} + #vec{p} #rightarrow #pi^{+} + X at #sqrt{s}=200 GeV \
                -1< #eta^{#pi}< 1 ")
    latex.DrawLatex(2.6,-0.07,"2005 STAR Preliminary");
    
    ## pi-minus
    c2 = ROOT.TCanvas('c2','A_{LL} for #pi^{-}', 1060, 800)
    bg.SetXTitle('#pi^{-} P_{T} (GeV/c)')
    bg.DrawCopy()
    g2.SetMarkerSize(0.9);
    g2.SetMarkerStyle(20)
    g2.Draw('p')
    [ g.Draw('l') for g in minusGraphs ]
    systGraph['minus'].SetLineColor(1)
    systGraph['minus'].SetFillColor(15)
    systGraph['minus'].Draw('fl')
    line.Draw('same')
    #leg.Draw('p')
    latex.DrawLatex(2.3,0.12," #vec{p} + #vec{p} #rightarrow #pi^{-} + X at #sqrt{s}=200 GeV \
                -1< #eta^{#pi}< 1 ")
    latex.DrawLatex(2.6,-0.07,"2005 STAR Preliminary")
    
    ## add the new predictions
    from analysis.asym import theoryCurves
    plusGraphs2 = [
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_std, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_zero, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_max, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_min, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_gsc, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph()
    ]
    minusGraphs2 = [
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_std, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_zero, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_max, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_min, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),    
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_gsc, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph()
    ]
    
    leg = ROOT.TLegend(0.13, 0.65, 0.35, 0.88)
    leg.SetFillStyle(0)
    leg.SetBorderSize(0)
    leg.AddEntry(plusGraphs2[0],' GRSV-STD', 'l')
    leg.AddEntry(plusGraphs2[1],' #Delta G =  0', 'l')
    leg.AddEntry(plusGraphs2[2],' #Delta G =  G', 'l')
    leg.AddEntry(plusGraphs2[3],' #Delta G = -G', 'l')
    leg.AddEntry(plusGraphs2[4],' GS Set C', 'l')
    
    for grList in (plusGraphs2, minusGraphs2):
        grList[1].SetLineColor(ROOT.kBlue)
        grList[2].SetLineColor(ROOT.kRed)
        grList[3].SetLineColor(ROOT.kGreen)
        grList[4].SetLineColor(ROOT.kMagenta)
        for gr in grList:
            gr.SetLineWidth(3)
    
    for grList in (plusGraphs, minusGraphs):
        for gr in grList:
            gr.SetLineStyle(2)
            
    c1.cd()
    leg.Draw('p')
    [ g.Draw('l') for g in plusGraphs2 ]
    latex.DrawLatex(4.8, 0.09, 'solid: DSS  dashed: mod. KKP')
    
    c2.cd()
    leg.Draw('p')
    [ g.Draw('l') for g in minusGraphs2 ]
    latex.DrawLatex(4.8, 0.09, 'solid: DSS  dashed: mod. KKP')
    
    raw_input('wait here:')
    c1.Print('.gif')
    c2.Print('.gif')


def asymmetries_for_publication_run5(runlist=None):
    """final results for inclusive asymmetries from Run 5"""
    asym_plus = analysis.AsymmetryGenerator('asym_plus', key='pt')
    asym_minus = analysis.AsymmetryGenerator('asym_minus', key='pt')
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Final
    
    #theory = analysis.asym.theoryCurves()
    #plusGraphs = [ theory.getGraph('plus',key) for key in ('std','zero','max','min') ]
    #minusGraphs = [ theory.getGraph('minus',key) for key in ('std','zero','max','min') ]
    from analysis.asym import theoryCurves
    plusGraphs = [
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_std, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_zero, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_max, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_min, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_gsc, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph()
    ]
    minusGraphs = [
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_std, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_zero, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_max, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_min, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),    
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_gsc, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph()
    ]
    
    for grList in (plusGraphs, minusGraphs):
        grList[1].SetLineStyle(3)
        grList[1].SetLineColor(ROOT.kBlue)
        grList[2].SetLineStyle(4)
        grList[2].SetLineColor(ROOT.kRed)
        grList[3].SetLineStyle(2)
        grList[3].SetLineColor(ROOT.kGreen)
        grList[4].SetLineStyle(5)
        grList[4].SetLineColor(ROOT.kMagenta)
        for gr in grList:
            gr.SetLineWidth(3)
    
    ## systematic uncertainties
    baseline = -0.1
    syst_x = [3.0, 5.0, 7.0, 9.0]
    
    ## preliminary result
    #syst = {'plus': [7.3, 8.4, 7.5, 5.1], 'minus': [5.7, 6.0, 5.7, 7.1] }
    
    ## assuming pT dependence in PID background
    #syst = {'plus': [2.26, 2.99, 4.59, 10.26], 'minus': [1.05, 1.66, 7.07, 12.41] }
    
    syst = {}
    tmp = systematic_uncertainty_run5('plus')
    syst['plus'] = [1000*elem for elem in tmp]
    tmp = systematic_uncertainty_run5('minus')
    syst['minus'] = [1000*elem for elem in tmp]
    
    systGraph = {'plus': ROOT.TGraph(len(syst_x)+3), 'minus': ROOT.TGraph(len(syst_x)+3) }
    for charge in ('plus','minus'):
        systGraph[charge].SetPoint(0, 3.0, baseline)
        systGraph[charge].SetPoint(5, 9.0, baseline)
        systGraph[charge].SetPoint(6, 3.0, baseline)
        for i,val in enumerate(syst[charge]):
            systGraph[charge].SetPoint(i+1, syst_x[i], baseline + (val/1000.0))
    
    ## generate the asymmetries
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['pt', 'pt_near', 'pt_away'])
            
            try:
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            #asym_plus.FillFromHistogramManager(mgr, 'alltrigs', 1, uu, ud, du, dd, pol.py, pol.pb)
            #asym_minus.FillFromHistogramManager(mgr, 'alltrigs', -1, uu, ud, du, dd, pol.py, pol.pb)
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
            
    ## fun with graphics
    h1 = asym_plus.GetAsymmetry('ll')
    g1 = ROOT.TGraphErrors(h1)
    h2 = asym_minus.GetAsymmetry('ll')
    g2 = ROOT.TGraphErrors(h2)
    
    ## ignore bin width errors
    for gr in (g1,g2):
        for point in range(gr.GetN()):
            gr.SetPointError(point, 0.0, gr.GetErrorY(point))
            
    line = ROOT.TLine(2.0, 0.0, 10.0, 0.0)
    line.SetLineStyle(2)
    
    latex = ROOT.TLatex()
    
    leg = ROOT.TLegend(0.13, 0.62, 0.40, 0.89)
    leg.SetFillStyle(0)
    leg.SetBorderSize(0)
    leg.AddEntry(plusGraphs[0],' GRSV-std', 'l')
    leg.AddEntry(plusGraphs[2],' GRSV #Delta g =     g input', 'l')
    leg.AddEntry(plusGraphs[1],' GRSV #Delta g =     0 input', 'l')
    leg.AddEntry(plusGraphs[3],' GRSV #Delta g = -g input', 'l')
    leg.AddEntry(plusGraphs[4],' GS Set C', 'l')
    
    bg = ROOT.TH1D(h1)
    bg.Reset()
    bg.SetYTitle(' A_{LL}')
    bg.GetYaxis().SetRangeUser(-0.08, 0.06)
    
    ## combo plot
    c3 = ROOT.TCanvas('c3', 'A_{LL} combined', 0, 0, 800, 350)
    #c3 = ROOT.TCanvas('c3', 'A_{LL} combined', 0, 0, 350, 600)
    c3.Draw()
    titlepad = ROOT.TPad('titlepad', '', 0.0, 0.9, 1.0, 1.0)
    
    leftpad  = ROOT.TPad('leftpad','', 0.0, 0.0, 0.5, 1.0)
    leftpad.SetLeftMargin(0.14)
    leftpad.SetRightMargin(0.02)
    #leftpad  = ROOT.TPad('top','', 0.0, 0.45, 1.0, 0.9)
    #leftpad.SetTopMargin(0.14)
    #leftpad.SetRightMargin(0.02)
    
    rightpad = ROOT.TPad('rightpad','', 0.5, 0.0, 1.0, 1.0)
    rightpad.SetLeftMargin(0.11)
    rightpad.SetRightMargin(0.05)
    #rightpad = ROOT.TPad('bottom','', 0.0, 0.0, 1.0, 0.45)
    #rightpad.SetLeftMargin(0.11)
    #rightpad.SetRightMargin(0.05)
    
    for pad in (titlepad, leftpad, rightpad):
        pad.Draw()
        pad.SetFillColor(10)
        pad.SetBorderMode(0)
        pad.SetFillStyle(4000) ## make it transparent
        
    #leg2 = ROOT.TLegend(0.16, 0.62, 0.54, 0.90)
    leg2 = ROOT.TLegend(0.16, 0.12, 0.6, 0.45)
    leg2.SetFillStyle(0)
    leg2.SetBorderSize(0)
    leg2.AddEntry(plusGraphs[0],' GRSV-std', 'l')
    leg2.AddEntry(plusGraphs[2],' GRSV #Deltag =  g input', 'l')
    leg2.AddEntry(plusGraphs[1],' GRSV #Deltag =  0 input', 'l')
    leg2.AddEntry(plusGraphs[3],' GRSV #Deltag = -g input', 'l')
    leg2.AddEntry(plusGraphs[4],' GS Set C', 'l')
    
    titlepad.cd()
    latex.SetTextSize(0.7)
    latex.SetTextAlign(21)
    latex.DrawLatex(0.5,0.3,"STAR #vec{p} + #vec{p} #rightarrow #pi + X at #sqrt{s}=200 GeV \
                       |#eta_{#pi}|<1.0")
    
    leftpad.cd()
    bg.SetXTitle('')
    bg.SetYTitle('A_{LL}')
    bg.GetYaxis().SetTitleSize(0.05)
    bg.GetYaxis().SetTitleOffset(1.22)
    bg.DrawCopy()
    g2.SetMarkerSize(0.9);
    g2.SetMarkerStyle(20)
    g2.Draw('p')
    [ g.Draw('l') for g in minusGraphs ]
    systGraph['minus'].SetLineColor(1)
    systGraph['minus'].SetFillColor(12)
    systGraph['minus'].Draw('fl')
    line.Draw('same')
    leg2.Draw('p')
    latex.SetTextSize(0.2)
    latex.SetTextAlign(21)
    #latex.DrawLatex(4.0,-0.075,'#pi^{-}')
    latex.DrawLatex(3.1,0.03,'#pi^{-}')
    
    rightpad.cd()
    bg.SetXTitle('#pi P_{T} [GeV/c]')
    bg.SetYTitle('')
    bg.DrawCopy()
    g1.SetMarkerSize(0.9);
    g1.SetMarkerStyle(21)
    g1.Draw('p')
    [ g.Draw('l') for g in plusGraphs ]
    systGraph['plus'].SetLineColor(1)
    systGraph['plus'].SetFillColor(12)
    systGraph['plus'].Draw('fl')
    line.Draw('same')
    #latex.DrawLatex(4.0,-0.075,'#pi^{+}')
    latex.DrawLatex(3.1,0.03,'#pi^{+}')
    
    'pi-plus fit to pol0'
    h1.Fit('pol0', 'N', '', 2.0, 10.0)
    'pi-minus fit to pol0'
    h2.Fit('pol0', 'N', '', 2.0, 10.0)
    
    for h in (h1,h2):
        print h.GetName()
        for i in range(h.GetNbinsX()):
            print 'y=% .2e, stat=%.2e' % (h.GetBinContent(i+1), h.GetBinError(i+1))
    raw_input('wait here:')
    
    c3.Print('.eps')


def jet_correlations_run5(runlist=None):
    """3-D deta-dphi plot -- possible paper plot at one time. \
    Also plots the uncorrected pion momentum fraction for near-side
    and away-side
    """
    style = ROOT.TStyle(ROOT.gStyle)
    style.SetOptStat(0)   
    style.SetLabelOffset(-0.01,'xy')
    style.SetLabelSize(0.035,'xy')
    style.SetTitleOffset(1.2,'y')
    style.cd()
    
    h1 = None
    h2 = None
    h3 = None
    
    ## silly hack
    keepMeOpen = []
    
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['dphi_deta', 'z', 'z_away'])
            
            if h1 is None:
                h1 = mgr['anyspin']['jetpatch'].tracks_sum['dphi_deta'].Clone()
                h2 = mgr['anyspin']['jetpatch'].tracks_sum['z'].Clone()
                h3 = mgr['anyspin']['jetpatch'].tracks_sum['z_away'].Clone()
                keepMeOpen.append(tfile)
            else:
                h1.Add(mgr['anyspin']['jetpatch'].tracks_sum['dphi_deta'])
                h2.Add(mgr['anyspin']['jetpatch'].tracks_sum['z'])
                h3.Add(mgr['anyspin']['jetpatch'].tracks_sum['z_away'])
    
    c1 = ROOT.TCanvas('c1')
    c1.SetLogz()
    h1.SetXTitle('#phi pion - #phi jet')
    h1.SetYTitle('#eta pion - #eta jet')
    h1.GetYaxis().SetRangeUser(-2.0, 0.4)
    h1.DrawCopy('lego2')
    
    #reset some styles
    style.SetLabelOffset(0.005,'xy')
    style.SetLabelSize(0.04,'xy')
    style.SetTitleOffset(1,'y')
    
    c2 = ROOT.TCanvas('c2')
    h2.FitSlicesY()
    fig3c_mean = ROOT.gDirectory.Get('%s_1' % (h2.GetName(),))
    fig3c_mean.SetTitle('Uncorrected pion momentum fraction')
    fig3c_mean.SetXTitle('#pi p_{T} [GeV/c]')
    fig3c_mean.SetYTitle('< z >')
    fig3c_mean.SetAxisRange(0,1,'y')
    fig3c_mean.SetMarkerStyle(21)
    
    h3.FitSlicesY()
    fig3c_away_mean = ROOT.gDirectory.Get('%s_1' % (h3.GetName(),))
    fig3c_away_mean.SetMarkerStyle(25)
    fig3c_away_mean.SetMarkerColor(ROOT.kRed)
    fig3c_away_mean.SetLineColor(ROOT.kRed)
    
    leg = ROOT.TLegend(0.75,0.8,0.89,0.89)
    leg.AddEntry(fig3c_mean,'dR < 0.4','p')
    leg.AddEntry(fig3c_away_mean,'dR > 1.5','p')
    
    fig3c_mean.DrawCopy()
    fig3c_away_mean.Draw('same')
    leg.Draw('same')
    
    #c3 = ROOT.TCanvas('c3')
    #h3.Draw()
    
    c4 = ROOT.TCanvas('combo plot')
    mainpad = ROOT.TPad('mainpad', '', 0.0, 0.0, 1.0, 1.0)
    mainpad.SetLeftMargin(0.1)
    mainpad.SetRightMargin(0.03)
    mainpad.SetTopMargin(0.05)
    mainpad.SetBottomMargin(0.1)
    #mainpad.SetTicky()
    
    insetpad = ROOT.TPad('insetpad', '', 0.52, 0.13, 0.97, 0.6)
    insetpad.SetLogz()
    #insetpad.SetLeftMargin(0.)
    #insetpad.SetRightMargin(0.)
    #insetpad.SetTopMargin(0.)
    #insetpad.SetBottomMargin(0.)
    
    for pad in (mainpad, insetpad):
        pad.Draw()
        pad.SetFillColor(10)
        pad.SetBorderMode(0)
        pad.SetFillStyle(4000) ## make it transparent
    
    mainpad.cd()
    fig3c_mean.SetTitle('')
    fig3c_mean.GetYaxis().SetRangeUser(0., 0.7)
    fig3c_mean.Draw()
    fig3c_away_mean.Draw('same')
    
    insetpad.cd()
    h1.GetXaxis().SetTitle('#Delta #phi')
    h1.GetXaxis().SetTitleSize(0.1)
    h1.GetXaxis().SetLabelSize(0.06)
    h1.GetYaxis().SetTitle('#Delta #eta')
    h1.GetYaxis().SetTitleSize(0.1)
    h1.GetYaxis().SetLabelSize(0.06)
    
    ## this is temporary till we get the cuts right
    #h1.GetZaxis().SetRangeUser(10, 600000)
    #h1.GetYaxis().SetRangeUser(-1.5, 0.4)
    h1.GetYaxis().SetRangeUser(-1.7, 0.7)
    
    
    h1.DrawCopy('lego2')
    #h1.DrawCopy('col z')
    
    mainpad.cd()
    leg2 = ROOT.TLegend(0.15, 0.78, 0.45, 0.92)
    leg2.AddEntry(fig3c_mean,'trigger jet','p')
    leg2.AddEntry(fig3c_away_mean,'away-side jet','p')
    leg2.Draw()
    
    raw_input('wait here:')


def jet_correlations_run6():
    """3-D deta-dphi plot -- possible paper plot at one time. \
    Also plots the uncorrected pion momentum fraction for near-side
    and away-side
    """
    style = ROOT.TStyle(ROOT.gStyle)
    style.SetOptStat(0)   
    style.SetLabelOffset(-0.01,'xy')
    style.SetLabelSize(0.035,'xy')
    style.SetTitleOffset(1.2,'y')
    style.cd()
    
    runlist = None
    
    h1 = None
    h2 = None
    h3 = None
    
    ## silly hack
    keepMeOpen = []
    
    allFiles = glob(run6_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['dphi_deta', 'z', 'z_away'])
            
            if h1 is None:
                h1 = mgr['anyspin']['alltrigs'].tracks_sum['dphi_deta'].Clone()
                h2 = mgr['anyspin']['alltrigs'].tracks_sum['z'].Clone()
                h3 = mgr['anyspin']['alltrigs'].tracks_sum['z_away'].Clone()
                keepMeOpen.append(tfile)
            else:
                h1.Add(mgr['anyspin']['alltrigs'].tracks_sum['dphi_deta'])
                h2.Add(mgr['anyspin']['alltrigs'].tracks_sum['z'])
                h3.Add(mgr['anyspin']['alltrigs'].tracks_sum['z_away'])
    
    c1 = ROOT.TCanvas('c1')
    c1.SetLogz()
    h1.SetXTitle('#phi pion - #phi jet')
    h1.SetYTitle('#eta pion - #eta jet')
    h1.DrawCopy('lego2')
    
    #reset some styles
    style.SetLabelOffset(0.005,'xy')
    style.SetLabelSize(0.04,'xy')
    style.SetTitleOffset(1,'y')
    
    c2 = ROOT.TCanvas('c2')
    h2.FitSlicesY()
    fig3c_mean = ROOT.gDirectory.Get('%s_1' % (h2.GetName(),))
    fig3c_mean.SetTitle('Uncorrected pion momentum fraction')
    fig3c_mean.SetXTitle('#pi p_{T}')
    fig3c_mean.SetYTitle('< p_{T,#pi} / p_{T,jet} >')
    fig3c_mean.SetAxisRange(0,1,'y')
    fig3c_mean.SetMarkerStyle(21)
    
    h3.FitSlicesY()
    fig3c_away_mean = ROOT.gDirectory.Get('%s_1' % (h3.GetName(),))
    fig3c_away_mean.SetMarkerStyle(25)
    fig3c_away_mean.SetMarkerColor(ROOT.kRed)
    fig3c_away_mean.SetLineColor(ROOT.kRed)
    
    leg = ROOT.TLegend(0.75,0.8,0.89,0.89)
    leg.AddEntry(fig3c_mean,'dR < 0.4','p')
    leg.AddEntry(fig3c_away_mean,'dR > 1.5','p')
    
    fig3c_mean.Draw()
    fig3c_away_mean.Draw('same')
    leg.Draw('same')
    
    c3 = ROOT.TCanvas('c3')
    h3.Draw()
    
    raw_input('wait here:')


def asymmetry_statistics_comparison():
    """plots statistical prescision of A_{LL} for Run 5 (prelim + final)
    and Run 5 + Run 6 combined away-side measurement
    """
    prelim = analysis.AsymmetryGenerator('prelim')
    final = analysis.AsymmetryGenerator('final')
    combo = analysis.AsymmetryGenerator('combo', key='pt_away')
    
    scalars_run5 = analysis.ScalarCounts(run5_scalar_path)
    scalars_run6 = analysis.ScalarCounts(run6_scalar_path)
    
    polarizations = analysis.Polarizations.Final
    polarizations_prelim = analysis.Polarizations.Online
    
    ## generate the asymmetries
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        print fname, run
        tfile = ROOT.TFile(fname)
        mgr = analysis.HistogramManager(tfile,['pt', 'pt_away'])
            
        try:
            bin7 = scalars_run5[str(run) + '-5-7']
            bin8 = scalars_run5[str(run) + '-5-8']
            bin9 = scalars_run5[str(run) + '-5-9']
        except KeyError:
            print run, 'is not in the scalars database'
            continue
        uu = bin7.uu + bin8.uu + bin9.uu
        ud = bin7.ud + bin8.ud + bin9.ud
        du = bin7.du + bin8.du + bin9.du
        dd = bin7.dd + bin8.dd + bin9.dd
        
        if run in analysis.golden_runlist_c:
            try:
                pol = polarizations_prelim[bin7.fill]
                prelim.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            except KeyError:
                print bin7.fill, 'has no preliminary polarization values'
            
        try:
            pol = polarizations[bin7.fill]
            final.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            combo.FillFromHistogramManager(mgr, 'alltrigs', 1, uu, ud, du, dd, pol.py, pol.pb)
        except KeyError:
            print bin7.fill, 'has no final polarization values'
        
        tfile.Close()
    
    allFiles = glob(run6_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        print fname, run
        tfile = ROOT.TFile(fname)
        mgr = analysis.HistogramManager(tfile,['pt', 'pt_away'])
            
        try:
            bin6 = scalars_run6[str(run) + '-5-6']
            bin7 = scalars_run6[str(run) + '-5-7']
            bin8 = scalars_run6[str(run) + '-5-8']
            bin9 = scalars_run6[str(run) + '-5-9']
        except KeyError:
            print run, 'is not in the scalars database'
            continue
        uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
        ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
        du = bin6.du + bin7.du + bin8.du + bin9.du
        dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
        
        try:
            pol = polarizations[bin6.fill]
            combo.FillFromHistogramManager(mgr, 'alltrigs', 1, uu, ud, du, dd, pol.py, pol.pb)
        except KeyError:
            print bin6.fill, 'has no final polarization values'
        
        tfile.Close()
        
    h1 = prelim.GetAsymmetry('ll')
    h2 = final.GetAsymmetry('ll')
    h3 = combo.GetAsymmetry('ll')
    
    g1 = ROOT.TGraphErrors(h1)
    g2 = ROOT.TGraphErrors(h2)
    g3 = ROOT.TGraphErrors(h3)
    
    for point in range(g1.GetN()):
        x = h1.GetBinCenter(point+1)
        g1.SetPoint(point, x-0.2, 0)
        g2.SetPoint(point, x, 0)
        g3.SetPoint(point, x+0.2, 0)
        for gr in (g1,g2,g3):
            gr.SetMarkerStyle(21)
            gr.SetPointError(point, 0.0, gr.GetErrorY(point))
            
    g2.SetMarkerColor(ROOT.kRed)
    g2.SetLineColor(ROOT.kRed)
    g3.SetMarkerColor(ROOT.kGreen)
    g3.SetLineColor(ROOT.kGreen)
    
    leg = ROOT.TLegend(0.13, 0.67, 0.40, 0.89)
    leg.SetFillStyle(0)
    leg.SetBorderSize(0)
    leg.AddEntry(g1, '2005 prelim', 'p')
    leg.AddEntry(g2, '2005 final', 'p')
    leg.AddEntry(g3, '2005/6 away-side', 'p')
    
    bg = ROOT.TH1D(h1)
    bg.Reset()
    bg.SetTitle('Statistical Precision of Various A_{LL} Measurements')
    bg.SetXTitle('p_{T}')
    bg.GetYaxis().SetRangeUser(-0.06, 0.06)
    
    c = ROOT.TCanvas()
    bg.DrawCopy()
    g1.Draw('p')
    g2.Draw('p')
    g3.Draw('p')
    leg.Draw()
    
    raw_input('press enter:')
    


def trigger_bias_using_away_side(runlist=None, trgname='alltrigs'):
    """plot asym vs. pt for near + away, fit with pol0 and compare"""
    generator = { 
    'near_plus'  : analysis.AsymmetryGenerator('near_plus', key='pt_near'), 
    'near_minus' : analysis.AsymmetryGenerator('near_minus', key='pt_near'),
    'away_plus'  : analysis.AsymmetryGenerator('away_plus', key='pt_away'), 
    'away_minus' : analysis.AsymmetryGenerator('away_minus', key='pt_away') 
    }
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Final
    
    ## generate the asymmetries
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['pt_near', 'pt_away'])
            
            try:
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            generator['near_plus'].FillFromHistogramManager(mgr, trgname, 1, uu, ud, du, dd, pol.py, pol.pb)
            generator['near_minus'].FillFromHistogramManager(mgr, trgname, -1, uu, ud, du, dd, pol.py, pol.pb)
            generator['away_plus'].FillFromHistogramManager(mgr, trgname, 1, uu, ud, du, dd, pol.py, pol.pb)
            generator['away_minus'].FillFromHistogramManager(mgr, trgname, -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    #ROOT.gStyle.SetOptStat('n')
    #ROOT.gStyle.SetOptFit(111)
    fit = {
    'near_plus' : ROOT.TF1('near_plus', 'pol0'),
    'near_minus' : ROOT.TF1('near_minus', 'pol0'),
    'away_plus' : ROOT.TF1('away_plus', 'pol0'),
    'away_minus' : ROOT.TF1('away_minus', 'pol0'),
    }
    fit['away_plus'].SetLineColor(ROOT.kRed)
    fit['away_minus'].SetLineColor(ROOT.kRed)
    
    c1 = ROOT.TCanvas('plus','Comparison of near and away side for #pi^{+}')
    h1_near = generator['near_plus'].GetAsymmetry('ll')
    h1_near.GetYaxis().SetRangeUser(-0.11, 0.11)
    h1_away = generator['away_plus'].GetAsymmetry('ll')
    h1_away.SetLineColor(ROOT.kRed)
    h1_near.Draw()
    h1_near.Fit(fit['near_plus'],'','same')
    h1_away.Draw('same')
    h1_away.Fit(fit['away_plus'],'','same')
    
    leg1 = ROOT.TLegend(0.13,0.7,0.43,0.89)
    leg1.AddEntry(fit['near_plus'],'%f +/- %f' % 
        (fit['near_plus'].GetParameter(0), fit['near_plus'].GetParError(0)),'l')
    leg1.AddEntry(fit['away_plus'],'%f +/- %f' % 
        (fit['away_plus'].GetParameter(0), fit['away_plus'].GetParError(0)),'l')
    leg1.Draw()
    
    c2 = ROOT.TCanvas('minus','Comparison of near and away side for #pi^{-}')
    h2_near = generator['near_minus'].GetAsymmetry('ll')
    h2_near.GetYaxis().SetRangeUser(-0.11, 0.11)
    h2_away = generator['away_minus'].GetAsymmetry('ll')
    h2_away.SetLineColor(ROOT.kRed)
    h2_near.Draw()
    h2_near.Fit(fit['near_minus'],'','same')
    h2_away.Draw('same')
    h2_away.Fit(fit['away_minus'],'','same')
    
    leg2 = ROOT.TLegend(0.13,0.7,0.43,0.89)
    leg2.AddEntry(fit['near_minus'],'%f +/- %f' % 
        (fit['near_minus'].GetParameter(0), fit['near_minus'].GetParError(0)),'l')
    leg2.AddEntry(fit['away_minus'],'%f +/- %f' % 
        (fit['away_minus'].GetParameter(0), fit['away_minus'].GetParError(0)),'l')
    leg2.Draw()
    
    print 'Size of systematic assigned if we take the difference btw the fits with errors:'
    val = math.fabs( fit['near_plus'].GetParameter(0) - fit['away_plus'].GetParameter(0) )
    err = math.sqrt(fit['near_plus'].GetParError(0) ** 2 + fit['away_plus'].GetParError(0) ** 2)
    print 'plus  : %f' % (val+err,)
    val = math.fabs( fit['near_minus'].GetParameter(0) - fit['away_minus'].GetParameter(0) )
    err = math.sqrt(fit['near_minus'].GetParError(0) ** 2 + fit['away_minus'].GetParError(0) ** 2)
    print 'minus : %f' % (val+err,)
    
    
    raw_input('press enter:')


def qa_pid():
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    fill_runlists = {}
    reverse_dict = {}
    
    for fname in allFiles:
        run = analysis.getRun(fname)
        reverse_dict[run] = 0.0
    answer = analysis.getAllFills(reverse_dict.keys())
    for run,fill in answer:
        reverse_dict[run] = int(fill)
        try:
            fill_runlists[fill].append(run)
        except KeyError:
            fill_runlists[fill] = [run]    
    
    nSigmaRun = ROOT.TH1D('nSigmaRun', 'Mean nSigmaPion per run', len(allFiles), 0.5, len(allFiles)+0.5)
    nSigmaFill = ROOT.TH1D('nSigmaFill', 'blerg2', len(fill_runlists), 0.5, len(fill_runlists)+0.5)
    
    ps = ROOT.TPostScript('blerg.ps')
    c = ROOT.TCanvas('c','',100,100,600,800)
    
    pad = 1
    ROOT.gStyle.SetOptStat('m')
    for row,fname in enumerate(allFiles):
        if row % 15 == 0:
            c.Update()
            ps.NewPage()
            c.Clear()
            c.Divide(3,5)
            pad = 1 
        c.cd(pad)    
        print fname
        tfile = ROOT.TFile(fname)
        mgr = analysis.HistogramManager(tfile,['nSigmaPion'])
        h = mgr.anyspin['alltrigs'].tracks_sum['nSigmaPion']
        run = analysis.getRun(fname)
        h.SetTitle('%3d - %d - %d' % (row+1, run, reverse_dict[run]))
        h.SetStats(True)
        if h.GetMean() > 0: 
            h.SetLineColor(ROOT.kRed)
            print 'bad = fill, run'
        h.DrawCopy()
        nSigmaRun.SetBinContent(row+1, h.GetMean())
        nSigmaRun.SetBinError(row+1, h.GetMeanError())
        pad += 1
    ps.Close()
    c = ROOT.TCanvas()
    nSigmaRun.Draw()
    raw_input('press enter:')


def jetpatch_phi_correlation(tree, patchNumber):
    patchPhi = analysis.histos.JetCuts.patchPhi2006
    h = ROOT.TH1D('h','',720,-360,360)
    for entry in tree:
        for i in range(12):
            adc = tree.event.jetPatchAdc(i)
            if adc > analysis.histos.JetCuts.triggerThresholds[137221]:
                for jet in tree.event.jets():
                    diff = math.degrees(jet.Phi()) - patchPhi[i]
                    h.Fill(diff)
    h.Draw()
    raw_input('press enter:')


def pid_calibration(h, useElectrons=True):
    """takes an nSigmaPion histo and does the triple-Gaussian fit"""
    h.SetStats(True)
    #h.SetTitle('n#sigma(#pi) calibration for F7305')
    h.SetXTitle('n#sigma(#pi)')
    ROOT.gStyle.SetOptFit(111)
    
    if useElectrons:
        fit = ROOT.TF1('fit','gaus(0)+gaus(3)+gaus(6)', -6.0, 6.0)
    else:
        fit = ROOT.TF1('fit','gaus(0)+gaus(3)', -6.0, 6.0)
    fit.SetParameter(0, h.GetMaximum() * 0.9)
    fit.SetParameter(1, 0.0)
    fit.SetParameter(2, 1.0)
    fit.SetParameter(3, h.GetMaximum() * 0.5)
    fit.SetParameter(4, -2.0)
    fit.SetParameter(5, 1.0)
    if useElectrons:
        fit.SetParameter(6, h.GetMaximum() * 0.05)
        #fit.SetParLimits(6, 0.0, h.GetMaximum() * 0.1)
        fit.SetParameter(7, 3.)
        fit.SetParLimits(7, 1.5, 5.0)  ## this one drops b/g by 10% !
        fit.SetParameter(8, 1.0)
    
    fit.SetParName(0, '#pi magnitude')
    fit.SetParName(1, '#pi mean')
    fit.SetParName(2, '#pi width')
    fit.SetParName(3, 'p/K magnitude')
    fit.SetParName(4, 'p/K mean')
    fit.SetParName(5, 'p/K width')
    if useElectrons:
        fit.SetParName(6, 'ele magnitude')
        fit.SetParName(7, 'ele mean')
        fit.SetParName(8, 'ele width')
    
    h.Fit(fit, 'rq')
    h.DrawCopy()
    
    pifit = ROOT.TF1('pifit', 'gaus', -6.0, 6.0)
    pifit.FixParameter(0, fit.GetParameter(0))
    pifit.FixParameter(1, fit.GetParameter(1))
    pifit.FixParameter(2, fit.GetParameter(2))
    pifit.SetLineColor(ROOT.kRed)
    
    pkfit = ROOT.TF1('pkfit', 'gaus', -6.0, 6.0)
    pkfit.FixParameter(0, fit.GetParameter(3))
    pkfit.FixParameter(1, fit.GetParameter(4))
    pkfit.FixParameter(2, fit.GetParameter(5))
    pkfit.SetLineColor(ROOT.kGreen)
    
    if useElectrons:
        elefit = ROOT.TF1('elefit', 'gaus', -6.0, 6.0)
        elefit.FixParameter(0, fit.GetParameter(6))
        elefit.FixParameter(1, fit.GetParameter(7))
        elefit.FixParameter(2, fit.GetParameter(8))
        elefit.SetLineColor(ROOT.kBlue)
    
    pifit.DrawCopy('same')
    pkfit.DrawCopy('same')
    if useElectrons:
        elefit.DrawCopy('same')
    
    lowBound = pifit.GetParameter(1) - 1.0 * pifit.GetParameter(2)
    highBound = pifit.GetParameter(1) + 2.0 * pifit.GetParameter(2)
    totalPions = pifit.Integral(-6.0, 6.0)
    oldPions = pifit.Integral(-1.0, 2.0)
    newPions = pifit.Integral(lowBound, highBound)
    if useElectrons:
        oldBg = pkfit.Integral(-1.0, 2.0) + elefit.Integral(-1.0, 2.0)
        newBg = pkfit.Integral(lowBound, highBound) + elefit.Integral(lowBound, highBound)
    else:
        oldBg = pkfit.Integral(-1.0, 2.0)
        newBg = pkfit.Integral(lowBound, highBound)
    print 'tot=%7.2f old eff=%.2f new eff=%.2f old bg=%.2f new bg=%.2f' % \
        (totalPions, oldPions/totalPions, newPions/totalPions, oldBg/fit.Integral(-1.0, 2.0), newBg/fit.Integral(lowBound, highBound))
    #print 'old', pifit.Integral(-1.0, 2.0), 'new', pifit.Integral(lowBound, highBound)
    #print 'old', pkfit.Integral(-1.0, 2.0), 'new', pkfit.Integral(lowBound, highBound)
    #print 'old', elefit.Integral(-1.0, 2.0), 'new', elefit.Integral(lowBound, highBound)
    
    
    #print h.GetTitle()[-4:], pifit.GetParameter(1)
    #raw_input('press enter to continue:')
    if useElectrons:
        return fit, pifit, pkfit, elefit
    else:
        return fit, pifit, pkfit


def pid_calibration_allfills(mydir='/Users/kocolosk/data/run5/hist-by-fill'):
    """generates a PDF of triple-Gaussian fits for all fills, plus a histogram of pion means"""
    allFiles = os.listdir(mydir)
    hfill = ROOT.TH1D('hfill','mean of pion Gaussian by RHIC Fill', len(allFiles), 0.5, len(allFiles)+0.5)
    ps = ROOT.TPostScript('pid.ps')
    c = ROOT.TCanvas('c','',100,100,600,800)
    pad = 1
    
    ## some cumulative stats
    nEntries = 0
    nTotalPions = 0
    nOldPions = 0
    nNewPions = 0
    nOldBg = 0
    nNewBg = 0
    nOldCounts = 0
    nNewCounts = 0
    
    myrecords = [] ## fname, pi mean, pi sigma
    counter = 0
    for fname in allFiles:
        if not fname.endswith('.root'): continue
        if counter % 15 == 0:
            c.Update()
            ps.NewPage()
            c.Clear()
            c.Divide(3,5)
            pad = 1
        counter += 1 
        c.cd(pad)    
        print fname
        tfile = ROOT.TFile(os.path.join(mydir, fname))
        mgr = analysis.HistogramManager(tfile, ['nSigmaPion'])
        h = mgr.anyspin['alltrigs'].tracks_sum['nSigmaPion']
        h.SetTitle('n#sigma(#pi) calibration for F%s' % (fname[-14:-10],))
        fit, pifit, pkfit, elefit = pid_calibration(h)
        mean = fit.GetParameter(1)
        error = fit.GetParError(1)
        sigma = fit.GetParameter(2)
        hfill.SetBinContent(counter+1, mean)
        hfill.SetBinError(counter+1, error)
        myrecords.append((fname, mean, sigma))
        pad += 1
        
        ## stats
        lowBound = pifit.GetParameter(1) - 1.0 * pifit.GetParameter(2)
        highBound = pifit.GetParameter(1) + 2.0 * pifit.GetParameter(2)
        binWidth = h.GetBinWidth(1)
        nEntries += h.GetEntries()
        nTotalPions += pifit.Integral(-6.0, 6.0) / binWidth
        nOldPions += pifit.Integral(-1.0, 2.0) / binWidth
        nNewPions += pifit.Integral(lowBound, highBound) / binWidth
        nOldBg += (pkfit.Integral(-1.0, 2.0) + elefit.Integral(-1.0, 2.0)) / binWidth
        nNewBg += (pkfit.Integral(lowBound, highBound) + elefit.Integral(lowBound, highBound)) / binWidth
        nOldCounts += fit.Integral(-1.0, 2.0) / binWidth
        nNewCounts += fit.Integral(lowBound, highBound) / binWidth
    ps.Close()
    c = ROOT.TCanvas()
    hfill.GetYaxis().SetRangeUser(-0.5, 0.8)
    hfill.SetXTitle('fill index')
    hfill.Draw('e')
    
    for r in myrecords:
        print '%d : (% 1.6f, %1.6f),' % (int(r[0][13:17]), r[1], r[2])
    
    print 'Old Total Efficiency    = %.4f' % (nOldPions/nTotalPions,)
    print 'New Total Efficiency    = %.4f' % (nNewPions/nTotalPions,)
    print 'Old Background Fraction = %.4f' % (nOldBg/nOldCounts,)
    print 'New Background Fraction = %.4f' % (nNewBg/nNewCounts,)
    print 'Total Statistics Old = %.0f New %.0f' % (nOldCounts, nNewCounts)
    raw_input('press enter:')


def print_statistics(runlist, keytype='event'):
    """Run 5 only: prints detailed (event/track) statistics for all runs in runlist"""
    a = [0, 0, 0, 0, 0, 0, 0, 0]
    print ' ---------------------------------------------------------------------------------- '
    print '|                  96011   96201   96211   96221   96233 |      HT      JP     ALL |'
    for row,run in enumerate(runlist):
        if row % 10 == 0: 
            print ' ---------------------------------------------------------------------------------- '
        f = ROOT.TFile('~/data/run5/hist/chargedPions_%d.hist.root' % run)
        if keytype=='event':
            h = analysis.HistogramManager(f,'bx7').anyspin
            b = [
                h['96011']['bx7'].GetEntries(),
                h['96201']['bx7'].GetEntries(),
                h['96211']['bx7'].GetEntries(),
                h['96221']['bx7'].GetEntries(),
                h['96233']['bx7'].GetEntries(),
                h['hightower']['bx7'].GetEntries(),
                h['jetpatch']['bx7'].GetEntries(),
                h['alltrigs']['bx7'].GetEntries() 
            ]
        else:
            h = analysis.HistogramManager(f,'pt').anyspin
            b = [
                h['96011'].tracks_sum['pt'].GetEntries(),
                h['96201'].tracks_sum['pt'].GetEntries(),
                h['96211'].tracks_sum['pt'].GetEntries(),
                h['96221'].tracks_sum['pt'].GetEntries(),
                h['96233'].tracks_sum['pt'].GetEntries(),
                h['hightower'].tracks_sum['pt'].GetEntries(),
                h['jetpatch'].tracks_sum['pt'].GetEntries(),
                h['alltrigs'].tracks_sum['pt'].GetEntries() 
            ]
        print '| %d %d : %7d %7d %7d %7d %7d | %7d %7d %7d |' % (analysis.getFill(run), run, \
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7] )
        for i in range(len(b)):
            a[i] += b[i]
    print ' ---------------------------------------------------------------------------------- '
    print '|     sum      : %7d %7d %7d %7d %7d | %7d %7d %7d |' % \
        (a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7])


def runlist_luminosity(runlist):
    """prints integrated luminosity seen by minbias trigger in runs of this list"""
    lumiSum = 0
    for run in runlist:
        if run > 7000000:
            f = ROOT.TFile('~/data/run6/tree/chargedPions_%d.tree.root' % (run,))
            minBiasId = 117001
        else:
            path = '/Users/kocolosk/data/run5/tree/chargedPions_%d.tree.root' % run
            if os.path.isfile(path):
                f = ROOT.TFile(path)
            else:
                path = '/Users/kocolosk/data/run5/tree-minbias/chargedPions_%d.tree.root' % run
                f = ROOT.TFile(path)
            minBiasId = 96011
        try:
            tree = f.tree
            lumi = analysis.tree.integratedLuminosity(tree, minBiasId)
            print '%d: %3.6f nb^-1' % (run, lumi)
            lumiSum += lumi
        except AttributeError:
            pass
    print 'Integrated Recorded Luminosity for Runlist: %3.6f pb^-1' % (lumiSum/1000,)


def spinInfoForFrank():
    """not actually a plot"""
    chain = ROOT.TChain('tree')
    chain.Add('~/data/run5/tree/chargedPions_*')
    
    ## only adding these while the other ones spin
    chain.Add('~/data/run5/tree/backup-2008-01-08-trigger-prescales/chargedPions_*')
    
    chain.SetBranchStatus('*',0)
    chain.SetBranchStatus('mRunId',1)
    chain.SetBranchStatus('mEventId',1)
    chain.SetBranchStatus('mSpinBit',1)
    chain.SetBranchStatus('mBx7',1)
    chain.SetBranchStatus('mSpinQA',1)
    
    f = ROOT.TFile('spin_info.root','recreate')
    nt = ROOT.TNtuple('nt','spin info','run:event:spinbit:bx7:qa')
        
    for entry in chain:
        ev = entry.event
        nt.Fill(ev.runId(), ev.eventId(), ev.spinBit(), ev.bx7(), ev.isSpinValid())
    
    nt.Write()
    f.Close()


def ssa_by_fill(asym_key='ly', runlist=analysis.final_runlist_run5, variable='pt', year=2006, bins=None):
    """generates canvas for asym_key (ly,lb,ls,us). Each data point is the SSA for a fill"""
    tuples = analysis.getAllFills(runlist)
    fills = []
    
    for run, fill in tuples:
        ## temporary hacks
        ## http://www.star.bnl.gov/HyperNews-star/protected/get/starspin/3324.html
        if 6144002 <= run <= 6144029: fill = 7128
        if 6144041 <= run <= 6144042: fill = 7129        
        if 6145067 <= run <= 6145068: fill = 7136
        if 6146001 <= run <= 6146026: fill = 7138
        if run in runlist:
            fills.append(int(fill))
    fills = analysis.uniqify(fills)
    
    asym_plus = {}
    asym_minus = {}
    for f in fills:
        asym_plus[f] = analysis.AsymmetryGenerator(name='F%s_plus' % f, bins=bins or [1,0.0,1.0], key=variable)
        asym_minus[f] = analysis.AsymmetryGenerator(name='F%s_minus' % f, bins=bins or [1,0.0,1.0], key=variable)
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Final
    
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,('pt',))
            
            try:
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus[bin7.fill].FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus[bin7.fill].FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    title = {'ly':'Yellow Beam', 'lb':'Blue Beam', 'ls':'Like-Sign', 'us':'Unlike-Sign'}
    final_hist_plus = ROOT.TH1D('final_hist_plus','#pi^{+} %s SSA' % title[asym_key], len(fills), 0.5, len(fills)+0.5)
    final_hist_minus = ROOT.TH1D('final_hist_minus','#pi^{-} %s SSA' % title[asym_key], len(fills), 0.5, len(fills)+0.5)
    marker_color = {'ly':ROOT.kYellow, 'lb':ROOT.kBlue, 'ls':ROOT.kRed, 'us':ROOT.kBlack}
    for h in (final_hist_plus, final_hist_minus):
        h.SetMarkerColor( marker_color[asym_key] )
        h.GetYaxis().SetRangeUser(-0.2, 0.2)
        h.SetXTitle('fill index')
    final_hist_plus.SetMarkerStyle(21)
    final_hist_minus.SetMarkerStyle(20)
    for i,f in enumerate(fills):
        hplus = asym_plus[f].GetAsymmetry(asym_key)
        final_hist_plus.SetBinContent( i+1, hplus.GetBinContent(1) )
        final_hist_plus.SetBinError( i+1, hplus.GetBinError(1) )
        print '%d % .4f % .4f % .4f' % (f, hplus.GetBinContent(1), hplus.GetBinError(1), hplus.GetBinContent(1)/hplus.GetBinError(1))
        hplus.Delete()
        
        hminus = asym_minus[f].GetAsymmetry(asym_key)
        final_hist_minus.SetBinContent( i+1, hminus.GetBinContent(1) )
        final_hist_minus.SetBinError( i+1, hminus.GetBinError(1) )
        hminus.Delete()
    
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    
    cp = ROOT.TCanvas('%s_ssa_fill_plus' % asym_key)
    final_hist_plus.Draw('e1')
    final_hist_plus.Fit('pol0')
    cm = ROOT.TCanvas('%s_ssa_fill_minus' % asym_key)
    final_hist_minus.Draw('e1')
    final_hist_minus.Fit('pol0')
    raw_input('wait here')
    cp.Print('.png')
    cm.Print('.png')


def ssa(asym_key='ly', runlist=analysis.final_runlist_run5, variable='pt'):
    """plots a ssa against the given variable"""
    ## note: need to redefine binning if variable != pt
    asym_plus = analysis.AsymmetryGenerator('asym_plus', key=variable)
    asym_minus = analysis.AsymmetryGenerator('asym_minus', key=variable)
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    polarizations = analysis.Polarizations.Final
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,('pt',))
            
            try:
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
            
    title = {'ly':'Yellow Beam', 'lb':'Blue Beam', 'ls':'Like-Sign', 'us':'Unlike-Sign'}
    marker_color = {'ly':ROOT.kYellow, 'lb':ROOT.kBlue, 'ls':ROOT.kRed, 'us':ROOT.kBlack}
    
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    
    hp = asym_plus.GetAsymmetry(asym_key)
    hp.SetTitle(title[asym_key] + ' SSA for #pi^{+}')
    hp.SetMarkerStyle(21)
    
    hm = asym_minus.GetAsymmetry(asym_key)
    hm.SetTitle(title[asym_key] + ' SSA for #pi^{-}')
    hm.SetMarkerStyle(20)
    
    for h in (hp,hm):
        h.SetMarkerColor(marker_color[asym_key])
        h.SetXTitle(variable)
        if asym_key in ('ly', 'lb'):
            h.GetYaxis().SetRangeUser(-0.05, 0.05)
        else:
            h.GetYaxis().SetRangeUser(-0.1, 0.1)
    
    cp = ROOT.TCanvas('cp')
    hp.Fit('pol0')
    hp.Draw('e1')
    
    cm = ROOT.TCanvas('cm')
    hm.Fit('pol0')
    hm.Draw('e1')
    
    raw_input('wait here')
    cp.Print('%s_ssa_%s_plus.png' % (asym_key, variable))
    cm.Print('%s_ssa_%s_minus.png' % (asym_key, variable))
    


def pid_background_asymmetry(runlist=analysis.final_runlist_run5):
    """plots A_{LL} for charged tracks outside PID window and fits with a pol0"""
    asym_plus = analysis.AsymmetryGenerator('asym_plus', key='pt_bg')
    asym_minus = analysis.AsymmetryGenerator('asym_minus', key='pt_bg')
    #scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    polarizations = analysis.Polarizations.Final
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    allFiles += glob(run6_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,('pt_bg',))
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                try:
                    bin6 = scalars[str(run) + '-6-6']
                    bin7 = scalars[str(run) + '-6-7']
                    bin8 = scalars[str(run) + '-6-8']
                    bin9 = scalars[str(run) + '-6-9']
                except KeyError:
                    print run, 'is not in the scalars database'
                continue
            if run > 7000000:
                uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
                ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
                du = bin6.du + bin7.du + bin8.du + bin9.du
                dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            else:
                uu = bin7.uu + bin8.uu + bin9.uu
                ud = bin7.ud + bin8.ud + bin9.ud
                du = bin7.du + bin8.du + bin9.du
                dd = bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
            
    #title = {'ly':'Yellow Beam', 'lb':'Blue Beam', 'ls':'Like-Sign', 'us':'Unlike-Sign'}
    #marker_color = {'ly':ROOT.kYellow, 'lb':ROOT.kBlue, 'ls':ROOT.kRed, 'us':ROOT.kBlack}
    
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    
    hp = asym_plus.GetAsymmetry('ll')
    hp.SetTitle('PID Background A_{LL} for #pi^{+} (Run 6 data)')
    hp.SetMarkerStyle(21)
    
    hm = asym_minus.GetAsymmetry('ll')
    hm.SetTitle('PID Background A_{LL} for #pi^{-} (Run 6 data)')
    hm.SetMarkerStyle(20)
    
    for h in (hp,hm):
        #h.SetMarkerColor(marker_color[asym_key])
        h.SetXTitle('p_{T}')
       # if asym_key in ('ly', 'lb'):
        h.GetYaxis().SetRangeUser(-0.1, 0.1)
        #else:
        #    h.GetYaxis().SetRangeUser(-0.1, 0.1)
    
    cp = ROOT.TCanvas('cp')
    hp.Fit('pol0')
    hp.Draw('e1')
    
    cm = ROOT.TCanvas('cm')
    hm.Fit('pol0')
    hm.Draw('e1')
    
    for h in (hp,hm):
        print h.GetName()
        for i in range(h.GetNbinsX()):
            print 'y=% .2e, stat=%.2e' % (h.GetBinContent(i+1), h.GetBinError(i+1))
    raw_input('wait here')
    cp.Print('pid_background_asymmetry_plus.png')
    cm.Print('pid_background_asymmetry_minus.png')


def print_ssa(runlist=analysis.final_runlist_run5, charge=1):
    """prints (val +/- err) => (nSig) for y,b,ls,us for each run in list"""
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    polarizations = analysis.Polarizations.Final
    
    print '------------------------------------------------------------------------------------\
---------------------------------------------------------------'
    print ' Fill |   Run    |          Yellow Beam           |           Blue Beam            |\
           Like Sign            |          Unlike Sign          '
    print '------------------------------------------------------------------------------------\
---------------------------------------------------------------'
    for run in runlist:
        gen = analysis.AsymmetryGenerator(name=str(run), bins=[1,2.0,10.0])
        f = ROOT.TFile(run5_hist_dir + '/chargedPions_%d.hist.root' % run)
        mgr = analysis.HistogramManager(f,('pt',))
        
        try:
            bin7 = scalars[str(run) + '-5-7']
            bin8 = scalars[str(run) + '-5-8']
            bin9 = scalars[str(run) + '-5-9']
        except KeyError:
            print run, 'is not in the scalars database'
            continue
        uu = bin7.uu + bin8.uu + bin9.uu
        ud = bin7.ud + bin8.ud + bin9.ud
        du = bin7.du + bin8.du + bin9.du
        dd = bin7.dd + bin8.dd + bin9.dd
        
        try:
            pol = polarizations[bin7.fill]
        except KeyError:
            print bin7.fill, 'has no final polarization values'
            continue
        
        gen.FillFromHistogramManager(mgr, 'jetpatch', charge, uu, ud, du, dd, pol.py, pol.pb)
        f.Close()
        y = gen.GetAsymmetry('ly')
        line = 'F%d | R%d | (% .5f +- %.5f) => % .2f |' % (bin7.fill, run, y.GetBinContent(1), \
            y.GetBinError(1), (y.GetBinContent(1)/y.GetBinError(1)) )
        b = gen.GetAsymmetry('lb')
        line += ' (% .5f +- %.5f) => % .2f |' % (b.GetBinContent(1), \
            b.GetBinError(1), (b.GetBinContent(1)/b.GetBinError(1)))
        ls = gen.GetAsymmetry('ls')
        line += ' (% .5f +- %.5f) => % .2f |' % (ls.GetBinContent(1), \
            ls.GetBinError(1), (ls.GetBinContent(1)/ls.GetBinError(1)))
        us = gen.GetAsymmetry('us')
        line += ' (% .5f +- %.5f) => % .2f' % (us.GetBinContent(1), \
            us.GetBinError(1), (us.GetBinContent(1)/us.GetBinError(1)))
        print line
        [ h.Delete() for h in (y,b,ls,us) ]


def asigma(runlist=analysis.transverse_run6):
    """plots asigma -- duh"""
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    asym_plus = analysis.AsymmetryGenerator('asym_plus', key='pt')
    asym_minus = analysis.AsymmetryGenerator('asym_minus', key='pt')
    #scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    polarizations = analysis.Polarizations.Final
    allFiles = glob(run5_hist_dir + '-transverse/chargedPions_*.hist.root')
    allFiles += glob(run6_hist_dir + '-transverse/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,('pt',))
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                try:
                    bin6 = scalars[str(run) + '-6-6']
                    bin7 = scalars[str(run) + '-6-7']
                    bin8 = scalars[str(run) + '-6-8']
                    bin9 = scalars[str(run) + '-6-9']
                except KeyError:
                    print run, 'is not in the scalars database'
                continue
            if run > 7000000:
                uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
                ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
                du = bin6.du + bin7.du + bin8.du + bin9.du
                dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            else:
                uu = bin7.uu + bin8.uu + bin9.uu
                ud = bin7.ud + bin8.ud + bin9.ud
                du = bin7.du + bin8.du + bin9.du
                dd = bin7.dd + bin8.dd + bin9.dd
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    hp = asym_plus.GetAsymmetry('ll')
    hp.SetTitle('A_{#Sigma} for #pi^{+} using Run 6 data')
    hp.SetMarkerStyle(21)
    
    hm = asym_minus.GetAsymmetry('ll')
    hm.SetTitle('A_{#Sigma} for #pi^{-} using Run 6 data')
    hm.SetMarkerStyle(20)
    
    for h in (hp,hm):
        h.SetXTitle('p_{T}')
        h.GetYaxis().SetRangeUser(-0.105, 0.10)
    
    cp = ROOT.TCanvas('cp')
    hp.Fit('pol0')
    hp.Draw('e1')
    
    cm = ROOT.TCanvas('cm')
    hm.Fit('pol0')
    hm.Draw('e1')
    
    for h in (hp,hm):
        print h.GetName()
        for i in range(h.GetNbinsX()):
            print 'y=% .2e, stat=%.2e' % (h.GetBinContent(i+1), h.GetBinError(i+1))
    
    raw_input('wait here')
    cp.Print('asigma_plus.png')
    cm.Print('asigma_minus.png')
    


def pid_pt_dependence(runlist=analysis.final_runlist_run5, tfile=None):
    """plots recalibrated nSigmaPion for charge-summed pions in each pT bin"""
    if tfile is None:
        nsig = [ ROOT.TH1D('nsig_%d' % i,'',100,-6.0,6.0) for i in range(5)]
        nsig[0].SetTitle('Recalibrated n#sigma(#pi), 2<p_{T}<10')
        nsig[1].SetTitle('Recalibrated n#sigma(#pi), 8<p_{T}<10')
        nsig[2].SetTitle('Recalibrated n#sigma(#pi), 6<p_{T}<8')
        nsig[3].SetTitle('Recalibrated n#sigma(#pi), 4<p_{T}<6')
        nsig[4].SetTitle('Recalibrated n#sigma(#pi), 2<p_{T}<4')
        ptbins = [2.0, 4.0, 6.0, 8.0]
        ptbins.reverse()
        ecuts = analysis.histos.EventCuts()
        for run in runlist:
            fill = analysis.getFill(run)
            tcuts = analysis.histos.TrackCuts(fill)
            pidFit = analysis.histos.pidCalibration[fill]
            if run > 7000000:
                fname = run6_tree_dir + '/chargedPions_%d.tree.root' % run
            else:
                fname = run5_tree_dir + '/chargedPions_%d.tree.root' % run
                if not os.path.isfile(fname):
                    fname = run5_tree_dir + '-minbias/chargedPions_%d.tree.root' % run
            print fname
            f = ROOT.TFile(fname)
            for t in f.tree:
                ecuts.set(t.event)
                if not ecuts.all: continue
                for track in t.event.tracks():
                    tcuts.set(track)
                    if tcuts.eta and tcuts.dca and tcuts.fit:
                        ## recalibrate nSigmaPion
                        nSigmaPion = (track.nSigmaPion() - pidFit[0]) / pidFit[1]
                        nsig[0].Fill(nSigmaPion)
                        for i,ptcut in enumerate(ptbins):
                            if track.pt() > ptcut:
                                nsig[i+1].Fill(nSigmaPion)
                                break
    else:
        nsig = [ tfile.Get('nsig_%d' % i) for i in range(5) ]
    
    
    c = []
    outfile = ROOT.TFile('/tmp/pid_pt_dependence.root','recreate')
    for h in nsig:
        fits = pid_calibration(h)
        #fits = pid_calibration(h, useElectrons=(h.GetName() != 'nsig_1'))
        [ h.GetListOfFunctions().Add(f) for f in fits[1:] ]
        c.append(ROOT.TCanvas())
        h.Draw()
        h.Write()
        c[-1].Print('.png')
    
    raw_input('wait here:')
    outfile.Close()


def systematic_uncertainty_run5(charge='plus', key=None):
    """returns final bin-by-bin systematic uncertainties.  key can be one of (None)"""
    plus_all_meas = [
    analysis.DataPoint( y=-5.85e-03, stat=4.57e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y= 2.74e-02, stat=1.15e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y= 3.50e-03, stat=2.24e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-1.96e-02, stat=4.05e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    minus_all_meas = [
    analysis.DataPoint( y=-2.04e-03, stat=4.71e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y=-1.09e-03, stat=1.21e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y=-3.65e-02, stat=2.37e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y= 3.42e-03, stat=4.32e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    
    ## math.fabs(nSigmaPion) > 2
    plus_all_pid_bg = [
    analysis.DataPoint( y= 1.60e-02, stat=7.21e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y=-5.02e-03, stat=1.67e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y=-4.15e-02, stat=3.39e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-1.58e-02, stat=6.39e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    minus_all_pid_bg = [
    analysis.DataPoint( y= 6.63e-03, stat=7.08e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y=-6.25e-03, stat=1.71e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y= 3.39e-02, stat=3.80e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y= 1.69e-02, stat=7.74e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    
    ## math.fabs(nSigmaPion) > 2, 2006 data
    plus_all_pid_bg_2006 = [
    analysis.DataPoint( y= 1.47e-02, stat=5.85e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y=-4.32e-03, stat=1.09e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y= 9.59e-03, stat=1.90e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-1.01e-03, stat=3.11e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    minus_all_pid_bg_2006 = [
    analysis.DataPoint( y= 4.98e-03, stat=6.15e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y= 1.95e-02, stat=1.21e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y=-1.91e-02, stat=2.22e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-6.86e-03, stat=4.01e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    
    ## asigma from 2006 transverse runs, BJP1 triggers only
    plus_asigma = [
    analysis.DataPoint( y=-2.26e-03, stat=6.17e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y= 9.57e-03, stat=1.28e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y=-3.81e-02, stat=2.17e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-3.15e-02, stat=3.42e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    minus_asigma = [
    analysis.DataPoint( y= 3.29e-05, stat=6.39e-03, sys=0.00, x=3.0, xlow=2.0, binwidth=2.0 ),
    analysis.DataPoint( y=-4.37e-03, stat=1.37e-02, sys=0.00, x=5.0, xlow=4.0, binwidth=2.0 ),
    analysis.DataPoint( y=-3.53e-02, stat=2.37e-02, sys=0.00, x=7.0, xlow=6.0, binwidth=2.0 ),
    analysis.DataPoint( y=-2.44e-02, stat=3.85e-02, sys=0.00, x=9.0, xlow=8.0, binwidth=2.0 )
    ]
    
    pid_bg_frac = [0.10, 0.09, 0.10, 0.16]
    non_long_frac = [0.018, 0.018, 0.018, 0.018]
    
    ## quadrature sum of ZDC/BBC comparison and beam-gas background study
    relative_lumi_syst = math.sqrt(4.9e-04 ** 2 + 3.0e-04 ** 2)
    
    ## trigger bias
    plus_trigger_bias = [0.0, 0.0, 0.0, 0.0]
    minus_trigger_bias = [0.0, 0.0, 0.0, 0.0]
    
    if charge == 'plus':
        all_meas = plus_all_meas
        trigger_bias = plus_trigger_bias
        pid_bg = plus_all_pid_bg
        ## next line combines Run 5 and Run 6 b/g asymmetries
        [pbg.add(plus_all_pid_bg_2006[i]) for i,pbg in enumerate(pid_bg)]
        asigma = plus_asigma
    elif charge == 'minus':
        all_meas = minus_all_meas
        trigger_bias = minus_trigger_bias
        pid_bg = minus_all_pid_bg
        ## next line combines Run 5 and Run 6 b/g asymmetries
        [pbg.add(minus_all_pid_bg_2006[i]) for i,pbg in enumerate(pid_bg)]
        asigma = minus_asigma
    else:
        raise KeyError(charge)
    
    syst = []
    for i,datum in enumerate(all_meas):
        dpid = math.fabs(pid_bg[i].y - datum.y)
        err = math.sqrt(pid_bg[i].stat**2 + datum.stat**2)
        ##  use error on background if measurements are consistent
        if (pid_bg[i].stat > dpid): dpid = pid_bg[i].stat 
        
        ## correct equation is just A_sigma, not A_LL - A_sigma
        #dasigma = math.fabs(asigma[i].y - datum.y)
        dasigma = math.fabs(asigma[i].y)
        err = math.sqrt(asigma[i].stat**2 + datum.stat**2)
        ##  use error on background if measurements are consistent
        if (asigma[i].stat > dasigma): dasigma = asigma[i].stat
        
        tot = math.sqrt( trigger_bias[i]**2 + \
                         (dpid*pid_bg_frac[i])**2 + \
                         (dasigma*non_long_frac[i])**2 + \
                         (relative_lumi_syst)**2 \
                       )
        syst.append(tot)
        
        print 'Systematic Uncertainty for charge=%s, pT=%.1f' % (charge, datum.x)
        print 'trigger bias   = %.2e' % (trigger_bias[i])
        print 'pid background = %.3f * %.2e = %.2e' % (pid_bg_frac[i], dpid, pid_bg_frac[i]*dpid)
        print 'non-long beam  = %.3f * %.2e = %.2e' % (non_long_frac[i], dasigma, non_long_frac[i]*dasigma)
        print 'relative lumi  = %.2e' % relative_lumi_syst
        print 'Total: %e' % tot
        print '----------------------------------------------------'
    
    return syst
    


def dis2008_run6_projections():
    """using real 2006 data, plot projected statistical significance of Run 6 inclusive"""
    ROOT.gStyle.SetOptDate(0)
    asym_plus = analysis.AsymmetryGenerator('asym_plus')
    asym_minus = analysis.AsymmetryGenerator('asym_minus')
    
    runlist = analysis.long2_run6
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Final
    
    from analysis.asym import theoryCurves
    plusGraphs = [
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_std, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_zero, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_max, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_min, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_plus_dss_cteqm5_gsc, analysis.xsec.werner_plus_dss_cteqm5_pt).getGraph()
    ]
    minusGraphs = [
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_std, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_zero, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_max, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_min, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph(),    
    theoryCurves(analysis.asym.werner_minus_dss_cteqm5_gsc, analysis.xsec.werner_minus_dss_cteqm5_pt).getGraph()
    ]
    
    
    ## generate the asymmetries
    allFiles = glob(run6_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['pt'])
            
            try:
                bin6 = scalars[str(run) + '-5-6']
                bin7 = scalars[str(run) + '-5-7']
                bin8 = scalars[str(run) + '-5-8']
                bin9 = scalars[str(run) + '-5-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    ## fun with graphics
    h1 = asym_plus.GetAsymmetry('ll')
    [ h1.SetBinContent(i+1, 0.0) for i in range(4) ]
    g1 = ROOT.TGraphErrors(h1)
    h2 = asym_minus.GetAsymmetry('ll')
    [ h2.SetBinContent(i+1, 0.0) for i in range(4) ]
    g2 = ROOT.TGraphErrors(h2)
    
    for grList in (plusGraphs, minusGraphs):
        #grList[1].SetLineStyle(3)
        grList[1].SetLineColor(ROOT.kBlue)
        #grList[2].SetLineStyle(4)
        grList[2].SetLineColor(ROOT.kRed)
        #grList[3].SetLineStyle(2)
        grList[3].SetLineColor(ROOT.kGreen)
        #grList[4].SetLineStyle(5)
        grList[4].SetLineColor(ROOT.kMagenta)
        for gr in grList:
            gr.SetLineWidth(3)
    
    ## ignore bin width errors
    for gr in (g1,g2):
        for point in range(gr.GetN()):
            gr.SetPointError(point, 0.0, gr.GetErrorY(point))
    
    line = ROOT.TLine(2.0, 0.0, 10.0, 0.0)
    line.SetLineStyle(2)
    
    latex = ROOT.TLatex()
    
    leg = ROOT.TLegend(0.13, 0.65, 0.35, 0.88)
    leg.SetFillStyle(0)
    leg.SetBorderSize(0)
    leg.AddEntry(plusGraphs[0],' GRSV-STD', 'l')
    leg.AddEntry(plusGraphs[1],' #Delta G =  0', 'l')
    leg.AddEntry(plusGraphs[2],' #Delta G =  G', 'l')
    leg.AddEntry(plusGraphs[3],' #Delta G = -G', 'l')
    leg.AddEntry(plusGraphs[4],' GS Set C', 'l')
    
    bg = ROOT.TH1D(h1)
    bg.Reset()
    bg.SetYTitle(' A_{LL}')
    bg.GetYaxis().SetRangeUser(-0.11, 0.11)
    
    ## pi-plus
    c1 = ROOT.TCanvas('c1','A_{LL} for #pi^{+}', 1060, 800)
    bg.SetXTitle('#pi^{+} P_{T} (GeV/c)')
    bg.DrawCopy()
    g1.SetMarkerSize(0.9);
    g1.SetMarkerStyle(21)
    g1.Draw('p')
    [ g.Draw('l') for g in plusGraphs ]
    #systGraph['plus'].SetLineColor(1)
    #systGraph['plus'].SetFillColor(15)
    #systGraph['plus'].Draw('fl')
    line.Draw('same')
    leg.Draw('p')
    latex.DrawLatex(2.3,0.12," #vec{p} + #vec{p} #rightarrow #pi^{+} + X at #sqrt{s}=200 GeV \
                -1< #eta^{#pi}< 1 ")
    latex.DrawLatex(2.6,-0.07,"2006 STAR Projections");
    
    ## pi-minus
    c2 = ROOT.TCanvas('c2','A_{LL} for #pi^{-}', 1060, 800)
    bg.SetXTitle('#pi^{-} P_{T} (GeV/c)')
    bg.DrawCopy()
    g2.SetMarkerSize(0.9);
    g2.SetMarkerStyle(20)
    g2.Draw('p')
    [ g.Draw('l') for g in minusGraphs ]
    #systGraph['minus'].SetLineColor(1)
    #systGraph['minus'].SetFillColor(15)
    #systGraph['minus'].Draw('fl')
    line.Draw('same')
    leg.Draw('p')
    latex.DrawLatex(2.3,0.12," #vec{p} + #vec{p} #rightarrow #pi^{-} + X at #sqrt{s}=200 GeV \
                -1< #eta^{#pi}< 1 ")
    latex.DrawLatex(2.6,-0.07,"2006 STAR Projections")
    
    raw_input('wait here:')
    c1.Print('.gif')
    c2.Print('.gif')


def z_slope_run5(trig='jetpatch', runlist=None, charge=0):
    """docstring for z_slope_run5"""
    z_jet = None
    z_away_jet = None
    keepMeOpen = []
    allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    for fname in allFiles[:]:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['z_jet', 'z_away_jet'],(trig,))
            
            if z_jet is None:
                z_jet = mgr['anyspin'][trig].trackHistograms(charge)['z_jet'].Clone()
                z_away_jet = mgr['anyspin'][trig].trackHistograms(charge)['z_away_jet'].Clone()
                keepMeOpen.append(tfile)
            else:
                z_jet.Add(mgr['anyspin'][trig].trackHistograms(charge)['z_jet'])
                z_away_jet.Add(mgr['anyspin'][trig].trackHistograms(charge)['z_away_jet'])
    
    z_jet_pt = [z_jet.ProjectionY('z_jet_pt_%d' % (i+1,), i+1,i+1) \
        for i in range(z_jet.GetNbinsX())]
    z_away_jet_pt = [z_away_jet.ProjectionY('z_away_jet_pt_%d' % (i+1,), i+1,i+1) \
        for i in range(z_away_jet.GetNbinsX())]
    
    ps = ROOT.TPostScript('zslopes_%s.ps' % trig)
    c = ROOT.TCanvas('c','',100,100,600,800)
    c.Divide(3,4)
    
    if trig =='96233':
        threshold = 6.4
    elif trig == '96221':
        threshold = 4.5
    else:
        threshold = 0
    
    f = ROOT.gROOT.GetFunction('expo')
    f.SetLineWidth(2)
    f.SetLineStyle(2)
    
    for i,g in enumerate(z_jet_pt):
        title = '%.1f < p_{T,jet} < %.1f ' % (z_jet.GetBinLowEdge(i+1), z_jet.GetBinLowEdge(i+2))
        
        pad = c.cd(i+1)
        pad.SetTitle(title)
        ROOT.gPad.SetLogy()
        
        ## anything lower is restricted by jet pT cut
        fit_min = 2.0 / z_jet.GetBinLowEdge(i+1) 
        ## kinda arbitrary, just ensuring enough energy in BTOW to fire trigger
        fit_max = (z_jet.GetBinLowEdge(i+1) - threshold)/z_jet.GetBinLowEdge(i+1)
        
        if fit_max - fit_min < 0: fit_max += 0.5
        
        z_jet_pt[i].SetTitle(title)
        z_jet_pt[i].GetXaxis().SetTitle('z')
        z_jet_pt[i].Fit('expo','r', '', fit_min, fit_max)
        
        writer = ROOT.TLatex()
        writer.SetNDC()
        writer.DrawLatex(0.42, 0.95, 'slope %.2f #chi^{2}: %.1f/%d' % \
            (f.GetParameter(1), f.GetChisquare(), f.GetNDF()) )
        
        writer2 = ROOT.TLatex()
        writer2.SetNDC()
        writer2.SetTextColor(ROOT.kRed)
        
        z_away_jet_pt[i].SetMarkerColor(ROOT.kRed)
        z_away_jet_pt[i].SetLineColor(ROOT.kRed)
        z_away_jet_pt[i].Fit('expo', 'r', 'same', fit_min, fit_max)
        
        writer2.DrawLatex(0.42, 0.85, 'slope %.2f #chi^{2}: %.1f/%d' % \
            (f.GetParameter(1), f.GetChisquare(), f.GetNDF()) )
    
    raw_input('wait here:')
    ps.Close()


def away_side_asymmetries_run6(runlist):
    asym_plus = analysis.AsymmetryGenerator('asym_plus', key='away_lead_pt')
    asym_minus = analysis.AsymmetryGenerator('asym_minus', key='away_lead_pt')
    
    scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'
    scalars = analysis.ScalarCounts(scalar_path)
    
    polarizations = analysis.Polarizations.Final
    
    ## generate the asymmetries
    allFiles = glob(run6_hist_dir + '/chargedPions_*.hist.root')[:]
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,['away_lead_pt'])
            
            try:
                bin6 = scalars.get(str(run) + '-5-6') or scalars[str(run) + '-6-6']
                bin7 = scalars.get(str(run) + '-5-7') or scalars[str(run) + '-6-7']
                bin8 = scalars.get(str(run) + '-5-8') or scalars[str(run) + '-6-8']
                bin9 = scalars.get(str(run) + '-5-9') or scalars[str(run) + '-6-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin6.uu + bin7.uu + bin8.uu + bin9.uu
            ud = bin6.ud + bin7.ud + bin8.ud + bin9.ud
            du = bin6.du + bin7.du + bin8.du + bin9.du
            dd = bin6.dd + bin7.dd + bin8.dd + bin9.dd
            
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus.FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du,
                                               dd, pol.py, pol.pb)
            asym_minus.FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du,
                                                dd, pol.py, pol.pb)
            tfile.Close()
    
    line = ROOT.TLine(2.0, 0.0, 10.0, 0.0)
    line.SetLineStyle(2)
            
    c1 = ROOT.TCanvas()
    h1 = asym_plus.GetAsymmetry('ll')
    h1.GetYaxis().SetRangeUser(-0.1, 0.1)
    h1.SetTitle('Run 6 away-side A_{LL} BJP1 #pi^{+}')
    h1.SetXTitle('p_{T}')
    h1.SetMarkerStyle(21)
    h1.Draw('e1')
    line.Draw('same')
    
    c2 = ROOT.TCanvas()
    h2 = asym_minus.GetAsymmetry('ll')
    h2.GetYaxis().SetRangeUser(-0.1, 0.1)
    h2.SetTitle('Run 6 away-side A_{LL} BJP1 #pi^{-}')
    h2.SetXTitle('p_{T}')
    h2.SetMarkerStyle(20)
    h2.Draw('e1')
    line.Draw('same')
    
    raw_input('wait here:')


def ssa_by_run(asym_key='ly', runlist=analysis.final_runlist_run5, variable='pt', year=2006, bins=[1,2.0,10.0]):
    """generates canvas for asym_key (ly,lb,ls,us). Each data point is the SSA for a fill"""
    
    asym_plus = {}
    asym_minus = {}
    # if variable == 'jet_pt':
    #     bins = [1,5.0,50.0]
    # else:
    #     bins = [1,2.0,10.0]
    for f in runlist:
        asym_plus[f] = analysis.AsymmetryGenerator(name='R%s_plus' % f, bins=bins, key=variable, useR123=True)
        asym_minus[f] = analysis.AsymmetryGenerator(name='R%s_minus' % f, bins=bins, key=variable, useR123=True)
    
    if year == 2005:
        scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run5.txt'
        allFiles = glob(run5_hist_dir + '/chargedPions_*.hist.root')
    else:
        scalar_path = os.environ['STAR'] + '/StRoot/StSpinPool/StTamuRelLum/inputs/run6.txt'        
        allFiles = glob(run6_hist_dir + '/chargedPions_*.hist.root')
    
    scalars = analysis.ScalarCounts(scalar_path)
    polarizations = analysis.Polarizations.Final
    
    for fname in allFiles:
        run = analysis.getRun(fname)
        if runlist is None or run in runlist:
            print fname, run
            tfile = ROOT.TFile(fname)
            mgr = analysis.HistogramManager(tfile,(variable,))
            
            try:
                bin6 = scalars.get(str(run) + '-5-6') or scalars[str(run) + '-6-6']
                bin7 = scalars.get(str(run) + '-5-7') or scalars[str(run) + '-6-7']
                bin8 = scalars.get(str(run) + '-5-8') or scalars[str(run) + '-6-8']
                bin9 = scalars.get(str(run) + '-5-9') or scalars[str(run) + '-6-9']
            except KeyError:
                print run, 'is not in the scalars database'
                continue
            uu = bin7.uu + bin8.uu + bin9.uu
            ud = bin7.ud + bin8.ud + bin9.ud
            du = bin7.du + bin8.du + bin9.du
            dd = bin7.dd + bin8.dd + bin9.dd
            if year == 2006:
                uu += bin6.uu
                ud += bin6.ud
                du += bin6.du
                dd += bin6.dd
            try:
                pol = polarizations[bin7.fill]
            except KeyError:
                print bin7.fill, 'has no final polarization values'
                continue
            
            asym_plus[run].FillFromHistogramManager(mgr, 'jetpatch', 1, uu, ud, du, dd, pol.py, pol.pb)
            asym_minus[run].FillFromHistogramManager(mgr, 'jetpatch', -1, uu, ud, du, dd, pol.py, pol.pb)
            tfile.Close()
    
    title = {'ll': 'Double Spin', 'ly':'Yellow Beam', 'lb':'Blue Beam', 'ls':'Like-Sign', 'us':'Unlike-Sign'}
    final_hist_plus = ROOT.TH1D('final_hist_plus','#pi^{+} %s SSA' % title[asym_key], len(runlist), 0.5, len(runlist)+0.5)
    final_hist_minus = ROOT.TH1D('final_hist_minus','#pi^{-} %s SSA' % title[asym_key], len(runlist), 0.5, len(runlist)+0.5)
    sigma_plus = ROOT.TH1D('sigma_plus', '#pi^{+} %s SSA Deviation Per Run' % \
        title[asym_key], 50, -7.0, 7.0)
    sigma_minus = ROOT.TH1D('sigma_minus', '#pi^{-} %s SSA Deviation Per Run' % \
        title[asym_key], 50, -7.0, 7.0)
    if variable == 'jet_pt':
        [h.SetTitle('Jet %s SSA' % title[asym_key]) for h in (final_hist_minus,final_hist_plus)]
    marker_color = {'ll': ROOT.kBlack, 'ly':ROOT.kYellow, 'lb':ROOT.kBlue, 'ls':ROOT.kRed, 'us':ROOT.kBlack}
    for h in (final_hist_plus, final_hist_minus):
        h.SetMarkerColor( marker_color[asym_key] )
        h.GetYaxis().SetRangeUser(-0.2, 0.2)
        h.SetXTitle('run index')
    final_hist_plus.SetMarkerStyle(21)
    final_hist_minus.SetMarkerStyle(20)
    for h in (sigma_plus, sigma_minus):
        h.SetXTitle('n#sigma')
    for i,f in enumerate(runlist):
        hplus = asym_plus[f].GetAsymmetry(asym_key)
        try:
            sigma_plus.Fill(hplus.GetBinContent(1)/hplus.GetBinError(1))
            final_hist_plus.SetBinContent( i+1, hplus.GetBinContent(1) )
            final_hist_plus.SetBinError( i+1, hplus.GetBinError(1) )
            print '%d % .4f % .4f % .4f' % (f, hplus.GetBinContent(1), hplus.GetBinError(1), hplus.GetBinContent(1)/hplus.GetBinError(1))
        except ZeroDivisionError:
            print 'ACK', f, hplus.GetBinContent(1), hplus.GetBinError(1)
        
        hplus.Delete()
        
        hminus = asym_minus[f].GetAsymmetry(asym_key)
        try:
            sigma_minus.Fill(hminus.GetBinContent(1)/hminus.GetBinError(1))
            final_hist_minus.SetBinContent( i+1, hminus.GetBinContent(1) )
            final_hist_minus.SetBinError( i+1, hminus.GetBinError(1) )
        except ZeroDivisionError:
            print 'ACK', f, hminus.GetBinContent(1), hminus.GetBinError(1)
        
        hminus.Delete()
    
    ROOT.gStyle.SetOptStat('oume')
    ROOT.gStyle.SetOptFit(111)
    
    c1 = ROOT.TCanvas()
    sigma_minus.Fit('gaus')
    c2 = ROOT.TCanvas()
    sigma_plus.Fit('gaus')
    
    ROOT.gStyle.SetOptStat(0)
    ROOT.gStyle.SetErrorX(0.0)
    ROOT.gStyle.SetOptFit(111)
    
    cp = ROOT.TCanvas('%s_ssa_run_plus' % asym_key)
    final_hist_plus.Draw('e1')
    final_hist_plus.Fit('pol0')
    print ROOT.gROOT.GetFunction('pol0').GetProb()
    cm = ROOT.TCanvas('%s_ssa_run_minus' % asym_key)
    final_hist_minus.Draw('e1')
    final_hist_minus.Fit('pol0')
    print ROOT.gROOT.GetFunction('pol0').GetProb()
    raw_input('wait here')
    cp.Print('.png')
    cm.Print('.png')


def datamc(simuFile, dataDir, runlist, trigger):
    fsimu = ROOT.TFile(simuFile)
    simu = analysis.HistogramManager(fsimu)
    
    event_keys = ['vz', 'vzBBC', 'jet_pt', 'lead_neutral', 'inclusive_jet_mult',
        'dijet_mult']
    
    track_keys = ['pt', 'eta', 'phi', 'nHitsFit', 'dEdx', 'dcaG', 'nSigmaPion',
        'pt_near', 'pt_away', 'pt_bg', 
        'away_mult', 'near_mult', 'away_lead_pt', 'near_lead_pt',
        'lead_matched', 'lead_cutfail', 'z_away2', 'z_away3', 'z_away4',
        'away2_eta', 'away2_nHitsFit', 'away2_dcaG', 'vz', 'distortedPt']
    
    log_scale = ('lead_neutral', 'inclusive_jet_mult', 'dijet_mult', 'pt', 'dcaG',
        'pt_near', 'pt_away', 'pt_bg', 'away_mult', 'near_mult', 'away_lead_pt', 
        'near_lead_pt', 'lead_matched', 'lead_cutfail', 'z_away2', 'z_away3', 'z_away4',
        'away2_dcaG', 'jet_pt', 'distortedPt')
    
    ## normalize based on integrated luminosity in mb
    ## lumi for long2_run6, 2008-08-28: 5.43 pb^-1
    norm = 5.43E+09    
    ## lumi for final_runlist_run5, 2008-09-04: 2.11 pb^-1
    # norm = 2.11E+09
    print 'normalization factor for simulations: %.2E' % norm
    
    ## scale normalization down to PID efficiency (but neglecting contam)
    # track_norm = norm * 0.82
    
    ## if I include p/k/e contamination this factor is approximately
    track_norm = norm * 0.93
    
    rebin = ('vz', 'vzBBC', 'jet_pt')
    
    keepme = []
    for key in event_keys:
        c = analysis.graphics.canvas1e(key)
        cpad = c.cd(1)
        epad = c.cd(2)
        if key in log_scale: cpad.SetLogy()
        
        d = analysis.hadd_interactive(dataDir, runlist, trigger, 'anyspin', None, key)
        if d.GetTitle() == '': d.SetTitle(key)
        
        s = simu.anyspin[trigger][key]
        s.Scale(norm)
        s.SetLineColor(ROOT.kRed)
        s.SetMarkerColor(ROOT.kRed)
        
        if key in rebin: 
            d.Rebin()
            s.Rebin()
        
        cpad.cd()
        d.Draw('e')
        s.Draw('hist same')
        
        ## time for the ratios
        line = ROOT.TLine(d.GetBinLowEdge(1), 0.0, \
                          d.GetBinLowEdge(d.GetNbinsX()+1), 0.0)
        line.SetLineStyle(4)
        
        epad.cd()
        ratio = d.Clone()
        ratio.Add(s, -1.0)
        ratio.Divide(d)
        ratio.SetTitle('')
        ratio.GetYaxis().SetTitle('(data-simu)/data')
        ratio.GetYaxis().SetRangeUser(-1.0, 1.0)
        ratio.Draw()
        line.Draw('same')
        
        c.Update()
        
        keepme.extend([c,d,s])
    
    for key in track_keys:
        c = analysis.graphics.canvas3('track ' + key)
        mpad = c.cd(1)
        ppad = c.cd(2)
        empad = c.cd(3)
        eppad = c.cd(4)
        c2 = analysis.graphics.canvas1e('track sum ' + key)
        pad = c2.cd(1)
        epad = c2.cd(2)
        
        pads = (mpad, ppad, pad)
        if key in log_scale:
            [p.SetLogy() for p in pads]
        
        dkey = key=='distortedPt' and 'pt' or key
        
        da = analysis.hadd_interactive(dataDir, runlist, trigger, 'anyspin', 'sum', dkey)
        dm = analysis.hadd_interactive(dataDir, runlist, trigger, 'anyspin', 'minus', dkey)
        dp = analysis.hadd_interactive(dataDir, runlist, trigger, 'anyspin', 'plus', dkey)
        d2 = (da, dm, dp)
        da.SetTitle(key + ' for #pi^{-} + #pi^{+}')
        dm.SetTitle(key + ' for #pi^{-}')
        dp.SetTitle(key + ' for #pi^{+}')
        
        
        sa = simu.anyspin[trigger].tracks_sum[key]
        sm = simu.anyspin[trigger].tracks_minus[key]
        sp = simu.anyspin[trigger].tracks_plus[key]
        s2 = (sa, sm, sp)
        for s in s2:
            s.SetLineColor(ROOT.kRed)
            s.SetMarkerColor(ROOT.kRed)
            s.Scale(track_norm)
        
        if key in rebin:
            [h.Rebin() for h in d2+s2]
        
        pad.cd()
        da.Draw('e')
        sa.Draw('hist same')
        
        mpad.cd()
        dm.Draw('e')
        sm.Draw('hist same')
        
        ppad.cd()
        dp.Draw('e')
        sp.Draw('hist same')
        
        ## time for the ratios
        line = ROOT.TLine(dm.GetBinLowEdge(1), 0.0, \
                          dm.GetBinLowEdge(dm.GetNbinsX()+1), 0.0)
        line.SetLineStyle(4)
        
        epad.cd()
        ratio = da.Clone()
        ratio.Add(sa, -1.0)
        ratio.Divide(da)
        ratio.Draw()
        line.Draw('same')
        
        empad.cd()
        mratio = dm.Clone()
        mratio.Add(sm, -1.0)
        mratio.Divide(dm)
        mratio.Draw()
        line.Draw('same')
        
        eppad.cd()
        pratio = dp.Clone()
        pratio.Add(sp, -1.0)
        pratio.Divide(dp)
        pratio.Draw()
        line.Draw('same')
        
        ratios = (ratio, mratio, pratio)
        for h in ratios:
            h.SetTitle('')
            h.GetYaxis().SetTitle('(data-simu)/data')
            h.GetYaxis().SetRangeUser(-1.0, 1.0)
            
        c.Update()
        c2.Update()
        keepme.extend([c, da, dm, dp, sa, sm, sp, ratio, mratio, pratio])
    
    save = raw_input('save these histograms? (y/N): ')
    if save == 'y':
        for item in keepme:
            if item.ClassName() == 'TCanvas':
                item.Print(item.GetTitle().replace(' ', '_') + '.png')
    
    [o.Delete() for o in keepme]


def mcasym(fname, trigger='jetpatch', keys=None):
    f = ROOT.TFile(fname)
    keys = keys or ['STD','MAX','MIN','ZERO','GS_NLOC']
    print keys
    mgr = analysis.HistogramManager(f, keys = keys+['denom'])
    keepme = []
    line = ROOT.TLine(0,0,1,0)
    line.SetLineStyle(2)
    color = {
        'STD': ROOT.kBlack,
        'MAX': ROOT.kRed,
        'MIN': ROOT.kGreen,
        'ZERO': ROOT.kBlue,
        'GS_NLOC': ROOT.kMagenta
    }
    c = analysis.graphics.canvas2()
    for i,key in enumerate(keys):
        opt = i>0 and 'e2 same' or 'e2'
        c.cd(1)
        line.Draw()
        minus = mgr.anyspin[trigger].tracks_minus[key].Clone()
        # minus.GetXaxis().SetRangeUser(0.1, 0.8)
        minus.GetYaxis().SetRangeUser(-0.05,0.05)
        minus.SetLineColor(color[key])
        minus.SetFillColor(color[key])
        minus.SetTitle('#pi^{-}')
        minus.GetXaxis().SetTitle('z')
        minus.Draw(opt)
        c.cd(2)
        line.Draw()
        plus = mgr.anyspin[trigger].tracks_plus[key].Clone()
        # plus.GetXaxis().SetRangeUser(0.1, 0.8)
        plus.GetYaxis().SetRangeUser(-0.05,0.05)
        plus.SetLineColor(color[key])
        plus.SetFillColor(color[key])
        plus.SetTitle('#pi^{+}')
        plus.GetXaxis().SetTitle('z')
        plus.Draw(opt)
        keepme.extend([c,minus,plus])
    raw_input('wait:')
