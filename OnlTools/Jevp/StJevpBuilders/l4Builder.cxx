/**
 * This builder is for HLT online QA
 * Basic HLT plots[#0-47] include information of global/primary track, event,
 * EMC, ToF and HLT heavy-fragment. All of above are designed to show in
 * STAR run monitor. JPsi plots[#0-13] including J/Psi invariant mass,
 * two daughters(e) info, and corresponding plots for di-pion, di-muon, should be
 * saved into root files which then can be added later for expert run-by-run or
 * day-by-day check through webpage.
 *
 * Default mode contains Basic plots.
 * The directory of dE/dx theoretical curve should be pointed in inputPara.dat.
 * Essential Functions init/start/stop/event/main
 *
 * @todo Weird TheoDedx_K_neg value if set mininum P at 0.1(0.101 now)
 * @author Q-Y. Shou, Zhengqiao
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h>
#include <DAQ_L3/daq_l3.h>
#include <DAQ_L4/daq_l4.h>
#include <TStyle.h>
#include "TVector3.h"
#include <TLegend.h>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <TF1.h>
#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include "TLorentzVector.h"
#include <TProfile.h>
#include <math.h>
#include <rtsSystems.h>
#include <rtsLog.h>
#include "l4Builder.h"
#include <omp.h>
#include <TStopwatch.h>

using namespace std;

// #include "RTS/trg/include/trgDataDefs.h"
// #include <DAQ_TOF/daq_tof.h>
// #include <DAQ_HLT/daq_hlt.h>
// #include <DAQ_SC/daq_sc.h>
// #include "DAQ_TRG/daq_trg.h"
// #include "DAQ_READER/daq_det.h"
// #include "daqFormats.h"

static Double_t funcDedx_e_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_e_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_Pi_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_Pi_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_K_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_K_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_P_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_P_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_D_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_D_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_T_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_T_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_He3_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_He3_neg(Double_t *x, Double_t *par);
static Double_t funcDedx_He4_pos(Double_t *x, Double_t *par);
static Double_t funcDedx_He4_neg(Double_t *x, Double_t *par);
static double dedxMean[8][11901];
static int n = 11900;
static double Min = 0.1;
static double Max = 12.;
static double Bin = (Max - Min) / n; // 0.001

ClassImp(l4Builder);

void l4Builder::initialize(int argc, char *argv[])
{
	cout << "Initialization ...(Timing begin)" << endl;
	timer.Start();

	l4Builder me2;
	struct stat64 st2;
	sprintf(me2.Destindir_dat,"%s/HLT_paras","/a/l4jevp/client");   // /a/jevp/client
	if(stat64(me2.Destindir_dat, &st2) == 0) {
		LOG(DBG,"%s exist.\n", me2.Destindir_dat);
	} else {
		LOG(DBG,"%s does not exist. Create.\n", me2.Destindir_dat);
		if(mkdir(me2.Destindir_dat, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) != 0) LOG(DBG,"mkdir() error");
	}


	index = 0;
	LOG(DBG, "Index defined %d\n",index);

	runnumber = 0;
	pi = 3.1415927;
	twopi = 6.2831854;
	A = 0.3736;
	BeamX = 0;
	BeamY = 0;
	innerGainPara = -999;
	outerGainPara = -999;
	eventCounter = 0;

	sprintf(dEdxTheoDir, "/a/l4jevp/client/dedx");
	LOG(DBG, "Index now %d %s\n",index, dEdxTheoDir);	

	sprintf(dEdxMeanFiles[0], "%s/dedx_mean_Electron", dEdxTheoDir);
	sprintf(dEdxMeanFiles[1], "%s/dedx_mean_Pion", dEdxTheoDir);
	sprintf(dEdxMeanFiles[2], "%s/dedx_mean_Kaon", dEdxTheoDir);
	sprintf(dEdxMeanFiles[3], "%s/dedx_mean_Proton", dEdxTheoDir);
	sprintf(dEdxMeanFiles[4], "%s/dedx_mean_Deuteron", dEdxTheoDir);
	sprintf(dEdxMeanFiles[5], "%s/dedx_mean_Triton", dEdxTheoDir);
	sprintf(dEdxMeanFiles[6], "%s/dedx_mean_He3", dEdxTheoDir);
	sprintf(dEdxMeanFiles[7], "%s/dedx_mean_He4", dEdxTheoDir);


	inputDedx();


	fTheoDedx_e_pos   = new TF1("TheoDedx_e_pos", funcDedx_e_pos, 0.1, 5., 0);
	fTheoDedx_e_neg   = new TF1("TheoDedx_e_neg", funcDedx_e_neg, -5., -0.1, 0);
	fTheoDedx_Pi_pos  = new TF1("TheoDedx_Pi_pos", funcDedx_Pi_pos, 0.1, 5., 0);
	fTheoDedx_Pi_neg  = new TF1("TheoDedx_Pi_neg", funcDedx_Pi_neg, -5., -0.1, 0);
	fTheoDedx_K_pos   = new TF1("TheoDedx_K_pos", funcDedx_K_pos, 0.101, 5., 0);
	fTheoDedx_K_neg   = new TF1("TheoDedx_K_neg", funcDedx_K_neg, -5., -0.101, 0);
	fTheoDedx_P_pos   = new TF1("TheoDedx_P_pos", funcDedx_P_pos, 0.1, 5., 0);
	fTheoDedx_P_neg   = new TF1("TheoDedx_P_neg", funcDedx_P_neg, -5., -0.1, 0);
	fTheoDedx_D_pos   = new TF1("TheoDedx_D_pos", funcDedx_D_pos, 0.1, 5., 0);
	fTheoDedx_D_neg   = new TF1("TheoDedx_D_neg", funcDedx_D_neg, -5., -0.1, 0);
	fTheoDedx_T_pos   = new TF1("TheoDedx_T_pos", funcDedx_T_pos, 0.1, 5., 0);
	fTheoDedx_T_neg   = new TF1("TheoDedx_T_neg", funcDedx_T_neg, -5., -0.1, 0);
	fTheoDedx_He3_pos = new TF1("TheoDedx_He3_pos", funcDedx_He3_pos, 0.1, 5., 0);
	fTheoDedx_He3_neg = new TF1("TheoDedx_He3_neg", funcDedx_He3_neg, -5., -0.1, 0);
	fTheoDedx_He4_pos = new TF1("TheoDedx_He4_pos", funcDedx_He4_pos, 0.1, 5., 0);
	fTheoDedx_He4_neg = new TF1("TheoDedx_He4_neg", funcDedx_He4_neg, -5., -0.1, 0);
	fTheoDedx_e_pos->SetLineWidth(0.3);
	fTheoDedx_e_neg->SetLineWidth(0.3);
	fTheoDedx_Pi_pos->SetLineWidth(0.3);
	fTheoDedx_Pi_neg->SetLineWidth(0.3);
	fTheoDedx_K_pos->SetLineWidth(0.3);
	fTheoDedx_K_neg->SetLineWidth(0.3);
	fTheoDedx_P_pos->SetLineWidth(0.3);
	fTheoDedx_P_neg->SetLineWidth(0.3);
	fTheoDedx_D_pos->SetLineWidth(0.3);
	fTheoDedx_D_neg->SetLineWidth(0.3);
	fTheoDedx_T_pos->SetLineWidth(0.3);
	fTheoDedx_T_neg->SetLineWidth(0.3);
	fTheoDedx_He3_pos->SetLineWidth(0.3);
	fTheoDedx_He3_neg->SetLineWidth(0.3);
	fTheoDedx_He4_pos->SetLineWidth(0.3);
	fTheoDedx_He4_neg->SetLineWidth(0.3);

	// Initialize JevpPlot
	gStyle->SetPalette(1);
	gStyle->SetOptLogz(1);
	gStyle->SetPadGridX(0);
	gStyle->SetPadGridY(0);

	for(int i = 0; i < 39; i++) {
		HltPlots[i] = new JevpPlot();
		HltPlots[i]->gridx = 0;
		HltPlots[i]->gridy = 0;
		HltPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 3; i++) {                                                                     
		BeamPlots[i] = new JevpPlot();                                                                 
		BeamPlots[i]->gridx = 0;                                                                       
		BeamPlots[i]->gridy = 0;                                                                       
		BeamPlots[i]->setPalette(1);                                                                   
	}    
	for(int i = 0; i < 5; i++) {
		BesGoodPlots[i] = new JevpPlot();
		BesGoodPlots[i]->gridx = 0;
		BesGoodPlots[i]->gridy = 0;
		BesGoodPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 2; i++) {
		BesMontinorPlots[i] = new JevpPlot();
		BesMontinorPlots[i]->gridx = 0;
		BesMontinorPlots[i]->gridy = 0;
		BesMontinorPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 4; i++) {
		HLTGood2Plots[i] = new JevpPlot();
		HLTGood2Plots[i]->gridx = 0;
		HLTGood2Plots[i]->gridy = 0;
		HLTGood2Plots[i]->setPalette(1);
	}
	for(int i = 0; i < 5; i++) {
		FixedTargetPlots[i] = new JevpPlot();
		FixedTargetPlots[i]->gridx = 0;
		FixedTargetPlots[i]->gridy = 0;
		FixedTargetPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 5; i++) {
		FixedTargetMonitorPlots[i] = new JevpPlot();
		FixedTargetMonitorPlots[i]->gridx = 0;
		FixedTargetMonitorPlots[i]->gridy = 0;
		FixedTargetMonitorPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 1; i++) {
		HeavyFragmentPlots[i] = new JevpPlot();
		HeavyFragmentPlots[i]->gridx = 0;
		HeavyFragmentPlots[i]->gridy = 0;
		HeavyFragmentPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 10; i++) {
		DiElectronPlots[i] = new JevpPlot();
		DiElectronPlots[i]->gridx = 0;
		DiElectronPlots[i]->gridy = 0;
		DiElectronPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 10; i++) {
		DiElectron2TwrPlots[i] = new JevpPlot();
		DiElectron2TwrPlots[i]->gridx = 0;
		DiElectron2TwrPlots[i]->gridy = 0;
		DiElectron2TwrPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 2; i++) {
		DiPionPlots[i] = new JevpPlot();
		DiPionPlots[i]->gridx = 0;     
		DiPionPlots[i]->gridy = 0;     
		DiPionPlots[i]->setPalette(1); 
	}
	for(int i = 0; i < 14; i++) {
		DiMuonPlots[i] = new JevpPlot();
		DiMuonPlots[i]->gridx = 0;         
		DiMuonPlots[i]->gridy = 0;         
		DiMuonPlots[i]->setPalette(1); 
	}
	for(int i = 0; i < 6; i++) {
		UPCDiElectronPlots[i] = new JevpPlot();
		UPCDiElectronPlots[i]->gridx = 0;
		UPCDiElectronPlots[i]->gridy = 0;
		UPCDiElectronPlots[i]->setPalette(1);
	}
	for(int i = 0; i < 30; i++) {
		HltPlots_UPC[i] = new JevpPlot();
		HltPlots_UPC[i]->gridx = 0;
		HltPlots_UPC[i]->gridy = 0;
		HltPlots_UPC[i]->setPalette(1);
	}

	cout << "HltPlots OK" << endl;
	defineHltPlots();
	defineBeamPlots();
	defineBesGoodPlots();
	defineHLTGood2Plots();
	defineBesMontinorPlots();
	defineFixedTargetPlots();
	defineFixedTargetMonitorPlots();
	defineHeavyFragmentPlots();
	defineDiElectronPlots();
	defineDiPionPlots();
	defineDiMuonPlots();
	defineUPCDiElectronPlots();
	defineHltPlots_UPC();
	defineDiElectron2TwrPlots();
	setAllPlots();
	for(int i = 0; i < 39; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(HltPlots[i]);
	}

	for(int i = 0; i < 4; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(HLTGood2Plots[i]);
	}
	for(int i=0;i<5;i++) {
	    addPlot(BesGoodPlots[i]);
	}

	for(int i = 0; i < 1; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(HeavyFragmentPlots[i]);
	}
	for(int i = 0; i < 10; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(DiElectronPlots[i]);
	}
	for(int i = 0; i < 10; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(DiElectron2TwrPlots[i]);
	}
	for(int i = 0; i < 14; i++) {
		LOG(DBG, "Adding plot %d", i);
		addPlot(DiMuonPlots[i]);
	}

	cout << "Initialization Done" << endl;
}

void l4Builder::startrun(daqReader *rdr)
{
    //printf("hello there. This is startrun\n");
	runnumber = rdr->run;

	int initialno = 39;
	for(int i = 0; i < initialno; i++) {
		getPlotByIndex(i)->getHisto(0)->histo->Reset();
	}
	for(int i = 0; i < 5; i++)BesGoodPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 4; i++)HLTGood2Plots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 2; i++)BesMontinorPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 5; i++)FixedTargetPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 5; i++)FixedTargetMonitorPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 1; i++){
		for(int i = 3; i < 10; i++)DiElectronPlots[i]->getHisto(0)->histo->Reset();
		for(int i = 0; i < 3; i++)
		{
			DiElectronPlots[i]->getHisto(0)->histo->Reset();
			DiElectronPlots[i]->getHisto(1)->histo->Reset();
		}
	}
	for(int i = 0; i < 1; i++){
		for(int i = 3; i < 10; i++)DiElectron2TwrPlots[i]->getHisto(0)->histo->Reset();
		for(int i = 0; i < 3; i++)
		{
			DiElectron2TwrPlots[i]->getHisto(0)->histo->Reset();
			DiElectron2TwrPlots[i]->getHisto(1)->histo->Reset();
		}
	}
	for(int i = 0; i < 2; i++)DiPionPlots[i]->getHisto(0)->histo->Reset();


	for(int i = 0; i < 1; i++){
		DiMuonPlots[i]->getHisto(0)->histo->Reset();
		DiMuonPlots[i]->getHisto(1)->histo->Reset();
	}
	for(int i = 1; i < 7; i++)DiMuonPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 7; i < 14; i++){
		DiMuonPlots[i]->getHisto(0)->histo->Reset();
		DiMuonPlots[i]->getHisto(1)->histo->Reset();
	}
	for(int i = 0; i < 6; i++)UPCDiElectronPlots[i]->getHisto(0)->histo->Reset();
	for(int i = 0; i < 30; i++)HltPlots_UPC[i]->getHisto(0)->histo->Reset();

	TriggerFilled = false;
	EventFilled = false;
	GlobalTracksFilled = false;
	PrimaryTracksFilled = false;
	EMCFilled = false;
	TOFFilled = false;
	BESGoodFilled = false;
	HLTGood2Filled = false;
	BESMonitorFilled = false;
	FixedTargetFilled = false;
	FixedTargetMonitorFilled = false;
	DiElectronFilled = false;
	UPCFilled = false;
	DiMuonFilled = false;
	UPCDiElectronFilled = false;
	HeavyFragmentFilled = false;
	MTDQuarkoniumFilled = false;
	DiElectron2TwrFilled = false;


	//printf("Starting run #%d\n", runnumber);
};

void l4Builder::stoprun(daqReader *rdr)
{

        LOG(WARN, "Number of events processed in daq file = %d\n", eventCounter);
	//**********add parallel copy**********
	hMatchId_fiberId->Add(hMatchId_fiberId_copy);
	hMatchId_fiberId->Add(hMatchId_fiberId_copy2);

	gStyle->SetOptStat(000000);
	gStyle->SetStatW(0.13);
	gStyle->SetStatH(0.08);
	gStyle->SetOptFit(111);

	hDiElectronInvMassTpxEmc->SetLineColor(4);
	hDiElectronInvMassFullRange->SetLineColor(4);
	hDiElectronInvMassCut->SetLineColor(4);
	hDiElectronInvMassFullRange_UPC->SetLineColor(4);
	hDiPionInvMassFullRange->SetLineColor(4);
	hInvMassUS->SetLineColor(4);

	hDiElectronInvMassTpxEmc_Twr->SetLineColor(4);
	hDiElectronInvMassFullRange_Twr->SetLineColor(4);
	hDiElectronInvMassCut_Twr->SetLineColor(4);

	hMTDDiMuonJpsiMassUS->SetMarkerStyle(20);
	hMTDDiMuonJpsiMassUS->SetMarkerColor(1);
	hMTDDiMuonJpsiMassUS->SetLineColor(1);
	hMTDDiMuonJpsiMassLS->SetLineColor(4);

	hMTDQmInvMassUS->SetMarkerStyle(20);
	hMTDQmInvMassUS->SetMarkerColor(1);
	hMTDQmInvMassUS->SetLineColor(1);
	hMTDQmInvMassLS->SetLineColor(4);

	hMTDQmJpsiMass_ptcut0_US->SetMarkerStyle(20);
	hMTDQmJpsiMass_ptcut0_US->SetMarkerColor(1);
	hMTDQmJpsiMass_ptcut0_US->SetLineColor(1);
	hMTDQmJpsiMass_ptcut0_LS->SetLineColor(4);

	hMTDQmJpsiMass_ptcut2_US->SetMarkerStyle(20);
	hMTDQmJpsiMass_ptcut2_US->SetMarkerColor(1);
	hMTDQmJpsiMass_ptcut2_US->SetLineColor(1);
	hMTDQmJpsiMass_ptcut2_LS->SetLineColor(4);


	hMTDQmJpsiMass_ptcut4_US->SetMarkerStyle(20);
	hMTDQmJpsiMass_ptcut4_US->SetMarkerColor(1);
	hMTDQmJpsiMass_ptcut4_US->SetLineColor(1);
	hMTDQmJpsiMass_ptcut4_LS->SetLineColor(4);

	hMTDQmUpsilonMassUS->SetMarkerStyle(20);
	hMTDQmUpsilonMassUS->SetMarkerColor(1);
	hMTDQmUpsilonMassUS->SetLineColor(1);
	hMTDQmUpsilonMassLS->SetLineColor(4);

	float low = -13.12;
	float high = -12.8;
	TF1 *fit = new TF1("fit", "gaus", low, high);
	fit->SetParName(0, "Apt");
	fit->SetParName(1, "Pos");
	fit->SetParName(2, "Sig");
	fit->SetParameter(1, -12.92);
	fit->SetParameter(2, 0.08);
	hLn_dEdx->Fit(fit, "EMR");

	TF1 *fit_UPC = new TF1("fit_UPC", "gaus", low, high);
	fit_UPC->SetParName(0, "Apt");
	fit_UPC->SetParName(1, "Pos");
	fit_UPC->SetParName(2, "Sig");
	fit_UPC->SetParameter(1, -12.92);
	fit_UPC->SetParameter(2, 0.08);
	hLn_dEdx_UPC->Fit(fit_UPC, "EMR");

	TF1 *func = new TF1("func", "gaus", -6., 6.);
	func->SetParName(0, "Apt");
	func->SetParName(1, "Mean");
	func->SetParName(2, "Sigma");
	func->SetParameter(1, 0.);
	func->SetParameter(2, 0.4);

	int maxBin = hDcaXy->GetMaximumBin();
	double maxVal = -6. + 0.1 * maxBin;
	hDcaXy->Fit(func, "EMR", "", maxVal - 1.8, maxVal + 1.8);

	double meanpar = func->GetParameter(1);
	int maxBin_UPC = hDcaXy_UPC->GetMaximumBin();
	double maxVal_UPC = -6. + 0.1 * maxBin_UPC;
	hDcaXy_UPC->Fit(func, "EMR", "", maxVal_UPC - 1.8, maxVal_UPC + 1.8);

	char OutParas[256];
	sprintf(OutParas, "%s/HLT_paras/%d.dat", clientdatadir, runnumber);//qiao
	ofstream outstream;
	outstream.open(OutParas);

	if(outstream.fail()) {
	    LOG(ERR, "Open failed for file %s!", OutParas);
	}

	outstream << "beamX" << "    " << BeamX << endl;
	outstream << "beamY" << "    " << BeamY << endl;
	outstream << "innerGain" << "    " << innerGainPara << endl;
	outstream << "outerGain" << "    " << outerGainPara << endl;
	outstream << "dcaXy" << "    " << meanpar << endl;

	if(outstream.fail()) {
		LOG(ERR, "Writing failed for file %s!", OutParas);
	}

	outstream.close();

	int tmpRunNum = runnumber;
	char inum[256];
	char label[256];
	ifstream indata;
	string paraname;
	int icount = 100;
	for(int i = 0; i < 20000; i++) {
		sprintf(inum, "%s/%i.dat", "/a/l4jevp/client/HLT_paras", tmpRunNum);//qiao
		sprintf(label, "%i", tmpRunNum);
		tmpRunNum--;

		double beamX;
		double beamY;
		double innerGain;
		double outerGain;
		double dcaXy;
		indata.open(inum);
		if(indata.good()) {
			while(!indata.eof()) {
				indata >> paraname;
				if(paraname == "beamX") indata >> beamX;
				if(paraname == "beamY") indata >> beamY;
				if(paraname == "innerGain") indata >> innerGain;
				if(paraname == "outerGain") indata >> outerGain;
				if(paraname == "dcaXy") indata >> dcaXy;
			}
		} else {
			continue;
		}
		indata.close();
		indata.clear();

		hBeamX->SetBinContent(icount, beamX);
		hBeamY->SetBinContent(icount, beamY);
		if(innerGain > 0) hInnerGain->SetBinContent(icount, innerGain);
		if(outerGain > 0) hOuterGain->SetBinContent(icount, outerGain);
		hMeanDcaXy->SetBinContent(icount, dcaXy);
		if((icount - 1) % 5 == 0) {
			hBeamX->GetXaxis()->SetBinLabel(icount, label);
			hBeamY->GetXaxis()->SetBinLabel(icount, label);
			hInnerGain->GetXaxis()->SetBinLabel(icount, label);
			hOuterGain->GetXaxis()->SetBinLabel(icount, label);
			hMeanDcaXy->GetXaxis()->SetBinLabel(icount, label);

			hBeamX->GetXaxis()->LabelsOption("d");
			hInnerGain->GetXaxis()->LabelsOption("d");
			hMeanDcaXy->GetXaxis()->LabelsOption("d");
		}
		icount--;
		if(icount <= 0) break;
	}

	double lowestB;
	double highestB;
	double lowestBeamX = hBeamX->GetBinContent(hBeamX->GetMinimumBin());
	double lowestBeamY = hBeamY->GetBinContent(hBeamY->GetMinimumBin());
	double highestBeamX = hBeamX->GetBinContent(hBeamX->GetMaximumBin());
	double highestBeamY = hBeamY->GetBinContent(hBeamY->GetMaximumBin());
	if(lowestBeamX > lowestBeamY) lowestB = lowestBeamY;
	else lowestB = lowestBeamX;
	if(highestBeamX > highestBeamY) highestB = highestBeamX;
	else highestB = highestBeamY;
	if(lowestB < 0) lowestB = lowestB * 1.4;
	else lowestB = lowestB * 0.8;
	if(highestB < 0) highestB = highestB * 0.8;
	else highestB = highestB * 1.4;
	hBeamX->GetYaxis()->SetRangeUser(lowestB, highestB);
	hBeamY->GetYaxis()->SetRangeUser(lowestB, highestB);
	double lowestG;
	double highestG;
	double lowestInner = hInnerGain->GetBinContent(hInnerGain->GetMinimumBin());
	double lowestOuter = hOuterGain->GetBinContent(hOuterGain->GetMinimumBin());
	double highestInner = hInnerGain->GetBinContent(hInnerGain->GetMaximumBin());
	double highestOuter = hOuterGain->GetBinContent(hOuterGain->GetMaximumBin());
	if(lowestInner > lowestOuter) lowestG = lowestOuter;
	else lowestG = lowestInner;
	if(highestInner > highestOuter) highestG = highestInner;
	else highestG = highestOuter;
	lowestG = lowestG * 0.6;
	highestG = highestG * 1.4;
	hInnerGain->GetYaxis()->SetRangeUser(lowestG, highestG);
	hOuterGain->GetYaxis()->SetRangeUser(lowestG, highestG);
	double lowestD;
	double highestD;
	lowestD = hMeanDcaXy->GetBinContent(hMeanDcaXy->GetMinimumBin());
	highestD = hMeanDcaXy->GetBinContent(hMeanDcaXy->GetMaximumBin());
	if(lowestD < 0) lowestD = lowestD * 1.2;
	else lowestD = lowestD * 0.8;
	if(highestD < 0) highestD = highestD * 0.8;
	else highestD = highestD * 1.2;
	hMeanDcaXy->GetYaxis()->SetRangeUser(lowestD, highestD);

	writeHistogram();
	timer.Stop();
	printf("Stopping run #%d\n", runnumber);
	cout << "Timing end. " << "\n" << "Cpu time: " << timer.CpuTime()
		<< " Real time: " << timer.RealTime() << endl;

};

void l4Builder::writeHistogram()
{
	char histfile[256];
	sprintf(histfile, "%s/run14_hlt_%d_current_hist.root", Destindir, runnumber);
	TFile file(histfile, "RECREATE");
	int initialno = 39;

	for(int i = 0; i < initialno; i++) getPlotByIndex(i)->getHisto(0)->histo->Write();
	if(BESGoodFilled){
		for(int i = 0; i < 5; i++)BesGoodPlots[i]->getHisto(0)->histo->Write();
	}
	if(HLTGood2Filled){
		for(int i = 0; i < 4; i++)HLTGood2Plots[i]->getHisto(0)->histo->Write();
	}
	if(BESMonitorFilled){
		for(int i = 0; i < 2; i++)BesMontinorPlots[i]->getHisto(0)->histo->Write();
	}
	if(FixedTargetFilled){
		for(int i = 0; i < 5; i++)FixedTargetPlots[i]->getHisto(0)->histo->Write();
	}
	if(FixedTargetMonitorFilled){
		for(int i = 0; i < 5; i++)FixedTargetMonitorPlots[i]->getHisto(0)->histo->Write();
	}
	if(HeavyFragmentFilled) {
		for(int i = 0; i < 1; i++){
			HeavyFragmentPlots[i]->getHisto(0)->histo->Write();
		}
	}
	if(DiElectronFilled){
		for(int i = 3; i < 10; i++)DiElectronPlots[i]->getHisto(0)->histo->Write();
		for(int i = 0; i < 3; i++)
		{
			DiElectronPlots[i]->getHisto(0)->histo->Write();
			DiElectronPlots[i]->getHisto(1)->histo->Write();
		}
	}
	if(DiElectron2TwrFilled){
		for(int i = 3; i < 10; i++)DiElectron2TwrPlots[i]->getHisto(0)->histo->Write();
		for(int i = 0; i < 3; i++)
		{
			DiElectron2TwrPlots[i]->getHisto(0)->histo->Write();
			DiElectron2TwrPlots[i]->getHisto(1)->histo->Write();
		}
	}
	if(UPCFilled){
		for(int i = 0; i < 2; i++)DiPionPlots[i]->getHisto(0)->histo->Write();
	}
	if(DiMuonFilled){
		for(int i = 0; i < 1; i++)
		{
			DiMuonPlots[i]->getHisto(0)->histo->Write();
			DiMuonPlots[i]->getHisto(1)->histo->Write();
		}
		for(int i = 1; i < 7; i++)DiMuonPlots[i]->getHisto(0)->histo->Write();
		for(int i = 7; i < 14; i++)
		{
			DiMuonPlots[i]->getHisto(0)->histo->Write();
			DiMuonPlots[i]->getHisto(1)->histo->Write();
		}
	}
	if(UPCDiElectronFilled){
		for(int i = 0; i < 6; i++)UPCDiElectronPlots[i]->getHisto(0)->histo->Write();
	}
	if(UPCFilled){
		for(int i = 0; i < 30; i++)HltPlots_UPC[i]->getHisto(0)->histo->Write();
	}
	file.Close();
}

void l4Builder::main(int argc, char *argv[])
{
	l4Builder me;
	//-------------------
	getcwd(me.Currentdir, 256);
	char tmp[256];
	getcwd(tmp, 256);
	strcpy(me.Currentdir, tmp);

	struct stat64 st;
	sprintf(me.Destindir, "%s/output", me.Currentdir);//qiao
	if(stat64(me.Destindir, &st) == 0) {
		printf("%s exist.\n", me.Destindir);
	} else {
		printf("%s does not exist. Create.\n", me.Destindir);
		if(mkdir(me.Destindir, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) != 0) perror("mkdir() error");
	}
	/*    struct stat64 st2;
	      sprintf(me.Destindir_dat,"%s/HLT_paras","/a/l4jevp/client");   // /a/jevp/client
	      if(stat64(me.Destindir_dat, &st2) == 0) {
	      printf("%s exist.\n", me.Destindir_dat);
	      } else {
	      printf("%s does not exist. Create.\n", me.Destindir_dat);
	      if(mkdir(me.Destindir_dat, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH) != 0) perror("mkdir() error");
	      }*/

	//-------------------
	me.Main(argc, argv);
};


void l4Builder::event(daqReader *rdr)
{

	//   //************************************** SET THE TRIGGER BIT HERE to min bias value *************
	//   //We want all events right now (not just min-bias), min-bias is our main trigger.
	//   u_int trg = rdr->daqbits;
	//   //int minbias = 0x20;
	//   int vpdtag = 0x200;
	//   FILL_VPD_HISTOS = kFALSE;
	//   if (trg & vpdtag) FILL_VPD_HISTOS = kTRUE;
	//   else FILL_VPD_HISTOS = kFALSE;
	//   //if (trg & minbias)  //start of check for minbias.
	//   //***********************************************************************************************

	int triggerBitHighPt = 0x10000;
	int triggerBitDiElectron = 0x20000;
	int triggerBitHeavyFragment = 0x40000;
	int triggerBitBesgoodEvents = 0x200000;
	int triggerBitLowMult = 0x1000000;
	int triggerBitUPC = 0x4000000;
	int triggerBitUPCDiElectron = 0x2000000;
	int triggerBitDiMuon = 0x8000000;
	int triggerBitFixedTarget = 0x10000000;
	int triggerBitFixedTargetMonitor = 0x20000000;
	int triggerBitBesMonitor = 0x40000000;
	unsigned int triggerBitHLTGood2 = 0x80000000;
	unsigned int triggerBitDiElectron2Twr     = 0x00000001; // start to up lower 16 bit.
	unsigned int triggerBitMTDQuarkonium   =   0x00400000;

	//EXTRACT L4 TRACK INFO FROM DAQ FILE
	//daq_dta *dd = rdr->det("l3")->get("legacy");
	//daq_dta *dd = rdr->det("hlt")->get("gl3");
	daq_dta *dd = rdr->det("l4")->get("gl3");
	daq_dta *ddTof  = rdr->det("trg")->get("raw");
	int daqID = rdr->daqbits;

	if(!dd) {
		LOG(DBG, "No HLT in this event");
		return;
	}
	eventCounter++;

	HLT_EVE *hlt_eve;
	HLT_TOF *hlt_tof;
	HLT_PVPD *hlt_pvpd;
	HLT_EMC *hlt_emc;
	HLT_GT *hlt_gt;
	HLT_RHO *hlt_dipi;
	HLT_DIEP *hlt_upcdiep;
	HLT_PT *hlt_pt;
	HLT_NODE *hlt_node;
	HLT_HIPT *hlt_hipt;
	HLT_DIEP *hlt_diep;
	HLT_DIEP *hlt_Twrdiep;
	HLT_HF *hlt_hf;
	HLT_MTD *hlt_mtd;
	HLT_MTDQuarkonium *hlt_mtdqm;
	while(dd && dd->iterate()) {
		hlt_gl3_t *hlt = (hlt_gl3_t *) dd->Void;

		if(strcmp(hlt->name, "HLT_EVE") == 0) hlt_eve = (HLT_EVE *)hlt->data;
		else if(strcmp(hlt->name, "HLT_TOF") == 0) hlt_tof = (HLT_TOF *)hlt->data;
		else if(strcmp(hlt->name, "HLT_PVPD") == 0) hlt_pvpd = (HLT_PVPD *)hlt->data;
		else if(strcmp(hlt->name, "HLT_EMC") == 0) hlt_emc = (HLT_EMC *)hlt->data;
		else if(strcmp(hlt->name, "HLT_GT") == 0) hlt_gt = (HLT_GT *)hlt->data;
		else if(strcmp(hlt->name, "HLT_PT") == 0) hlt_pt = (HLT_PT *)hlt->data;
		else if(strcmp(hlt->name, "HLT_NODE") == 0) hlt_node = (HLT_NODE *)hlt->data;
		else if(strcmp(hlt->name, "HLT_HIPT") == 0) hlt_hipt = (HLT_HIPT *)hlt->data;
		else if(strcmp(hlt->name, "HLT_DIEP") == 0) hlt_diep = (HLT_DIEP *)hlt->data;
		else if(strcmp(hlt->name, "HLT_HF") == 0) hlt_hf = (HLT_HF *)hlt->data;
		else if(strcmp(hlt->name, "HLT_UPCRHO") == 0) hlt_dipi = (HLT_RHO *)hlt->data;
		else if(strcmp(hlt->name, "HLT_UPCDIEP") == 0) hlt_upcdiep = (HLT_DIEP *)hlt->data;
		else if(strcmp(hlt->name, "HLT_MTDDIMU") == 0) hlt_mtd = (HLT_MTD *)hlt->data;
		else if(strcmp(hlt->name, "HLT_MTDQuarkonium") == 0) hlt_mtdqm = (HLT_MTDQuarkonium *)hlt->data;
		else if(strcmp(hlt->name, "HLT_DIEP2Twr") == 0) hlt_Twrdiep = (HLT_DIEP *)hlt->data;
	}
	// Check Version
	if(hlt_eve->version != HLT_GL3_VERSION) {
		LOG(DBG, "ERROR: HLTFormats version doesn't match DAQ file version!");
		cout << "DAQ version is " << hlt_eve->version << " while HLTFormats version is " << HLT_GL3_VERSION << endl;
		return;
	}

	unsigned int decision = hlt_eve->hltDecision;

	omp_set_nested(1);
	omp_set_dynamic(0);
#pragma omp parallel sections num_threads(19)
	{

#pragma omp section
	  {

	    if(!TriggerFilled) {
		TriggerFilled = true;
		addServerTags("L4Trigger");
	    }
	    if(decision & triggerBitDiMuon) {
	      hEvtsAccpt->Fill(0.);
	    }
	    if(decision & triggerBitMTDQuarkonium) {
	      hEvtsAccpt->Fill(1.);
	    }
	    if(decision & triggerBitHLTGood2) {
	      hEvtsAccpt->Fill(2.);
	    }
	    if(decision & triggerBitDiElectron) {
	      hEvtsAccpt->Fill(3.);
	    }
	    if(decision & triggerBitDiElectron2Twr) {
	      hEvtsAccpt->Fill(4.);
	    }
	    if(decision & triggerBitHeavyFragment) {
	      hEvtsAccpt->Fill(5.);
	    }
	    hEvtsAccpt->GetXaxis()->SetBinLabel(1, "DiMuon");
	    hEvtsAccpt->GetXaxis()->SetBinLabel(2, "MTDQuarkonium");
	    hEvtsAccpt->GetXaxis()->SetBinLabel(3, "HLTGood2");
	    hEvtsAccpt->GetXaxis()->SetBinLabel(4, "DiElectron");
	    hEvtsAccpt->GetXaxis()->SetBinLabel(5, "DIEP2Twr");
	    hEvtsAccpt->GetXaxis()->SetBinLabel(6, "HeavyFragment");
	    // fill events
	    if(!EventFilled) {
	      EventFilled = true;
	      addServerTags("L4Event");
	    }
	    float vertX = hlt_eve->vertexX;
	    float vertY = hlt_eve->vertexY;
	    float vertZ = hlt_eve->vertexZ;
	    float vertR = sqrt(vertX * vertX + vertY * vertY);
	    float lmvertX = hlt_eve->lmVertexX;
	    float lmvertY = hlt_eve->lmVertexY;
	    float lmvertZ = hlt_eve->lmVertexZ;
	    float VzVpd =  hlt_eve->vpdVertexZ;
	    innerGainPara = hlt_eve->innerSectorGain;
	    outerGainPara = hlt_eve->outerSectorGain;
	    BeamX = hlt_eve->beamlineX;
	    BeamY = hlt_eve->beamlineY;
	    hVertexX->Fill(vertX);
	    hVertexY->Fill(vertY);
	    hVertexZ->Fill(vertZ);
	    hVertexXY->Fill(vertX, vertY);
	    hVertexR->Fill(vertR);
	    hLm_VertexX->Fill(lmvertX);
	    hLm_VertexY->Fill(lmvertY);
	    hLm_VertexZ->Fill(lmvertZ);
	    hVzvpd_Vz->Fill(VzVpd, lmvertZ);
	    hVzDiff->Fill(VzVpd - lmvertZ);
	    
	    if(daqID & upc) {
	      hVertexX_UPC->Fill(vertX);
	      hVertexY_UPC->Fill(vertY);
	      hVertexZ_UPC->Fill(vertZ);
	      hLm_VertexX_UPC->Fill(lmvertX);
	      hLm_VertexY_UPC->Fill(lmvertY);
	      hLm_VertexZ_UPC->Fill(lmvertZ);
	      hVzvpd_Vz_UPC->Fill(VzVpd, lmvertZ);
	      hVzDiff_UPC->Fill(VzVpd - lmvertZ);
	    }

	    //BesGood
	    if(decision & triggerBitBesgoodEvents) {
	      if(!BESGoodFilled) {
		BESGoodFilled = true;
		addServerTags("L4BesGoodEvents");
	      }
	      hBesGoodVertexXY->Fill(vertX, vertY);
	      hBesGoodVertexZ->Fill(vertZ);
	      hBesGoodVr->Fill(vertR);
	      hBesGoodVrVsVz->Fill(vertZ,vertR);
	    }
	    
	    //HLTGood2
	    
	    if(decision & triggerBitHLTGood2) {
	      if(!HLTGood2Filled) {
		HLTGood2Filled = true;
		addServerTags("L4HLTGood2");
	      }
	      hHLTGood2VertexXY->Fill(vertX, vertY);
	      hHLTGood2VertexZ->Fill(vertZ);
	      hHLTGood2Vr->Fill(vertR);
	    }
	    
	    //BESMonitor
	    
	    if(decision & triggerBitBesMonitor) {
	      if(!BESMonitorFilled) {
		BESMonitorFilled = true;
		addServerTags("L4BesGoodMonitor");
	      }
	      hBesMonitorVertexXY->Fill(vertX, vertY);
	      hBesMonitorVr->Fill(vertR);
	    }
	    
	    //FixedTarget
	    
	    if(decision & triggerBitFixedTarget) {
	      if(!FixedTargetFilled) {
		FixedTargetFilled = true;
		addServerTags("L4FixedTarget");
	      }
	      hFixedTargetVertexXY->Fill(vertX, vertY);
	      hFixedTargetVr->Fill(vertR);
	      hFixedTarget_VertexZ->Fill(vertZ);
	    }
	    
	    //FixedTargetMonitor
	    
	    if(decision & triggerBitFixedTargetMonitor) {
	      if(!FixedTargetMonitorFilled) {
		FixedTargetMonitorFilled = true;
		addServerTags("L4FixedTargetMonitor");
	      }
	      hFixedTargetMonitorVertexXY->Fill(vertX, vertY);
	      hFixedTargetMonitorVr->Fill(vertR);
	      hFixedTargetMonitor_VertexZ->Fill(vertZ);
	    }
	    
	    if(decision & triggerBitDiElectron) {
	      if(!DiElectronFilled) {
		DiElectronFilled = true;
		addServerTags("L4DiElectron");
	      }
	    }
	    
	    if(!GlobalTracksFilled) {
	      GlobalTracksFilled = true;
	      addServerTags("L4GlobalTracks");
	    }
	    
	    if(!TOFFilled) {
	      TOFFilled = true;
	      addServerTags("L4TOF");
	    }

	    if(!EMCFilled) {
	      EMCFilled = true;
	      addServerTags("L4EMC");
	    }
	    
	    if(!PrimaryTracksFilled) {
	      PrimaryTracksFilled = true;
	      addServerTags("L4PrimaryTracks");
	    }
	    
	    if(!HeavyFragmentFilled) {
	      HeavyFragmentFilled = true;
	      addServerTags("L4HeavyFragment");
	    }
	    
	    if(decision & triggerBitDiElectron2Twr) {
	      if(!DiElectron2TwrFilled) {
		DiElectron2TwrFilled = true;
		addServerTags("L4DiElectron2Twr");
	      }
	    }
	    
	    if(decision & triggerBitUPC) {
	      if(!UPCFilled) {
		UPCFilled = true;
		addServerTags("L4UPC");
	      }
	    }
	    
	    if(decision & triggerBitDiMuon) {
	      if(!DiMuonFilled) {
		DiMuonFilled = true;
		addServerTags("L4DiMuon");
	      }
	    }
	    
	    if(decision & triggerBitUPCDiElectron) {
	      if(!UPCDiElectronFilled) {
		UPCDiElectronFilled = true;
		addServerTags("L4UPCDiElectron");
	      }
	    }
	    
	  }
	  
#pragma omp section
	  {
	    // fill ToF hits

	    for(u_int i = 0; i < hlt_tof->nTofHits; i++) {
	      short trayId   = hlt_tof->tofHit[i].trayId;
	      short channel  = hlt_tof->tofHit[i].channel;
	      float tdc      = hlt_tof->tofHit[i].tdc;
	      float triggertime = hlt_tof->tofHit[i].triggertime;
	      hTrayID_TrgTime->Fill(trayId, tdc - triggertime);
	      hchannelID->Fill(channel);
	    }

	    // fill pVPD hit
	    
	    for(u_int i = 0; i < hlt_pvpd->nPvpdHits; i++) {
	      short trayId      = hlt_pvpd->pvpdHit[i].trayId;
	      float tdc         = hlt_pvpd->pvpdHit[i].tdc;
	      float triggertime = hlt_pvpd->pvpdHit[i].triggertime;
	      hTrayID_TrgTime->Fill(trayId, tdc - triggertime);
	    }
	  }

	    
#pragma omp section
	  {
	    // fill EMC
	    
	    for(u_int i = 0; i < hlt_emc->nEmcTowers; i++) {
	      float energy     = hlt_emc->emcTower[i].energy;
	      float phi   = hlt_emc->emcTower[i].phi;
	      float  eta   = hlt_emc->emcTower[i].eta;
	      int softId  = hlt_emc->emcTower[i].softId;
	      int daqId   = hlt_emc->emcTower[i].daqId;
	      hTowerEnergy->Fill(energy);//run
	      hTowerDaqId->Fill(daqId);  //run
	      hTowerSoftId->Fill(softId);  //run
	      hTowerEtaPhi->Fill(phi, eta); //run
	      
	      if(daqID & upc) {
		hTowerEnergy_UPC->Fill(energy);//run
		hTowerDaqId_UPC->Fill(daqId);  //run
		hTowerSoftId_UPC->Fill(softId);  //run
		hTowerEtaPhi_UPC->Fill(phi, eta); //run
	      }
	    }
	  }
	  
	  
#pragma omp section
	  {
	    // global track
	    for(u_int i = 0; i < (u_int)hlt_gt->nGlobalTracks; i++) {
	      int nHits = hlt_gt->globalTrack[i].nHits;
	     	      
	      if(hlt_gt->globalTrack[i].flag < 0.) continue;
	      float pt = hlt_gt->globalTrack[i].pt;
	      float pz = hlt_gt->globalTrack[i].tanl * pt;
	      float p  = TMath::Sqrt(pt*pt+pz*pz);
	      float eta = 0.0;
	      if(p==pz&&pz>0) eta = 10e10 ;
	      if(p==pz&&pz<0) eta = -10e10 ;
	      eta = 0.5*TMath::Log((p+pz)/(p-pz)); 
	      
	      hGlob_Eta->Fill(eta);	      

	      if(nHits >= 25 && fabs(eta) < 1.) {
		float phi = hlt_gt->globalTrack[i].psi;
		if(phi < 0.0) phi += twopi;
		hGlob_Pt->Fill(pt);
		hGlob_Phi->Fill(phi);
	      }
	    }
	  }


#pragma omp section
	  {
	    // global track
	    for(u_int i = 0; i < (u_int)hlt_gt->nGlobalTracks; i++) {
	      int nHits = hlt_gt->globalTrack[i].nHits;
	     	      
	      if(hlt_gt->globalTrack[i].flag < 0.) continue;
	      float pt = hlt_gt->globalTrack[i].pt;
	      float pz = hlt_gt->globalTrack[i].tanl * pt;
	      float p  = TMath::Sqrt(pt*pt+pz*pz);
	      float eta = 0.0;
	      if(p==pz&&pz>0) eta = 10e10 ;
	      if(p==pz&&pz<0) eta = -10e10 ;
	      eta = 0.5*TMath::Log((p+pz)/(p-pz)); 
	      
	      if(nHits >= 25 && fabs(eta) < 1.) {
		float phi = hlt_gt->globalTrack[i].psi;
		if(phi < 0.0) phi += twopi;
		if(daqID & upc) {
		  hGlob_Pt_UPC->Fill(pt);
		  hGlob_Phi_UPC->Fill(phi);
		}
	      }

	      if(decision & triggerBitFixedTarget) {
		hFixedTarget_Glob_Eta->Fill(eta);
	      }
	        
	      if(decision & triggerBitFixedTargetMonitor) {
		hFixedTargetMonitor_Glob_Eta->Fill(eta);
	      }
	    }
	  }


#pragma omp section
	  {
	 
	    for(u_int i = 0; i < (u_int)hlt_gt->nGlobalTracks; i++) {
	      int nHits = hlt_gt->globalTrack[i].nHits;
	      int ndedx = hlt_gt->globalTrack[i].ndedx;

	      hnhits->Fill(nHits);
	      hnDedx->Fill(ndedx);
	   
	      if(daqID & upc) {
		hnhits_UPC->Fill(nHits);
		hnDedx_UPC->Fill(ndedx);
	      }
	      
	      if(hlt_gt->globalTrack[i].flag < 0.) continue;
	      
	      if(nHits >= 20 && ndedx >= 15) {
		float pt = hlt_gt->globalTrack[i].pt;
		float pz = hlt_gt->globalTrack[i].tanl * pt;
		int  q  = hlt_gt->globalTrack[i].q;
		float p = TMath::Sqrt(pt*pt+pz*pz);
		float dedx = hlt_gt->globalTrack[i].dedx;
	
		hGlob_dEdx->Fill(p * q, dedx);
		if(daqID & upc) {
		  hGlob_dEdx_UPC->Fill(p * q, dedx);
		}

		if( ndedx >= 20) {
		  hdEdx->Fill(p * q, dedx); //HeavyFragment Trigger
		  if(daqID & upc) {
		    hdEdx_UPC->Fill(p * q, dedx); // for HF reference
		  }
		}
	      }
	    }
	  }


#pragma omp section
	  {
	    // primary tracks

	    double Array_dcaXy[hlt_node->nNodes];
	    double Array_dcaZ[hlt_node->nNodes];	    

#pragma omp parallel for num_threads(4)

	    for(u_int i = 0; i < (u_int)hlt_node->nNodes; i++) {
	      int     globalTrackSN  = hlt_node->node[i].globalTrackSN;
	      int     primaryTrackSN = hlt_node->node[i].primaryTrackSN;
	      hlt_track   GTrack     = hlt_gt->globalTrack[globalTrackSN];
	      double  dcaX           = GTrack.r0 * cos(GTrack.phi0) - hlt_eve->lmVertexX;
	      double  dcaY           = GTrack.r0 * sin(GTrack.phi0) - hlt_eve->lmVertexY;
	      double  cross          = dcaX * sin(GTrack.psi) - dcaY * cos(GTrack.psi);
	      double  theSign        = (cross >= 0) ? 1. : -1.;
	      double  dcaXy          = theSign * sqrt(pow(dcaX, 2) + pow(dcaY, 2));
	      double  dcaZ           = GTrack.z0 - hlt_eve->lmVertexZ;

	      Array_dcaXy[i]=dcaXy;
	      Array_dcaZ[i]=dcaZ;
	    }
	    
	    for(int j = 0; j < hlt_node->nNodes; j++) {
	      hDcaXy->Fill(Array_dcaXy[j]);
	      hDcaZ->Fill(Array_dcaZ[j]);
	      
	      if(daqID & upc) {
		hDcaXy_UPC->Fill(Array_dcaXy[j]);
		hDcaZ_UPC->Fill(Array_dcaZ[j]);
	      }
	    }
	  }

#pragma omp section
	  {
	    
	    int count = 0;
	    int count_UPC = 0;
	    for(u_int i = 0; i < hlt_node->nNodes; i++) {
	      //int     globalTrackSN  = hlt_node->node[i].globalTrackSN;
	      int     primaryTrackSN = hlt_node->node[i].primaryTrackSN;

	      if(primaryTrackSN < 0) continue;
	      count++;
	      if(daqID & upc) count_UPC++;
	      hlt_track PTrack = hlt_pt->primaryTrack[primaryTrackSN];
	      if(PTrack.flag < 0.) continue;
	     	      
	      int nHits = PTrack.nHits;
	      int ndedx = PTrack.ndedx;
	      int q = PTrack.q;
	      float pt = PTrack.pt;
	      float px = cos(PTrack.psi) * PTrack.pt;
	      float py = sin(PTrack.psi) * PTrack.pt;
	      float pz = PTrack.tanl * PTrack.pt;	      
	      
	      TVector3 mom(px, py, pz);
	      float eta = mom.PseudoRapidity();
	      float phi = mom.Phi();
	      if(phi < 0.0) phi += twopi;
	      float p = mom.Mag();
	      float dedx = PTrack.dedx;
	      
	      hPrim_Eta->Fill(eta);
	      if(daqID & upc) hPrim_Eta_UPC->Fill(eta);
	      if(nHits >= 25 && fabs(eta) < 1.) {
		hPrim_Pt->Fill(pt);
		hPrim_Phi->Fill(phi);
		if(daqID & upc) {
		  hPrim_Pt_UPC->Fill(pt);
		  hPrim_Phi_UPC->Fill(phi);
		}
	      }
	      if(decision & triggerBitFixedTarget) {
		hFixedTarget_Prim_Eta->Fill(eta);
	      }
	      if(decision & triggerBitFixedTargetMonitor) {
		hFixedTargetMonitor_Prim_Eta->Fill(eta);
	      }
	      
	      if(nHits >= 20 && ndedx >= 15) {
		hPrim_dEdx->Fill(p * q, dedx);
		if(daqID & upc) hPrim_dEdx_UPC->Fill(p * q, dedx);

		if(p >= 0.5 && p <= 0.6) {
		  hLn_dEdx->Fill(log(dedx));
		  if(daqID & upc) hLn_dEdx_UPC->Fill(log(dedx));
		}
	      }
	    }
	    
	    primaryTracks = count;
	    hglobalMult->Fill(hlt_gt->nGlobalTracks);
	    hprimaryMult->Fill(count);
	    
	    if(daqID & upc) {
	      primaryTracks_UPC = count_UPC;
	      hglobalMult_UPC->Fill(hlt_gt->nGlobalTracks);
	      hprimaryMult_UPC->Fill(count_UPC);
	    }
	    if(decision & triggerBitBesgoodEvents){
	      hBesGoodprimaryMult->Fill(count);
	    }
	    if(decision & triggerBitHLTGood2){
	      hHLTGood2primaryMult->Fill(count);
	    }
	  }

#pragma omp section
	  {
	    // fill nodes
	    
	    for(u_int i = 0; i < u_int (hlt_node->nNodes*(1.0/3.0)); i++) {
	      int  tofHitSN       = hlt_node->node[i].tofHitSN;
	      
	      if(tofHitSN >= 0) {
		int  primaryTrackSN = hlt_node->node[i].primaryTrackSN;	

		if(primaryTrackSN >= 0) {
		  int  projChannel = hlt_node->node[i].projChannel;
		  int  Proj_trayId = hlt_tof->tofHit[tofHitSN].trayId;
	
		  for(u_int j = 0; j < hlt_tof->nTofHits; j++) {
		    int fire_trayId = hlt_tof->tofHit[j].trayId;
	
		    if(Proj_trayId == fire_trayId) {
		      hMatchId_fiberId->Fill(projChannel, hlt_tof->tofHit[j].channel);
		    }
		  }
		}
	      }
	    }
	}
	  
#pragma omp section
	  {
	    
	    for(u_int i = u_int (hlt_node->nNodes*(1.0/3.0)); i < u_int (hlt_node->nNodes*(2.0/3.0)); i++) {
	      int  tofHitSN       = hlt_node->node[i].tofHitSN;
	      	      
	      if(tofHitSN >= 0) {
		int  primaryTrackSN = hlt_node->node[i].primaryTrackSN;	
	
		if(primaryTrackSN >= 0) {
		  int  projChannel = hlt_node->node[i].projChannel;
		  int Proj_trayId = hlt_tof->tofHit[tofHitSN].trayId;
		  
		  for(u_int j = 0; j < hlt_tof->nTofHits; j++) {
		    int fire_trayId = hlt_tof->tofHit[j].trayId;
		    if(Proj_trayId == fire_trayId) {
		      hMatchId_fiberId_copy->Fill(projChannel, hlt_tof->tofHit[j].channel);
		    }
		  }
		}
	      }
	    }
	  }

#pragma omp section
	  {
	    for(u_int i = u_int (hlt_node->nNodes*(2.0/3.0)); i < hlt_node->nNodes; i++) {
	      int  tofHitSN       = hlt_node->node[i].tofHitSN;
	      
	      if(tofHitSN >= 0) {
		int  primaryTrackSN = hlt_node->node[i].primaryTrackSN;	
	
		if(primaryTrackSN >= 0) {
		  int  projChannel = hlt_node->node[i].projChannel;
		  int Proj_trayId = hlt_tof->tofHit[tofHitSN].trayId;
		  
		  for(u_int j = 0; j < hlt_tof->nTofHits; j++) {
		    int fire_trayId = hlt_tof->tofHit[j].trayId;
		    if(Proj_trayId == fire_trayId) {
		      hMatchId_fiberId_copy2->Fill(projChannel, hlt_tof->tofHit[j].channel);
		    }
		  }
		}
	      }
	    }
	  }
	  
#pragma omp section
	  {

	    for(u_int i = 0; i < hlt_node->nNodes; i++) {
	      int     primaryTrackSN = hlt_node->node[i].primaryTrackSN;
	      int     tofHitSN       = hlt_node->node[i].tofHitSN;
	      int     emcTowerSN     = hlt_node->node[i].emcTowerSN;
	      hlt_track   NTrack         = hlt_pt->primaryTrack[primaryTrackSN];
              float   pt         = NTrack.pt;
	      float   pz         = NTrack.tanl * NTrack.pt;
	      float   p          = sqrt(pt * pt + pz * pz);
	      
	      if(tofHitSN >= 0) {
		float localY = hlt_node->node[i].localY;
		float localZ = hlt_node->node[i].localZ;
		float beta   = hlt_node->node[i].beta;
		hLocalZ->Fill(localZ);
		hLocalY->Fill(localY);
		if(primaryTrackSN >= 0) {
		  hInverseBeta->Fill(p, 1 / beta);
		}
	      }

	      if(emcTowerSN >= 0 && NTrack.nHits > 20 && NTrack.ndedx > 15) {
		double emcMatchPhiDiff = hlt_node->node[i].emcMatchPhiDiff;
		double emcMatchZEdge   = hlt_node->node[i].emcMatchZEdge;
		hMatchPhi_Diff->Fill(emcMatchPhiDiff);
		if(daqID & upc) hMatchPhi_Diff_UPC->Fill(emcMatchPhiDiff);
		if(emcMatchZEdge > 0.) {
		  hzEdge->Fill(emcMatchZEdge);
		  if(daqID & upc) hzEdge_UPC->Fill(emcMatchZEdge);
		}
	      }
	      
	    }
	  }

	  
#pragma omp section
	  {
	    
	    // heavy fragment
	   
	    for(u_int i = 0; i < hlt_hf->nHeavyFragments; i++) {
	      
	      int heavyFrag_NodeSN = hlt_hf->heavyFragmentSN[i];
	      int heavyFragmentglobSN  = hlt_node->node[heavyFrag_NodeSN].globalTrackSN;
	      hlt_track HFtrack = hlt_gt->globalTrack[heavyFragmentglobSN];
	      int nHits =  HFtrack.nHits;
	      int ndedx =  HFtrack.ndedx;
	      int q     =  HFtrack.q;
	      float hfpx    = HFtrack.pt * cos(HFtrack.psi);
	      float hfpy    = HFtrack.pt * sin(HFtrack.psi);
	      float hfpz    = HFtrack.pt * HFtrack.tanl;
	      float hfp     = sqrt(hfpx * hfpx + hfpy * hfpy + hfpz * hfpz);
	      float hfdedx  =  HFtrack.dedx;
	  
	      if(nHits >= 20 && ndedx >= 15) {
		hHFM_dEdx->Fill(hfp * q , hfdedx);
		if(daqID & upc) hHFM_dEdx_UPC->Fill(hfp * q , hfdedx);
	      }
	    }
	  }


#pragma omp section
	  {
	    // di-pion
	    if(decision & triggerBitUPC) {
	      
	      for(u_int i = 0; i < hlt_dipi->nRhos; i++) {
		int Daughter1NodeSN = hlt_dipi->PionPair[i].dau1NodeSN;
		int Daughter2NodeSN = hlt_dipi->PionPair[i].dau2NodeSN;
		int Daughter1TrackSN = hlt_node->node[Daughter1NodeSN].primaryTrackSN;
		int Daughter2TrackSN = hlt_node->node[Daughter2NodeSN].primaryTrackSN;
	      
		if(Daughter1TrackSN < 0) continue;
		hlt_track Daughter1Track =  hlt_pt->primaryTrack[Daughter1TrackSN];
		float Daughter1q     = Daughter1Track.q;
		if(Daughter2TrackSN < 0.) continue;
		hlt_track Daughter2Track =  hlt_pt->primaryTrack[Daughter2TrackSN];
		float Daughter2q     =  Daughter2Track.q;
		
		float m = hlt_dipi->PionPair[i].invariantMass;
		float diffphi = hlt_dipi->PionPair[i].deltphi;
		hDiPionDeltphi->Fill(diffphi);
		
		if(Daughter1q * Daughter2q < 0.) hDiPionInvMassFullRange->Fill(m);
		else hDiPionInvMassFullRangeBG->Fill(m);
		
	      }
	    }
	  }


#pragma omp section
	  {
	    // di-muon
	    const int nNodes = hlt_node->nNodes;
	    int global2prim[nNodes];
	    for(int inode = 0; inode<hlt_node->nNodes; inode++)
	      {
		int gTrackSN  = hlt_node->node[inode].globalTrackSN;
		int pTrackSN  = hlt_node->node[inode].primaryTrackSN;
		global2prim[gTrackSN] = pTrackSN;
	      }
	    
	    if(decision & triggerBitDiMuon) {
	      int nMtdHit = hlt_mtd->nMtdHits;
	      vector<int> pMuTrkId;
	      pMuTrkId.clear();
	      for(int i=0; i<nMtdHit; i++)
		{
		  int backleg  = (int)hlt_mtd->mtdHit[i].backleg;
		  int module   = (int)hlt_mtd->mtdHit[i].tray;
		  int channel  = (int)hlt_mtd->mtdHit[i].channel;
		  int gchannel = (module-1)*12+channel;
		  int gmodule  = (backleg-1)*5+module;
		  
		  hMtdHitMap->Fill(backleg,gchannel);
		  
		  int trkid   = (int)hlt_mtd->mtdHit[i].hlt_trackId;
		  if(trkid<0) continue;
		  double deltaz = hlt_mtd->mtdHit[i].delta_z;
		  double deltay = hlt_mtd->mtdHit[i].delta_y;
		  hMtdMatchHitMap->Fill(backleg,gchannel);
		  hMtdDeltaZvsModule->Fill(gmodule,deltaz);
		  hMtdDeltaZ->Fill(deltaz);
		  hMtdDeltaYvsModule->Fill(gmodule,deltay);
		  hMtdDeltaY->Fill(deltay);
		  if(fabs(deltaz)>20)continue;
		  if(fabs(deltay)>20)continue;
		  
		  int pTrkId  = global2prim[trkid];
		  if(pTrkId<0) continue;
		  hlt_track pTrack = hlt_pt->primaryTrack[pTrkId];
		  float pt = pTrack.pt;
		  if(pt<1.)continue;
		  if(pTrack.nHits<15)continue;
		  if(pTrack.ndedx<10)continue;
		  pMuTrkId.push_back(pTrkId);
		}

	  
	      // J/psi analysis
	      const float muMass = 0.10566;
	      unsigned int npmuon = pMuTrkId.size();
	      for(unsigned int i=0; i<npmuon; i++)
		{
		  hlt_track ipTrack = hlt_pt->primaryTrack[pMuTrkId[i]];
		  char iq = ipTrack.q;
		  float ipt = ipTrack.pt;
		  float ipsi = ipTrack.psi;
		  float itanl = ipTrack.tanl;
		  float ipx = TMath::Cos(ipsi)*ipt;
		  float ipy = TMath::Sin(ipsi)*ipt;
		  float ipz = itanl * ipt;
		  TLorentzVector imuon;
		  imuon.SetXYZM(ipx,ipy,ipz,muMass);
		  
		  for(UInt_t j=i+1; j<npmuon; j++)
		    {
		      hlt_track jpTrack = hlt_pt->primaryTrack[pMuTrkId[j]];
		      char jq = jpTrack.q;
		      float jpt = jpTrack.pt;
		      
		      double pt_lead = (ipt>jpt) ? ipt : jpt;
		      if(pt_lead<1.5) continue;
		      
		      float jpsi = jpTrack.psi;
		      float jtanl = jpTrack.tanl;
		      float jpx = TMath::Cos(jpsi)*jpt;
		      float jpy = TMath::Sin(jpsi)*jpt;
		      float jpz = jtanl * jpt;
		      TLorentzVector jmuon;
		      jmuon.SetXYZM(jpx,jpy,jpz,muMass);
		      
		      TLorentzVector muPair = imuon + jmuon;
		      if(iq*jq<0) {
			hInvMassUS->Fill(muPair.M());
			hMTDDiMuonJpsiMassUS->Fill(muPair.M());
			hMTDDiMuonUpsilonMassUS->Fill(muPair.M());
		      }
		      else{
			hInvMassLS->Fill(muPair.M());
			hMTDDiMuonJpsiMassLS->Fill(muPair.M());
			hMTDDiMuonUpsilonMassLS->Fill(muPair.M());
		      }
		    }//j
		}//i
	      

	  
	      double jpsi_lowm1=2.7, jpsi_highm1=3.5;
	      int lowbin = hMTDDiMuonJpsiMassUS->FindBin(jpsi_lowm1);
	      int highbin = hMTDDiMuonJpsiMassUS->FindBin(jpsi_highm1);
	      
	      US12=hMTDDiMuonJpsiMassUS->Integral(lowbin, highbin,"");
	      tlx12_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US12)));
	      LS12=hMTDDiMuonJpsiMassLS->Integral(lowbin, highbin,"");
	      tlx12_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS12)));
	      tlxmass12->SetText(0.6, 0.7, Form("%.2f #leq m_{ee} #leq %.2f", double(jpsi_lowm1), double(jpsi_highm1)) );
	      
	      double upsilon_lowm1=9, upsilon_highm1=11;
	      lowbin = hMTDDiMuonJpsiMassUS->FindBin(upsilon_lowm1);
	      highbin = hMTDDiMuonJpsiMassUS->FindBin(upsilon_highm1);
	      
	      US13=hMTDDiMuonUpsilonMassUS->Integral(lowbin, highbin,"");
	      tlx13_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US13)));
	      LS13=hMTDDiMuonUpsilonMassLS->Integral(lowbin, highbin,"");
	      tlx13_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS13)));
	      tlxmass13->SetText(0.6, 0.7, Form("%.2f #leq m_{ee} #leq %.2f", double(upsilon_lowm1), double(upsilon_highm1)) );
	    }//di muon
	  }

#pragma omp section
	  {
	
	    const int tmp_nNodes = hlt_node->nNodes;
	    int tmp_global2prim[tmp_nNodes];
	    for(int inode = 0; inode<hlt_node->nNodes; inode++)
	      {
		int tmp_gTrackSN  = hlt_node->node[inode].globalTrackSN;
		int tmp_pTrackSN  = hlt_node->node[inode].primaryTrackSN;
		tmp_global2prim[tmp_gTrackSN] = tmp_pTrackSN;
	      }
	    //-----------------------------------------------------------------
	    //if(decision & triggerBitDiMuon) {   need the triggerBitMTDQuarkonium
	    int nMTDQmPairs = hlt_mtdqm->nMTDQuarkonium;
	    for(int i=0; i<nMTDQmPairs; i++){
	      
		int mgtrkid1 = hlt_mtdqm->MTDQuarkonium[i].muonTrackId1;
		int mgtrkid2 = hlt_mtdqm->MTDQuarkonium[i].muonTrackId2;

		int pTrackSN1 = tmp_global2prim[mgtrkid1];
		int pTrackSN2 = tmp_global2prim[mgtrkid2];
		if( pTrackSN1<0||pTrackSN2<0 ) continue;

		hlt_track muPtrk1 =  hlt_pt->primaryTrack[pTrackSN1];
		hlt_track muPtrk2 =  hlt_pt->primaryTrack[pTrackSN2];
		if(muPtrk1.nHits<15||muPtrk2.nHits<15)continue;
		if(muPtrk1.ndedx<10||muPtrk2.ndedx<10)continue;

		double mupt1 = muPtrk1.pt;
		double mupt2 = muPtrk2.pt;
		double mupt_lead = (mupt1>mupt2) ? mupt1 : mupt2;
		if(mupt1<1.0||mupt2<1.0||mupt_lead<1.5) continue;

		//find the mtd hits for these two muons
		int muhit1=-1;
		int muhit2=-1;

		int nMtdHit = hlt_mtd->nMtdHits;
		for(int ihit=0; ihit<nMtdHit; ihit++)
		{
			if(muhit1>=0&&muhit2>=0) break;
			int gtrkid  = (int)hlt_mtd->mtdHit[ihit].hlt_trackId;
			if(gtrkid==mgtrkid1){muhit1=ihit; continue;}
			if(gtrkid==mgtrkid2){muhit2=ihit; continue;}
		}

		double mudy1 = hlt_mtd->mtdHit[muhit1].delta_y;
		double mudy2 = hlt_mtd->mtdHit[muhit2].delta_y;
		if(fabs(mudy1)>20||fabs(mudy2)>20)continue;

		double mudz1 = hlt_mtd->mtdHit[muhit1].delta_z;
		double mudz2 = hlt_mtd->mtdHit[muhit2].delta_z;
		if(fabs(mudz1)>20||fabs(mudz2)>20)continue;

		const double mumass = 0.10566; 

		double mupx1 = mupt1 * cos(muPtrk1.psi);
		double mupy1 = mupt1 * sin(muPtrk1.psi);
		double mupz1 = mupt1 * muPtrk1.tanl;

		double mupx2 = mupt2 * cos(muPtrk2.psi);
		double mupy2 = mupt2 * sin(muPtrk2.psi);
		double mupz2 = mupt2 * muPtrk2.tanl;

		TVector3 muPMom1(mupx1, mupy1, mupz1);

		TVector3 muPMom2(mupx2, mupy2, mupz2);
		
		TLorentzVector Muon1(0,0,0,0);
		Muon1.SetXYZM(mupx1, mupy1, mupz1, mumass);
		TLorentzVector Muon2(0,0,0,0);
		Muon2.SetXYZM(mupx2, mupy2, mupz2, mumass);

		TLorentzVector QmPair = Muon1 + Muon2;
		double qmMass=QmPair.M();
		double qmPt=QmPair.Pt();
		
		if(muPtrk1.q*muPtrk2.q<0){

			hMTDQmInvMassUS->Fill(qmMass);
			hMTDQmJpsiMass_ptcut0_US->Fill(qmMass);
			if(qmPt>2.)hMTDQmJpsiMass_ptcut2_US->Fill(qmMass);
			if(qmPt>4.)hMTDQmJpsiMass_ptcut4_US->Fill(qmMass);

			hMTDQmUpsilonMassUS->Fill(qmMass);

		}else{

			hMTDQmInvMassLS->Fill(qmMass);
			hMTDQmJpsiMass_ptcut0_LS->Fill(qmMass);
			if(qmPt>2.)hMTDQmJpsiMass_ptcut2_LS->Fill(qmMass);
			if(qmPt>4.)hMTDQmJpsiMass_ptcut4_LS->Fill(qmMass);

			hMTDQmUpsilonMassLS->Fill(qmMass);
		}
		
	    }
	    
	    double lowm1=2.7, highm1=3.5;
	    int lowbin = hMTDQmJpsiMass_ptcut0_US->FindBin(lowm1);
	    int highbin = hMTDQmJpsiMass_ptcut0_US->FindBin(highm1);
	    
	    US8=hMTDQmJpsiMass_ptcut0_US->Integral(lowbin, highbin,"");
	    tlx8_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US8)));
	    LS8=hMTDQmJpsiMass_ptcut0_LS->Integral(lowbin, highbin,"");
	    tlx8_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS8)));
	    
	    US9=hMTDQmJpsiMass_ptcut2_US->Integral(lowbin, highbin,"");
	    tlx9_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US9)));
	    LS9=hMTDQmJpsiMass_ptcut2_LS->Integral(lowbin, highbin,"");
	    tlx9_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS9)));
	    
	    US10=hMTDQmJpsiMass_ptcut4_US->Integral(lowbin, highbin,"");
	    tlx10_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US10)));
	    LS10=hMTDQmJpsiMass_ptcut4_LS->Integral(lowbin, highbin,"");
	    tlx10_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS10)));
	    
	    double lowm2=9.0, highm2=11.;
	    int lowbin2 = hMTDQmJpsiMass_ptcut0_US->FindBin(lowm2);
	    int highbin2 = hMTDQmJpsiMass_ptcut0_US->FindBin(highm2);
	    
	    US11=hMTDQmUpsilonMassUS->Integral(lowbin2, highbin2,"");
	    tlx11_us->SetText(0.15, 0.65, Form("#US = %.0f", double(US11)));
	    LS11=hMTDQmUpsilonMassLS->Integral(lowbin, highbin,"");
	    tlx11_ls->SetText(0.15, 0.6, Form("#LS = %.0f", double(LS11)));
	   
	  }


#pragma omp section
	  {
	    // upc di-e
	    if(decision & triggerBitUPCDiElectron) {
	      
	      for(u_int i = 0; i < hlt_upcdiep->nEPairs; i++) {
		int Daughter1NodeSN = hlt_upcdiep->ePair[i].dau1NodeSN;
		int Daughter2NodeSN = hlt_upcdiep->ePair[i].dau2NodeSN;
		int Daughter1TrackSN = hlt_node->node[Daughter1NodeSN].primaryTrackSN;
		int Daughter2TrackSN = hlt_node->node[Daughter2NodeSN].primaryTrackSN;
		int Daughter1EmcSN = hlt_node->node[Daughter1NodeSN].emcTowerSN;
		int Daughter2EmcSN = hlt_node->node[Daughter2NodeSN].emcTowerSN;
		
		if(Daughter1TrackSN < 0) continue;
		hlt_track Daughter1Track =  hlt_pt->primaryTrack[Daughter1TrackSN];
		
		float Daughter1_EP_ratio = -999.;
		
		float Daughter1q     = Daughter1Track.q;
		float Daughter1px    = Daughter1Track.pt * cos(Daughter1Track.psi);
		float Daughter1py    = Daughter1Track.pt * sin(Daughter1Track.psi);
		float Daughter1pz    = Daughter1Track.pt * Daughter1Track.tanl;
		float Daughter1dedx  = Daughter1Track.dedx;
			
		TVector3 Daughter1(Daughter1px, Daughter1py, Daughter1pz);
		float Daughter1p = Daughter1.Mag();
		
		float Daughter1phi = Daughter1.Phi();
		if(Daughter1phi < 0.) Daughter1phi += twopi;
		
		hdEdx_P1_UPC->Fill(Daughter1p , Daughter1dedx);
		if(Daughter1EmcSN >= 0) {
		  float Daughter1TowerEnergy = hlt_emc->emcTower[Daughter1EmcSN].energy;
		  Daughter1_EP_ratio = Daughter1TowerEnergy / Daughter1p;
		  hDaughter1P_TowerEnergy_UPC->Fill(Daughter1_EP_ratio);
		}
		
		if(Daughter2TrackSN < 0.) continue;
		hlt_track Daughter2Track =  hlt_pt->primaryTrack[Daughter2TrackSN];
		float Daughter2_EP_ratio = -999.;

		float Daughter2q     =  Daughter2Track.q;
		float Daughter2px    =  Daughter2Track.pt * cos(Daughter2Track.psi);
		float Daughter2py    =  Daughter2Track.pt * sin(Daughter2Track.psi);
		float Daughter2pz    =  Daughter2Track.pt * Daughter2Track.tanl;
		float Daughter2dedx  =  Daughter2Track.dedx;
		
		TVector3 Daughter2(Daughter2px, Daughter2py, Daughter2pz);
		float Daughter2p = Daughter2.Mag();

		float Daughter2phi = Daughter2.Phi();
		if(Daughter2phi < 0.0) Daughter2phi += twopi;
		hdEdx_P2_UPC->Fill(Daughter2p , Daughter2dedx);
		if(Daughter2EmcSN >= 0) {
		  float Daughter2TowerEnergy = hlt_emc->emcTower[Daughter2EmcSN].energy;
		  Daughter2_EP_ratio = Daughter2TowerEnergy / Daughter2p;
		  hDaughter2P_TowerEnergy_UPC->Fill(Daughter2_EP_ratio);
		}
		
		// j/psi
	
		float px = cos(hlt_upcdiep->ePair[i].psi) * hlt_upcdiep->ePair[i].pt;
		float py = sin(hlt_upcdiep->ePair[i].psi) * hlt_upcdiep->ePair[i].pt;
		float pz = hlt_upcdiep->ePair[i].tanl * hlt_upcdiep->ePair[i].pt;
		float m = hlt_upcdiep->ePair[i].invariantMass;
		
		if(Daughter1q * Daughter2q < 0.) {
		  hDiElectronInvMassFullRange_UPC->Fill(m);
		} else {
		  hDiElectronInvMassFullRangeBG_UPC->Fill(m);
		}
		TLorentzVector jpsi(0, 0, 0, 0);
		jpsi.SetXYZM(px, py, pz, m);
		float rapidity = jpsi.Rapidity();
		hDiLeptonRapidity_UPC->Fill(rapidity);
		
	      }
	    }
	  }
	    //******************************************************
	   

#pragma omp section
	  {
	    //di-e2Twr
	    
	    for(u_int i = 0; i < hlt_Twrdiep->nEPairs; i++) {
	      int Daughter1NodeSN = hlt_Twrdiep->ePair[i].dau1NodeSN;
	      int Daughter2NodeSN = hlt_Twrdiep->ePair[i].dau2NodeSN;
	      int Daughter1TrackSN = hlt_node->node[Daughter1NodeSN].primaryTrackSN;
	      int Daughter2TrackSN = hlt_node->node[Daughter2NodeSN].primaryTrackSN;
	      int Daughter1EmcSN = hlt_node->node[Daughter1NodeSN].emcTowerSN;
	      int Daughter2EmcSN = hlt_node->node[Daughter2NodeSN].emcTowerSN;
	      int Daughter1TofSN = hlt_node->node[Daughter1NodeSN].tofHitSN;
	      int Daughter2TofSN = hlt_node->node[Daughter2NodeSN].tofHitSN;
	      
	      if(Daughter1TrackSN < 0) continue;
	      hlt_track Daughter1Track =  hlt_pt->primaryTrack[Daughter1TrackSN];

	      float Daughter1beta = -999.;
	      float Daughter1phidiff = -999.;
	      float Daughter1_PE_ratio = -999.;
	      float Daughter1_EP_ratio = -999.;
	      
	      float Daughter1q     = Daughter1Track.q;
	      float Daughter1px    = Daughter1Track.pt * cos(Daughter1Track.psi);
	      float Daughter1py    = Daughter1Track.pt * sin(Daughter1Track.psi);
	      float Daughter1pz    = Daughter1Track.pt * Daughter1Track.tanl;
	      float Daughter1dedx  = Daughter1Track.dedx;
	      int Daughter1ndedx = Daughter1Track.ndedx;
	      
	      TVector3 Daughter1(Daughter1px, Daughter1py, Daughter1pz);
	      float Daughter1p = Daughter1.Mag();
	      
	      double dedx1E = getDedx(Daughter1p, e);
	      float nSigma1 = log(Daughter1dedx / dedx1E) / A * sqrt(Daughter1ndedx);
	      
	      float Daughter1phi = Daughter1.Phi();
	      if(Daughter1phi < 0.) Daughter1phi += twopi;
	      hdEdx_P1_Twr->Fill(Daughter1p , Daughter1dedx);
	      if(Daughter1EmcSN >= 0) {
		float Daughter1TowerEnergy = hlt_emc->emcTower[Daughter1EmcSN].energy;
		Daughter1_PE_ratio = Daughter1p / Daughter1TowerEnergy;
		Daughter1_EP_ratio = Daughter1TowerEnergy / Daughter1p;
		hDaughter1P_TowerEnergy_Twr->Fill(Daughter1_EP_ratio);
		Daughter1phidiff = hlt_node->node[Daughter1NodeSN].emcMatchPhiDiff;
	      }
	      if(Daughter1TofSN >= 0.) {
		Daughter1beta = hlt_node->node[Daughter1NodeSN].beta;
	      }

	      if(Daughter2TrackSN < 0.) continue;
	      hlt_track Daughter2Track =  hlt_pt->primaryTrack[Daughter2TrackSN];
	      float Daughter2phidiff = -999.;
	      float Daughter2beta = -999.;
	      float Daughter2_PE_ratio = -999.;
	      float Daughter2_EP_ratio = -999.;
	      
	      float Daughter2q     =  Daughter2Track.q;
	      float Daughter2px    =  Daughter2Track.pt * cos(Daughter2Track.psi);
	      float Daughter2py    =  Daughter2Track.pt * sin(Daughter2Track.psi);
	      float Daughter2pz    =  Daughter2Track.pt * Daughter2Track.tanl;
	      float Daughter2dedx  =  Daughter2Track.dedx;
	      int Daughter2ndedx = Daughter2Track.ndedx;
	      
	      TVector3 Daughter2(Daughter2px, Daughter2py, Daughter2pz);
	      float Daughter2p = Daughter2.Mag();
	      
	      double dedx2E = getDedx(Daughter2p, e);
	      float nSigma2 = log(Daughter2dedx / dedx2E) / A * sqrt(Daughter2ndedx);

	      float Daughter2phi = Daughter2.Phi();
	      if(Daughter2phi < 0.0) Daughter2phi += twopi;
	      
	      hdEdx_P2_Twr->Fill(Daughter2p , Daughter2dedx);
	      if(Daughter2EmcSN >= 0) {
		float Daughter2TowerEnergy = hlt_emc->emcTower[Daughter2EmcSN].energy;
		Daughter2_PE_ratio = Daughter2p / Daughter2TowerEnergy;
		Daughter2_EP_ratio = Daughter2TowerEnergy / Daughter2p;
		hDaughter2P_TowerEnergy_Twr->Fill(Daughter2_EP_ratio);
		Daughter2phidiff = hlt_node->node[Daughter2NodeSN].emcMatchPhiDiff;
	      }
	      if(Daughter2TofSN >= 0.) {
		Daughter2beta = hlt_node->node[Daughter2NodeSN].beta;
	      }
	      
	      // j/psi
	
	      float px = cos(hlt_Twrdiep->ePair[i].psi) * hlt_Twrdiep->ePair[i].pt;
	      float py = sin(hlt_Twrdiep->ePair[i].psi) * hlt_Twrdiep->ePair[i].pt;
	      float pz = hlt_Twrdiep->ePair[i].tanl * hlt_Twrdiep->ePair[i].pt;
	      float m = hlt_Twrdiep->ePair[i].invariantMass;
	      
	      if(Daughter1q * Daughter2q < 0.) {
		hDiElectronInvMassFullRange_Twr->Fill(m);
	      } else {
		hDiElectronInvMassFullRangeBG_Twr->Fill(m);
	      }
	      
	      if(nSigma1 > -0.9 && nSigma2 > -0.9 &&
		 Daughter1p > 2.3 && Daughter2p > 1.5 &&
		 Daughter1ndedx > 16 && Daughter2ndedx > 16 &&
		 Daughter1_PE_ratio < 1.5 && Daughter1_PE_ratio > 0.5 && Daughter2_PE_ratio < 1.5 && Daughter2_PE_ratio > 0.5 &&
		 Daughter1phidiff > 0. && Daughter1phidiff < 0.05 && Daughter2phidiff > 0. && Daughter2phidiff < 0.05){
		if(Daughter1q * Daughter2q < 0.)
		  {
		    hDiElectronInvMassTpxEmc_Twr->Fill(m);
		    if(fabs(1 / Daughter1beta - 1) < 0.04 && fabs(1 / Daughter2beta - 1) < 0.04)
		      {
			hDiElectronInvMassCut_Twr->Fill(m);
		      }
		  }
		else
		  {
		    hDiElectronInvMassTpxEmcBG_Twr->Fill(m);
		    if(fabs(1 / Daughter1beta - 1) < 0.04 && fabs(1 / Daughter2beta - 1) < 0.04)
		      {
			hDiElectronInvMassCutBG_Twr->Fill(m);
		      } 
		  }
		if(Daughter1TofSN >= 0.)
		  {
		    hDaughter1TpxEmcInverseBeta_Twr->Fill(1 / Daughter1beta);
		  }
		if(Daughter2TofSN >= 0.)
		  {
		    hDaughter2TpxEmcInverseBeta_Twr->Fill(1 / Daughter2beta);
		  }
	      }
	    
	      TLorentzVector jpsi(0, 0, 0, 0);
	      jpsi.SetXYZM(px, py, pz, m);
	      float rapidity = jpsi.Rapidity();
	      hDiLeptonRapidity_Twr->Fill(rapidity);
	      
	    }//nEPair
	  }

#pragma omp section
	  {
	    // di-e
	    for(u_int i = 0; i < hlt_diep->nEPairs; i++) {
	      int Daughter1NodeSN = hlt_diep->ePair[i].dau1NodeSN;
	      int Daughter2NodeSN = hlt_diep->ePair[i].dau2NodeSN;
	      int Daughter1TrackSN = hlt_node->node[Daughter1NodeSN].primaryTrackSN;
	      int Daughter2TrackSN = hlt_node->node[Daughter2NodeSN].primaryTrackSN;
	      int Daughter1EmcSN = hlt_node->node[Daughter1NodeSN].emcTowerSN;
	      int Daughter2EmcSN = hlt_node->node[Daughter2NodeSN].emcTowerSN;
	      int Daughter1TofSN = hlt_node->node[Daughter1NodeSN].tofHitSN;
	      int Daughter2TofSN = hlt_node->node[Daughter2NodeSN].tofHitSN;
	      
	      if(Daughter1TrackSN < 0) continue;
	      hlt_track Daughter1Track =  hlt_pt->primaryTrack[Daughter1TrackSN];

	      float Daughter1beta = -999.;
	      float Daughter1phidiff = -999.;
	      float Daughter1_PE_ratio = -999.;
	      float Daughter1_EP_ratio = -999.;
	      
	      float Daughter1q     = Daughter1Track.q;
	      float Daughter1px    = Daughter1Track.pt * cos(Daughter1Track.psi);
	      float Daughter1py    = Daughter1Track.pt * sin(Daughter1Track.psi);
	      float Daughter1pz    = Daughter1Track.pt * Daughter1Track.tanl;
	      float Daughter1dedx  = Daughter1Track.dedx;
	      int Daughter1ndedx = Daughter1Track.ndedx;
	      
	      TVector3 Daughter1(Daughter1px, Daughter1py, Daughter1pz);
	      float Daughter1p = Daughter1.Mag();
	      
	      double dedx1E = getDedx(Daughter1p, e);
	      float nSigma1 = log(Daughter1dedx / dedx1E) / A * sqrt(Daughter1ndedx);
	      
	      float Daughter1phi = Daughter1.Phi();
	      if(Daughter1phi < 0.) Daughter1phi += twopi;
	      hdEdx_P1->Fill(Daughter1p , Daughter1dedx);
	      if(Daughter1EmcSN >= 0) {
		float Daughter1TowerEnergy = hlt_emc->emcTower[Daughter1EmcSN].energy;
		Daughter1_PE_ratio = Daughter1p / Daughter1TowerEnergy;
		Daughter1_EP_ratio = Daughter1TowerEnergy / Daughter1p;
		hDaughter1P_TowerEnergy->Fill(Daughter1_EP_ratio);
		Daughter1phidiff = hlt_node->node[Daughter1NodeSN].emcMatchPhiDiff;
	      }
	      if(Daughter1TofSN >= 0.) {
		Daughter1beta = hlt_node->node[Daughter1NodeSN].beta;
	      }
	      
	      if(Daughter2TrackSN < 0.) continue;
	      hlt_track Daughter2Track =  hlt_pt->primaryTrack[Daughter2TrackSN];
	      float Daughter2phidiff = -999.;
	      float Daughter2beta = -999.;
	      float Daughter2_PE_ratio = -999.;
	      float Daughter2_EP_ratio = -999.;
	      
	      float Daughter2q     =  Daughter2Track.q;
	      float Daughter2px    =  Daughter2Track.pt * cos(Daughter2Track.psi);
	      float Daughter2py    =  Daughter2Track.pt * sin(Daughter2Track.psi);
	      float Daughter2pz    =  Daughter2Track.pt * Daughter2Track.tanl;
	      float Daughter2dedx  =  Daughter2Track.dedx;
	      int Daughter2ndedx = Daughter2Track.ndedx;
	      
	      TVector3 Daughter2(Daughter2px, Daughter2py, Daughter2pz);
	      float Daughter2p = Daughter2.Mag();
	      
	      double dedx2E = getDedx(Daughter2p, e);
	      float nSigma2 = log(Daughter2dedx / dedx2E) / A * sqrt(Daughter2ndedx);
	      
	      float Daughter2phi = Daughter2.Phi();
	      if(Daughter2phi < 0.0) Daughter2phi += twopi;
	      
	      hdEdx_P2->Fill(Daughter2p , Daughter2dedx);
	      if(Daughter2EmcSN >= 0) {
		float Daughter2TowerEnergy = hlt_emc->emcTower[Daughter2EmcSN].energy;
		Daughter2_PE_ratio = Daughter2p / Daughter2TowerEnergy;
		Daughter2_EP_ratio = Daughter2TowerEnergy / Daughter2p;
		hDaughter2P_TowerEnergy->Fill(Daughter2_EP_ratio);
		Daughter2phidiff = hlt_node->node[Daughter2NodeSN].emcMatchPhiDiff;
		}
	      if(Daughter2TofSN >= 0.) {
		Daughter2beta = hlt_node->node[Daughter2NodeSN].beta;
	      }

	      // j/psi
	
	      float px = cos(hlt_diep->ePair[i].psi) * hlt_diep->ePair[i].pt;
	      float py = sin(hlt_diep->ePair[i].psi) * hlt_diep->ePair[i].pt;
	      float pz = hlt_diep->ePair[i].tanl * hlt_diep->ePair[i].pt;
	      float m = hlt_diep->ePair[i].invariantMass;
	      
	      if(Daughter1q * Daughter2q < 0.) {
		hDiElectronInvMassFullRange->Fill(m);
	      }
	      else {
		hDiElectronInvMassFullRangeBG->Fill(m);
	      }
	      
	      if(nSigma1 > -0.9 && nSigma2 > -0.9 &&
		 Daughter1p > 2.3 && Daughter2p > 1.5 &&
		 Daughter1ndedx > 16 && Daughter2ndedx > 16 &&
		 Daughter1_PE_ratio < 1.5 && Daughter1_PE_ratio > 0.5 && Daughter2_PE_ratio < 1.5 && Daughter2_PE_ratio > 0.5 &&
		 Daughter1phidiff > 0. && Daughter1phidiff < 0.05 && Daughter2phidiff > 0. && Daughter2phidiff < 0.05) {
		if(Daughter1q * Daughter2q < 0.)
		  {
		    hDiElectronInvMassTpxEmc->Fill(m);
		    if(fabs(1 / Daughter1beta - 1) < 0.04 && fabs(1 / Daughter2beta - 1) < 0.04)
		      {
			hDiElectronInvMassCut->Fill(m);
		      }
		  }
		else
		  {
		    hDiElectronInvMassTpxEmcBG->Fill(m);
		    if(fabs(1 / Daughter1beta - 1) < 0.04 && fabs(1 / Daughter2beta - 1) < 0.04)
		      {
			hDiElectronInvMassCutBG->Fill(m);
		      }
		    }
		
		if(Daughter1TofSN >= 0.) {
		  hDaughter1TpxEmcInverseBeta->Fill(1 / Daughter1beta);
		}
		if(Daughter2TofSN >= 0.) {
		  hDaughter2TpxEmcInverseBeta->Fill(1 / Daughter2beta);
		  }
	      }
	      
	      TLorentzVector jpsi(0, 0, 0, 0);
	      jpsi.SetXYZM(px, py, pz, m);
	      float rapidity = jpsi.Rapidity();
	      hDiLeptonRapidity->Fill(rapidity);
	    }//nEPair
	    
	  }
	  
	}
};


/**
 * Get dE/dx theoretical value.
 * @param
 * @return
 * @exception
 * @see
 * @author
 */
void l4Builder::inputDedx()
{
	for(int i = 0; i < 8; i++) {
		ifstream ifs(dEdxMeanFiles[i]);
		string tem;
		getline(ifs, tem);

		for(int j = 0; j < 11901; j++) {
			ifs >> tem >> dedxMean[i][j];
			dedxMean[i][j] *= 1.e-06;
		}
	}
}

double l4Builder::getDedx(double p, const int name)
{
	if(p < 0.1) return 0;
	if(p > 12.) {
		if(name == e) return dedxMean[e][n];
		else if(name == Pi) return dedxMean[Pi][n];
		else if(name == K) return dedxMean[K][n];
		else if(name == P) return dedxMean[P][n];
		else if(name == D) return dedxMean[D][n];
		else if(name == T) return dedxMean[T][n];
		else if(name == He3) return dedxMean[He3][n];
		else if(name == He4) return dedxMean[He4][n];
	}

	int lndex = (int)((p - Min) / Bin);
	double dp = p - Min - lndex * Bin;

	if(name == e) return (1. - dp / Bin) * dedxMean[e][lndex] + dp / Bin * dedxMean[e][lndex + 1];
	else if(name == Pi) return (1. - dp / Bin) * dedxMean[Pi][lndex] + dp / Bin * dedxMean[Pi][lndex + 1];
	else if(name == K) return (1. - dp / Bin) * dedxMean[K][lndex] + dp / Bin * dedxMean[K][lndex + 1];
	else if(name == P) return (1. - dp / Bin) * dedxMean[P][lndex] + dp / Bin * dedxMean[P][lndex + 1];
	else if(name == D) return (1. - dp / Bin) * dedxMean[D][lndex] + dp / Bin * dedxMean[D][lndex + 1];
	else if(name == T) return (1. - dp / Bin) * dedxMean[T][lndex] + dp / Bin * dedxMean[T][lndex + 1];
	else if(name == He3) return (1. - dp / Bin) * dedxMean[He3][lndex] + dp / Bin * dedxMean[He3][lndex + 1];
	else if(name == He4) return (1. - dp / Bin) * dedxMean[He4][lndex] + dp / Bin * dedxMean[He4][lndex + 1];
	else return 0;
}

/**
 * Define dE/dx theoretical curve function.
 * @param
 * @return
 * @exception
 * @see
 * @author
 */
static Double_t funcDedx_e_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[e][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[e][lndex] + dp / Bin * dedxMean[e][lndex + 1];
}
static Double_t funcDedx_e_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[e][lndex] + dp / Bin * dedxMean[e][lndex + 1];
}
static Double_t funcDedx_Pi_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[Pi][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[Pi][lndex] + dp / Bin * dedxMean[Pi][lndex + 1];
}
static Double_t funcDedx_Pi_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[Pi][lndex] + dp / Bin * dedxMean[Pi][lndex + 1];
}
static Double_t funcDedx_K_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[K][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[K][lndex] + dp / Bin * dedxMean[K][lndex + 1];
}
static Double_t funcDedx_K_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[K][lndex] + dp / Bin * dedxMean[K][lndex + 1];
}
static Double_t funcDedx_P_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[P][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[P][lndex] + dp / Bin * dedxMean[P][lndex + 1];
}
static Double_t funcDedx_P_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[P][lndex] + dp / Bin * dedxMean[P][lndex + 1];
}
static Double_t funcDedx_D_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[D][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[D][lndex] + dp / Bin * dedxMean[D][lndex + 1];
}
static Double_t funcDedx_D_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[D][lndex] + dp / Bin * dedxMean[D][lndex + 1];
}
static Double_t funcDedx_T_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[T][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[T][lndex] + dp / Bin * dedxMean[T][lndex + 1];
}
static Double_t funcDedx_T_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[T][lndex] + dp / Bin * dedxMean[T][lndex + 1];
}
static Double_t funcDedx_He3_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[He3][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[He3][lndex] + dp / Bin * dedxMean[He3][lndex + 1];
}
static Double_t funcDedx_He3_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[He3][lndex] + dp / Bin * dedxMean[He3][lndex + 1];
}
static Double_t funcDedx_He4_pos(Double_t *x, Double_t *par)
{
	if(x[0] < 0.1) return 0;
	if(x[0] > 12.) return dedxMean[He4][n];
	int lndex = (int)((x[0] - Min) / Bin);
	double dp = x[0] - Min - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[He4][lndex] + dp / Bin * dedxMean[He4][lndex + 1];
}
static Double_t funcDedx_He4_neg(Double_t *x, Double_t *par)
{
	int lndex = (int)((-Min - x[0]) / Bin);
	double dp = -Min - x[0] - lndex * Bin;
	return (1. - dp / Bin) * dedxMean[He4][lndex] + dp / Bin * dedxMean[He4][lndex + 1];
}

/**
 * Define HLT/JPsi/UPC plots for every run, set these plots.
 * @param
 * @return
 * @exception
 * @see
 * @author
 */
void l4Builder::defineHltPlots()
{
	HltPlots[index]->logy = 1;
	hEvtsAccpt = new TH1I("EvtsAccpt", "EvtsAccpt", 6, 0., 6);
	ph = new PlotHisto();
	ph->histo = hEvtsAccpt;
	HltPlots[index]->addHisto(ph);

	// Tracks
	index++; //1
	hnhits = new TH1I("nHits", "nHits", 50, 0, 50);
	ph = new PlotHisto();
	ph->histo = hnhits;
	HltPlots[index]->addHisto(ph);

	index++; //2
	hnDedx = new TH1I("nDedx", "nDedx", 50, 0, 50);
	ph = new PlotHisto();
	ph->histo = hnDedx;
	HltPlots[index]->addHisto(ph);

	index++; //3
	hDcaXy = new TH1D("DcaXy", "DcaXy", 120, -6., 6.);
	ph = new PlotHisto();
	ph->histo = hDcaXy;
	HltPlots[index]->addHisto(ph);

	index++; //4
	hDcaZ = new TH1D("DcaZ", "DcaZ", 120, -6., 6.);
	ph = new PlotHisto();
	ph->histo = hDcaZ;
	HltPlots[index]->addHisto(ph);

	index++; 
	hdEdx = new TH2F("dEdx", "dEdx", 500, -5, 5, 300, 0, 3.e-5);
	HltPlots[index]->setDrawOpts("colz");
	ph = new PlotHisto();
	ph->histo = hdEdx;
	HltPlots[index]->addHisto(ph);

	index++; //5
	hLn_dEdx = new TH1D("Ln_dEdx", "Ln_dEdx", 500, -14, -11.5);
	ph = new PlotHisto();
	ph->histo = hLn_dEdx;
	HltPlots[index]->addHisto(ph);

	// Glob Tracks
	index++;//6
	HltPlots[index]->logy = 1;
	hGlob_Pt = new TH1D("Glob_Pt", "Glob_Pt", 150, 0., 15.);
	ph = new PlotHisto();
	ph->histo = hGlob_Pt;
	HltPlots[index]->addHisto(ph);

	index++;//7
	hGlob_Phi = new TH1D("Glob_Phi", "Glob_Phi", 360, 0., twopi);
	ph = new PlotHisto();
	ph->histo = hGlob_Phi;
	HltPlots[index]->addHisto(ph);

	index++;  //8
	hGlob_Eta = new TH1D("Glob_Eta", "Glob_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hGlob_Eta;
	HltPlots[index]->addHisto(ph);

	index++; //9
	//   HltPlots[index]->optstat = 0;
	HltPlots[index]->setDrawOpts("colz");

	hGlob_dEdx = new TH2F("Glob_dEdx", "Glob_dEdx", 500, -5, 5, 300, 0, 3.e-5);
	ph = new PlotHisto();
	ph->histo = hGlob_dEdx;
	HltPlots[index]->addHisto(ph);

	// Prim Tracks
	index++; //10
	HltPlots[index]->logy = 1;
	hPrim_Pt = new TH1D("Prim_Pt", "Prim_Pt", 150, 0., 15.);
	ph = new PlotHisto();
	ph->histo = hPrim_Pt;
	HltPlots[index]->addHisto(ph);

	index++; //11
	hPrim_Phi = new TH1D("Prim_Phi", "Prim_Phi", 360, 0., twopi);
	ph = new PlotHisto();
	ph->histo = hPrim_Phi;
	HltPlots[index]->addHisto(ph);

	index++; //12
	hPrim_Eta = new TH1D("Prim_Eta", "Prim_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hPrim_Eta;
	HltPlots[index]->addHisto(ph);

	index++; //13
	//   HltPlots[index]->optstat = 0;
	HltPlots[index]->setDrawOpts("colz");

	hPrim_dEdx = new TH2F("Prim_dEdx", "Prim_dEdx", 500, -5, 5, 300, 0, 3.e-5);
	ph = new PlotHisto();
	ph->histo = hPrim_dEdx;
	HltPlots[index]->addHisto(ph);

	// Event
	index++; //14
	hVertexX = new TH1D("VertexX", "VertexX", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hVertexX;
	HltPlots[index]->addHisto(ph);

	index++; //15
	hVertexY = new TH1D("VertexY", "VertexY", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hVertexY;
	HltPlots[index]->addHisto(ph);

	index++; //16
	hVertexZ = new TH1D("VertexZ", "VertexZ", 500, -100., 100.);
	ph = new PlotHisto();
	ph->histo = hVertexZ;
	HltPlots[index]->addHisto(ph);

	index++; //17
	HltPlots[index]->setDrawOpts("colz");
	hVertexXY = new TH2D("VertexXY", "VertexXY", 200, -2, 2, 200, -2, 2);
	ph = new PlotHisto();
	ph->histo = hVertexXY;
	HltPlots[index]->addHisto(ph);

	index++; //18
	hVertexR = new TH1D("VertexR", "VertexR", 200, 0, 4);
	ph = new PlotHisto();
	ph->histo = hVertexR;
	HltPlots[index]->addHisto(ph);

	index++; //19
	hLm_VertexX = new TH1D("Lm_VertexX", "Lm_VertexX", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexX;
	HltPlots[index]->addHisto(ph);

	index++; //20
	hLm_VertexY = new TH1D("Lm_VertexY", "Lm_VertexY", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexY;
	HltPlots[index]->addHisto(ph);

	index++; //21
	hLm_VertexZ = new TH1D("Lm_VertexZ", "Lm_VertexZ", 100, -10., 10.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexZ;
	HltPlots[index]->addHisto(ph);

	index++; //22
	HltPlots[index]->logy = 1;
	hglobalMult = new TH1I("globalMult", "globalMult", 1500, 0, 7000);
	ph = new PlotHisto();
	ph->histo = hglobalMult;
	HltPlots[index]->addHisto(ph);

	index++; //23
	HltPlots[index]->logy = 1;
	hprimaryMult = new TH1I("primaryMult", "primaryMult", 500, 0, 1600);
	ph = new PlotHisto();
	ph->histo = hprimaryMult;
	HltPlots[index]->addHisto(ph);


	// Emc
	index++; //24
	hMatchPhi_Diff = new TH1D("Emc_matchPhiDiff", "Emc_matchPhiDiff", 50, 0., 0.1);
	ph = new PlotHisto();
	ph->histo = hMatchPhi_Diff;
	HltPlots[index]->addHisto(ph);

	index++; //25
	hTowerEnergy = new TH1D("Emc_towerEnergy", "Emc_towerEnergy", 200, 0., 20.);
	ph = new PlotHisto();
	ph->histo = hTowerEnergy;
	HltPlots[index]->addHisto(ph);

	index++; //26
	hTowerDaqId = new TH1I("Emc_towerDaqId", "Emc_towerDaqId", 5000, 0., 5000.);
	ph = new PlotHisto();
	ph->histo = hTowerDaqId;
	HltPlots[index]->addHisto(ph);

	index++; //27
	hTowerSoftId = new TH1I("Emc_towerSoftId", "Emc_towerSoftId", 5000, 0., 5000.);
	ph = new PlotHisto();
	ph->histo = hTowerSoftId;
	HltPlots[index]->addHisto(ph);

	index++; //28
	hzEdge = new TH1D("Emc_zEdge", "Emc_zEdge", 100, 0., 5.);
	ph = new PlotHisto();
	ph->histo = hzEdge;
	HltPlots[index]->addHisto(ph);

	index++; //29
	//   HltPlots[index]->optstat = 0;
	HltPlots[index]->setDrawOpts("colz");
	hTowerEtaPhi = new TH2F("Emc_towerEtaPhi", "Emc_towerEtaPhi", 120, -pi, pi, 40, -1, 1);
	ph = new PlotHisto();
	ph->histo = hTowerEtaPhi;
	HltPlots[index]->addHisto(ph);

	// ToF
	index++; //30
	hLocalZ = new TH1D("Tof_LocalZ", "Tof_LocalZ", 100, -5., 5.0);
	ph = new PlotHisto();
	ph->histo = hLocalZ;
	HltPlots[index]->addHisto(ph);

	index++; //31
	hLocalY = new TH1D("Tof_LocalY", "Tof_LocalY", 300, -15., 15.);
	ph = new PlotHisto();
	ph->histo = hLocalY;
	HltPlots[index]->addHisto(ph);

	index++; //32
	HltPlots[index]->setDrawOpts("colz");
	hInverseBeta = new TH2F("Tof_InverseBeta", "Tof_InverseBeta", 500, 0, 5, 500, 0.0, 5.);
	ph = new PlotHisto();
	ph->histo = hInverseBeta;
	HltPlots[index]->addHisto(ph);

	index++; //33
	HltPlots[index]->setDrawOpts("colz");
	hMatchId_fiberId = new TH2F("Tof_matchId_fireId", "Tof_matchId_fireId", 200, 0, 200, 200, 0, 200);
	ph = new PlotHisto();
	ph->histo = hMatchId_fiberId;
	HltPlots[index]->addHisto(ph);

	hMatchId_fiberId_copy = new TH2F("Tof_matchId_fireId_copy", "Tof_matchId_fireId_copy", 200, 0, 200, 200, 0, 200);

	hMatchId_fiberId_copy2 = new TH2F("Tof_matchId_fireId_copy2", "Tof_matchId_fireId_copy2", 200, 0, 200, 200, 0, 200);

	index++; //34
	HltPlots[index]->setDrawOpts("colz");
	hTrayID_TrgTime = new TH2F("Tof_TrayID_TrgTime", "Tof_TrayID_TrgTime", 124, 0., 124, 400, 900, 1300);
	ph = new PlotHisto();
	ph->histo = hTrayID_TrgTime;
	HltPlots[index]->addHisto(ph);

	index++; //35
	hchannelID = new TH1D("Tof_channelID", "Tof_channelID", 200, 0, 200);
	ph = new PlotHisto();
	ph->histo = hchannelID;
	HltPlots[index]->addHisto(ph);

	index++; //36
	HltPlots[index]->setDrawOpts("colz");
	hVzvpd_Vz = new TH2F("Vzvpd_Vz", "Vzvpd_Vz", 400, -100, 100, 400, -100, 100);
	ph = new PlotHisto();
	ph->histo = hVzvpd_Vz;
	HltPlots[index]->addHisto(ph);

	index++; //37
	hVzDiff = new TH1D("VzDiff", "VzDiff", 200, -20, 20);
	ph = new PlotHisto();
	ph->histo = hVzDiff;
	HltPlots[index]->addHisto(ph);

}

void l4Builder::defineBeamPlots()
{
	index = 0; //0
	BeamPlots[index]->setDrawOpts("p");
	hBeamX = new TH1D("BeamX", "BeamX", 105, 0., 105);
	ph = new PlotHisto();
	ph->histo = hBeamX;
	BeamPlots[index]->addHisto(ph);
	hBeamY = new TH1D("BeamY", "BeamY", 105, 0., 105);
	ph = new PlotHisto();
	ph->histo = hBeamY;
	BeamPlots[index]->addHisto(ph);

	index++; //1
	BeamPlots[index]->setDrawOpts("p");
	hInnerGain = new TH1D("innerGain", "innerGain", 105, 0., 105);
	ph = new PlotHisto();
	ph->histo = hInnerGain;
	BeamPlots[index]->addHisto(ph);
	hOuterGain = new TH1D("outerGain", "outerGain", 105, 0., 105);
	ph = new PlotHisto();
	ph->histo = hOuterGain;
	BeamPlots[index]->addHisto(ph);

	index++; //2
	BeamPlots[index]->setDrawOpts("p");
	hMeanDcaXy = new TH1D("meanDcaXy", "meanDcaXy", 105, 0., 105);
	ph = new PlotHisto();
	ph->histo = hMeanDcaXy;
	BeamPlots[index]->addHisto(ph);

}

void l4Builder::defineBesGoodPlots()
{
	index=0;
	hBesGoodVertexXY = new TH2D("BesGood_VertexXY","BesGood_VertexXY",200,-5,5,200,-5,5);
	ph = new PlotHisto();
	ph->histo = hBesGoodVertexXY;
	BesGoodPlots[index]->addHisto(ph);

	index++; //1
	hBesGoodVertexZ = new TH1D("BesGood_VertexZ","BesGood_VertexZ",100,-200.,200.);
	ph = new PlotHisto();
	ph->histo = hBesGoodVertexZ;
	BesGoodPlots[index]->addHisto(ph);

	index++; //2
	hBesGoodVr = new TH1D("BesGood_Vr","BesGood_Vr",100,0,10);
	ph = new PlotHisto();
	ph->histo = hBesGoodVr;
	BesGoodPlots[index]->addHisto(ph);

	index++; //3
	BesGoodPlots[index]->logy=1;
	hBesGoodprimaryMult = new TH1I("BesGood_primaryMult", "BesGood_primaryMult",500,0,1600);
	ph = new PlotHisto();
	ph->histo = hBesGoodprimaryMult;
	BesGoodPlots[index]->addHisto(ph);

	index++; //4
	hBesGoodVrVsVz = new TH2D("BesGood_VrVsVz","BesGood_VrVsVz",1000,-150,150,400,0,10);
        ph = new PlotHisto();
        ph->histo = hBesGoodVrVsVz;
        BesGoodPlots[index]->addHisto(ph);
}

void l4Builder::defineHLTGood2Plots()
{
	index=0;
	hHLTGood2VertexXY = new TH2D("HLTGood2_VertexXY","HLTGood2_VertexXY",200,-5,5,200,-5,5);
	ph = new PlotHisto();
	ph->histo = hHLTGood2VertexXY;
	HLTGood2Plots[index]->addHisto(ph);

	index++; //1
	hHLTGood2VertexZ = new TH1D("HLTGood2_VertexZ","HLTGood2_VertexZ",100,-10.,10.);
	ph = new PlotHisto();
	ph->histo = hHLTGood2VertexZ;
	HLTGood2Plots[index]->addHisto(ph);

	index++; //2
	hHLTGood2Vr = new TH1D("HLTGood2_Vr","HLTGood2_Vr",100,0,5);
	ph = new PlotHisto();
	ph->histo = hHLTGood2Vr;
	HLTGood2Plots[index]->addHisto(ph);

	index++; //3
	HLTGood2Plots[index]->logy=1;
	hHLTGood2primaryMult = new TH1I("HLTGood2_primaryMult", "HLTGood2_primaryMult",500,0,1600);
	ph = new PlotHisto();
	ph->histo = hHLTGood2primaryMult;
	HLTGood2Plots[index]->addHisto(ph);
}

void l4Builder::defineBesMontinorPlots()
{
	index = 0;
	BesMontinorPlots[index]->setDrawOpts("colz");
	hBesMonitorVertexXY = new TH2D("BesMonitor_VertexXY", "BesMonitor_VertexXY", 200, -5, 5, 200, -5, 5);
	ph = new PlotHisto();
	ph->histo = hBesMonitorVertexXY;
	BesMontinorPlots[index]->addHisto(ph);

	index++; //1
	hBesMonitorVr = new TH1D("BesMonitor_Vr", "BesMonitor_Vr", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hBesMonitorVr;
	BesMontinorPlots[index]->addHisto(ph);
}

void l4Builder::defineFixedTargetPlots()
{
	index = 0;
	FixedTargetPlots[index]->setDrawOpts("colz");
	hFixedTargetVertexXY = new TH2D("FixedTarget_VertexXY", "FixedTarget_VertexXY", 200, -5, 5, 200, -5, 5);
	ph = new PlotHisto();
	ph->histo = hFixedTargetVertexXY;
	FixedTargetPlots[index]->addHisto(ph);

	index++; //1
	hFixedTargetVr = new TH1D("FixedTarget_Vr", "FixedTarget_Vr", 300, 0, 10);
	ph = new PlotHisto();
	ph->histo = hFixedTargetVr;
	FixedTargetPlots[index]->addHisto(ph);

	index++; //2
	hFixedTarget_VertexZ = new TH1D("FixedTarget_VertexZ","FixedTarget_VertexZ",1000,-200.,200.);
	ph = new PlotHisto();
	ph->histo = hFixedTarget_VertexZ;
	FixedTargetPlots[index]->addHisto(ph);

	index++; //3
	hFixedTarget_Prim_Eta = new TH1D("FixedTarget_Prim_Eta", "FixedTarget_Prim_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hFixedTarget_Prim_Eta;
	FixedTargetPlots[index]->addHisto(ph);

	index++;  //4
	hFixedTarget_Glob_Eta = new TH1D("FixedTarget_Glob_Eta", "FixedTarget_Glob_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hFixedTarget_Glob_Eta;
	FixedTargetPlots[index]->addHisto(ph);
}

void l4Builder::defineFixedTargetMonitorPlots()
{
	index = 0;
	FixedTargetMonitorPlots[index]->setDrawOpts("colz");
	hFixedTargetMonitorVertexXY = new TH2D("FixedTargetMonitor_VertexXY", "FixedTargetMonitor_VertexXY", 200, -5, 5, 200, -5, 5);
	ph = new PlotHisto();
	ph->histo = hFixedTargetMonitorVertexXY;
	FixedTargetMonitorPlots[index]->addHisto(ph);

	index++; //1
	hFixedTargetMonitorVr = new TH1D("FixedTargetMonitor_Vr", "FixedTargetMonitor_Vr", 300, 0, 10);
	ph = new PlotHisto();
	ph->histo = hFixedTargetMonitorVr;
	FixedTargetMonitorPlots[index]->addHisto(ph);

	index++; //2
	hFixedTargetMonitor_VertexZ = new TH1D("FixedTargetMonitor_VertexZ","FixedTargetMonitor_VertexZ",1000,-200.,200.);
	ph = new PlotHisto();
	ph->histo = hFixedTargetMonitor_VertexZ;
	FixedTargetMonitorPlots[index]->addHisto(ph);

	index++; //3
	hFixedTargetMonitor_Prim_Eta = new TH1D("FixedTargetMonitor_Prim_Eta", "FixedTargetMonitor_Prim_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hFixedTargetMonitor_Prim_Eta;
	FixedTargetMonitorPlots[index]->addHisto(ph);

	index++;  //4
	hFixedTargetMonitor_Glob_Eta = new TH1D("FixedTargetMonitor_Glob_Eta", "FixedTargetMonitor_Glob_Eta", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hFixedTargetMonitor_Glob_Eta;
	FixedTargetMonitorPlots[index]->addHisto(ph);
}

void l4Builder::defineHeavyFragmentPlots()
{
	index = 0;
	HeavyFragmentPlots[index]->setDrawOpts("colz");
	HeavyFragmentPlots[index]->gridx = 1;
	HeavyFragmentPlots[index]->gridy = 1;
	hHFM_dEdx = new TH2F("HeavyFragment_dEdx", "HeavyFragment_dEdx", 500, -5, 5, 300, 0, 3.e-5);
	ph = new PlotHisto();
	ph->histo = hHFM_dEdx;
	HeavyFragmentPlots[index]->addHisto(ph);
	ph = new PlotHisto();
	ph->histo = hdEdx;
	HeavyFragmentPlots[index]->addHisto(ph);
}
void l4Builder::defineDiElectronPlots() // not only J/Psi, but di-pion, di-muon
{
	// jpsi invariant mass
	index = 0;
	hDiElectronInvMassTpxEmc = new TH1D("DiElectronInvMassTpxEmc", "DiElectronInvMassTpxEmc", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassTpxEmc;
	DiElectronPlots[index]->addHisto(ph);
	hDiElectronInvMassTpxEmcBG = new TH1D("DiElectronInvMassTpxEmcBG", "DiElectronInvMassTpxEmcBG", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassTpxEmcBG;
	DiElectronPlots[index]->addHisto(ph);
	TLegend *lega15 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega15->AddEntry(hDiElectronInvMassTpxEmc, "Unlike Sign", "l");
        lega15->AddEntry(hDiElectronInvMassTpxEmcBG, "Like Sign", "l");
        DiElectronPlots[index]->addElement(lega15);
	

	index++; //1
	hDiElectronInvMassCut = new TH1D("DiElectronInvMassCut", "DiElectronInvMassCut", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassCut;
	DiElectronPlots[index]->addHisto(ph);
	hDiElectronInvMassCutBG = new TH1D("DiElectronInvMassCutBG", "DiElectronInvMassCutBG", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassCutBG;
	DiElectronPlots[index]->addHisto(ph);
	TLegend *lega16 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega16->AddEntry(hDiElectronInvMassCut, "Unlike Sign", "l");
        lega16->AddEntry(hDiElectronInvMassCutBG, "Like Sign", "l");
        DiElectronPlots[index]->addElement(lega16);

	index++; //2
	hDiElectronInvMassFullRange = new TH1D("DiElectronInvMassFullRange", "DiElectronInvMassFullRange", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRange;
	DiElectronPlots[index]->addHisto(ph);
	hDiElectronInvMassFullRangeBG = new TH1D("DiElectronInvMassFullRangeBG", "DiElectronInvMassFullRangeBG", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRangeBG;
	DiElectronPlots[index]->addHisto(ph);
	TLegend *lega17 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega17->AddEntry(hDiElectronInvMassFullRange, "Unlike Sign", "l");
        lega17->AddEntry(hDiElectronInvMassFullRangeBG, "Like Sign", "l");
        DiElectronPlots[index]->addElement(lega17);

	// daug e1
	index++; //3
	DiElectronPlots[index]->setDrawOpts("colz");
	hdEdx_P1 = new TH2F("dEdx_P1", "dEdx_P1", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P1;
	DiElectronPlots[index]->addHisto(ph);

	index++; //4
	hDaughter1P_TowerEnergy = new TH1D("Daughter1P_TowerEnergy", "Daughter1P_TowerEnergy", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter1P_TowerEnergy;
	DiElectronPlots[index]->addHisto(ph);

	index++; //5
	hDaughter1TpxEmcInverseBeta = new TH1D("Daughter1TpxEmcInverseBeta", "Daughter1TpxEmcInverseBeta", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo =  hDaughter1TpxEmcInverseBeta;
	DiElectronPlots[index]->addHisto(ph);

	// daug e2
	index++; //6
	DiElectronPlots[index]->setDrawOpts("colz");
	hdEdx_P2 = new TH2F("dEdx_P2", "dEdx_P2", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P2;
	DiElectronPlots[index]->addHisto(ph);

	index++; //7
	hDaughter2P_TowerEnergy = new TH1D("Daughter2P_TowerEnergy", "Daughter2P_TowerEnergy", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter2P_TowerEnergy;
	DiElectronPlots[index]->addHisto(ph);

	index++; //8
	hDaughter2TpxEmcInverseBeta = new TH1D("Daughter2TpxEmcInverseBeta", "Daughter2TpxEmcInverseBeta", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo =  hDaughter2TpxEmcInverseBeta;
	DiElectronPlots[index]->addHisto(ph);

	index++; //9
	hDiLeptonRapidity = new TH1D("DiLeptonRapidity", "DiLeptonRapidity", 150, -7.5, 7.5);
	ph = new PlotHisto();
	ph->histo = hDiLeptonRapidity;
	DiElectronPlots[index]->addHisto(ph);
}

void l4Builder::defineDiElectron2TwrPlots() // not only J/Psi, but di-pion, di-muon
{
	// jpsi invariant mass
	index = 0;
	hDiElectronInvMassTpxEmc_Twr = new TH1D("DiElectronInvMassTpxEmc_Twr", "DiElectronInvMassTpxEmc_Twr", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassTpxEmc_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	hDiElectronInvMassTpxEmcBG_Twr = new TH1D("DiElectronInvMassTpxEmcBG_Twr", "DiElectronInvMassTpxEmcBG_Twr", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassTpxEmcBG_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	TLegend *lega18 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega18->AddEntry(hDiElectronInvMassTpxEmc_Twr, "Unlike Sign", "l");
        lega18->AddEntry(hDiElectronInvMassTpxEmcBG_Twr, "Like Sign", "l");
        DiElectron2TwrPlots[index]->addElement(lega18);

	index++; //1
	hDiElectronInvMassCut_Twr = new TH1D("DiElectronInvMassCut_Twr", "DiElectronInvMassCut_Twr", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassCut_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	hDiElectronInvMassCutBG_Twr = new TH1D("DiElectronInvMassCutBG_Twr", "DiElectronInvMassCutBG_Twr", 120, 1., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassCutBG_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	TLegend *lega19 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega19->AddEntry(hDiElectronInvMassCut_Twr, "Unlike Sign", "l");
        lega19->AddEntry(hDiElectronInvMassCutBG_Twr, "Like Sign", "l");
        DiElectron2TwrPlots[index]->addElement(lega19);


	index++; //2
	hDiElectronInvMassFullRange_Twr = new TH1D("DiElectronInvMassFullRange_Twr", "DiElectronInvMassFullRange_Twr", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRange_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	hDiElectronInvMassFullRangeBG_Twr = new TH1D("DiElectronInvMassFullRangeBG_Twr", "DiElectronInvMassFullRangeBG_Twr", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRangeBG_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
	TLegend *lega20 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega20->AddEntry(hDiElectronInvMassFullRange_Twr, "Unlike Sign", "l");
        lega20->AddEntry(hDiElectronInvMassFullRangeBG_Twr, "Like Sign", "l");
        DiElectron2TwrPlots[index]->addElement(lega20);

	// daug e1
	index++; //3
	DiElectron2TwrPlots[index]->setDrawOpts("colz");
	hdEdx_P1_Twr = new TH2F("dEdx_P1_Twr", "dEdx_P1_Twr", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P1_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	index++; //4
	hDaughter1P_TowerEnergy_Twr = new TH1D("Daughter1P_TowerEnergy_Twr", "Daughter1P_TowerEnergy_Twr", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter1P_TowerEnergy_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	index++; //5
	hDaughter1TpxEmcInverseBeta_Twr = new TH1D("Daughter1TpxEmcInverseBeta_Twr", "Daughter1TpxEmcInverseBeta_Twr", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo =  hDaughter1TpxEmcInverseBeta_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	// daug e2
	index++; //6
	DiElectron2TwrPlots[index]->setDrawOpts("colz");
	hdEdx_P2_Twr = new TH2F("dEdx_P2_Twr", "dEdx_P2_Twr", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P2_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	index++; //7
	hDaughter2P_TowerEnergy_Twr = new TH1D("Daughter2P_TowerEnergy_Twr", "Daughter2P_TowerEnergy_Twr", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter2P_TowerEnergy_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	index++; //8
	hDaughter2TpxEmcInverseBeta_Twr = new TH1D("Daughter2TpxEmcInverseBeta_Twr", "Daughter2TpxEmcInverseBeta_Twr", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo =  hDaughter2TpxEmcInverseBeta_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);

	index++; //9
	hDiLeptonRapidity_Twr = new TH1D("DiLeptonRapidity_Twr", "DiLeptonRapidity_Twr", 150, -7.5, 7.5);
	ph = new PlotHisto();
	ph->histo = hDiLeptonRapidity_Twr;
	DiElectron2TwrPlots[index]->addHisto(ph);
}




void l4Builder::defineDiPionPlots()
{
	index = 0; //0
	hDiPionInvMassFullRange = new TH1D("DiPionInvMassFullRange ", "DiPionInvMassFullRange", 130, 0., 1.3);
	ph = new PlotHisto();
	ph->histo = hDiPionInvMassFullRange;
	DiPionPlots[index]->addHisto(ph);
	hDiPionInvMassFullRangeBG = new TH1D("DiPionInvMassFullRangeBG ", "DiPionInvMassFullRangeBG", 130, 0., 1.3);
	ph = new PlotHisto();
	ph->histo = hDiPionInvMassFullRangeBG;
	DiPionPlots[index]->addHisto(ph);

	index++; //1
	hDiPionDeltphi = new TH1D("DiPionDeltphi", "DiPionDeltphi", 120, -pi, pi);
	ph = new PlotHisto();
	ph->histo = hDiPionDeltphi;
	DiPionPlots[index]->addHisto(ph);
}

void l4Builder::defineDiMuonPlots()
{

	index = 0; //0
	hInvMassLS = new TH1F("hInvMassLS","Invariant mass of di-muon pairs (LS);M_{#mu#mu} (GeV/c)^{2};counts",50,2.5,3.5);
	ph = new PlotHisto();
	ph->histo = hInvMassLS;
	DiMuonPlots[index]->addHisto(ph);
	hInvMassUS = new TH1F("hInvMassUS","Invariant mass of di-muon pairs (US);M_{#mu#mu} (GeV/c)^{2};counts",50,2.5,3.5);
	ph = new PlotHisto();
	ph->histo = hInvMassUS;
	DiMuonPlots[index]->addHisto(ph);
	TLegend *lega21 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
        lega21->AddEntry(hInvMassUS, "Unlike Sign", "l");
        lega21->AddEntry(hInvMassLS, "Like Sign", "l");
        DiMuonPlots[index]->addElement(lega21);

	index++; //1 
	DiMuonPlots[index]->setDrawOpts("colz");
	hMtdHitMap = new TH2F("hMtdHitMap","MTD: channel vs backleg of hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
	ph = new PlotHisto();
	ph->histo = hMtdHitMap;
	DiMuonPlots[index]->addHisto(ph);

	index++; //2
	DiMuonPlots[index]->setDrawOpts("colz");
	hMtdMatchHitMap    = new TH2F("hMtdMatchHitMap","MTD: channel vs backleg of matched hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
	ph = new PlotHisto();
	ph->histo = hMtdMatchHitMap;
	DiMuonPlots[index]->addHisto(ph);

	index++; //3
	DiMuonPlots[index]->setDrawOpts("colz");
	hMtdDeltaZvsModule = new TH2F("hMtdDeltaZvsModule","#Deltaz of matched track-hit pair in each module;module;#deltaz (cm)",150,0.5,150.5,100,-50,50);
	ph = new PlotHisto();
	ph->histo = hMtdDeltaZvsModule;
	DiMuonPlots[index]->addHisto(ph);

	index++; //4
	hMtdDeltaZ         = new TH1F("hMtdDeltaZ","#Deltaz of matched track-hit pair;#deltaz (cm)",200,-100,100);
	ph = new PlotHisto();
	ph->histo = hMtdDeltaZ;
	DiMuonPlots[index]->addHisto(ph);

	index++; //5
	DiMuonPlots[index]->setDrawOpts("colz");
	hMtdDeltaYvsModule = new TH2F("hMtdDeltaYvsModule","#Deltay of matched track-hit pair in each module;module;#deltay (cm)",150,0.5,150.5,200,-50,50);
	ph = new PlotHisto();
	ph->histo = hMtdDeltaYvsModule;
	DiMuonPlots[index]->addHisto(ph);

	index++; //6
	hMtdDeltaY = new TH1F("hMtdDeltaY","#Deltay of matched track-hit pair;#deltay (cm)",200,-50,50);
	ph = new PlotHisto();
	ph->histo = hMtdDeltaY;
	DiMuonPlots[index]->addHisto(ph);

	index ++; //7
	hMTDQmInvMassUS = new TH1F("hMTDQmInvMassUS", "MTD quarkonium InvMass", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hMTDQmInvMassUS;
	DiMuonPlots[index]->addHisto(ph);

	hMTDQmInvMassLS = new TH1F("hMTDQmInvMassLS", "MTD quarkonium InvMass like sign", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hMTDQmInvMassLS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega->AddEntry(hMTDQmInvMassUS, "Unlike Sign", "l");
	lega->AddEntry(hMTDQmInvMassLS, "Like Sign",  "l");
	DiMuonPlots[index]->addElement(lega);

	index ++; //8
	hMTDQmJpsiMass_ptcut0_US = new TH1F("hMTDQmJpsiMass_ptcut0_US", "MTD quarkonium InvMass(Jpsi)", 20, 2.0, 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut0_US;
	DiMuonPlots[index]->addHisto(ph);

	hMTDQmJpsiMass_ptcut0_LS = new TH1F("hMTDQmJpsiMass_ptcut0_LS", "MTD quarkonium InvMass like sign", 20, 2., 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut0_LS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega8 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega8->AddEntry(hMTDQmJpsiMass_ptcut0_US, "Unlike Sign", "l");
	lega8->AddEntry(hMTDQmJpsiMass_ptcut0_LS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega8);

	tlx8_us = new TLatex();
	tlx8_us->SetNDC();
	tlx8_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx8_us);
	tlx8_ls = new TLatex();
	tlx8_ls->SetNDC();
	tlx8_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx8_ls);


	index ++; //9
	hMTDQmJpsiMass_ptcut2_US = new TH1F("hMTDQmJpsiMass_ptcut2_US", "MTD quarkonium InvMass(Jpsi_ptcut2)", 20, 2.0, 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut2_US;
	DiMuonPlots[index]->addHisto(ph);

	hMTDQmJpsiMass_ptcut2_LS = new TH1F("hMTDQmJpsiMass_ptcut2_LS", "MTD quarkonium InvMass like sign", 20, 2., 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut2_LS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega9 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega9->AddEntry(hMTDQmJpsiMass_ptcut2_US, "Unlike Sign", "l");
	lega9->AddEntry(hMTDQmJpsiMass_ptcut2_LS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega9);


	tlx9_us = new TLatex();
	tlx9_us->SetNDC();
	tlx9_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx9_us);
	tlx9_ls = new TLatex();
	tlx9_ls->SetNDC();
	tlx9_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx9_ls);




	index ++; //10
	hMTDQmJpsiMass_ptcut4_US = new TH1F("hMTDQmJpsiMass_ptcut4_US", "MTD quarkonium InvMass(Jpsi_ptcut4)", 20, 2.0, 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut4_US;
	DiMuonPlots[index]->addHisto(ph);

	hMTDQmJpsiMass_ptcut4_LS = new TH1F("hMTDQmJpsiMass_ptcut4_LS", "MTD quarkonium InvMass like sign", 20, 2., 4.);
	ph = new PlotHisto();
	ph->histo = hMTDQmJpsiMass_ptcut4_LS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega10 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega10->AddEntry(hMTDQmJpsiMass_ptcut4_US, "Unlike Sign", "l");
	lega10->AddEntry(hMTDQmJpsiMass_ptcut4_LS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega10);


	tlx10_us = new TLatex();
	tlx10_us->SetNDC();
	tlx10_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx10_us);
	tlx10_ls = new TLatex();
	tlx10_ls->SetNDC();
	tlx10_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx10_ls);


	index ++; //11
	hMTDQmUpsilonMassUS = new TH1F("hMTDQmUpsilonMassUS", "MTD quarkonium InvMass(Upsilon)", 25, 8.0, 13.);
	ph = new PlotHisto();
	ph->histo = hMTDQmUpsilonMassUS;
	DiMuonPlots[index]->addHisto(ph);

	hMTDQmUpsilonMassLS = new TH1F("hMTDQmUpsilonMassLS", "MTD quarkonium InvMass like sign", 25, 8., 13.);
	ph = new PlotHisto();
	ph->histo = hMTDQmUpsilonMassLS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega11 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega11->AddEntry(hMTDQmUpsilonMassUS, "Unlike Sign", "l");
	lega11->AddEntry(hMTDQmUpsilonMassLS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega11);


	tlx11_us = new TLatex();
	tlx11_us->SetNDC();
	tlx11_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx11_us);
	tlx11_ls = new TLatex();
	tlx11_ls->SetNDC();
	tlx11_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx11_ls);

	index ++; //12
	hMTDDiMuonJpsiMassUS = new TH1F("hMTDDiMuonJpsiMassUS", "MTD DiMuon InvMass(Jpsi)", 20, 2.0, 4.);
	ph = new PlotHisto();
	ph->histo = hMTDDiMuonJpsiMassUS;
	DiMuonPlots[index]->addHisto(ph);

	hMTDDiMuonJpsiMassLS = new TH1F("hMTDDiMuonJpsiMassLS", "MTD DiMuon InvMass like sign", 20, 2., 4.);
	ph = new PlotHisto();
	ph->histo = hMTDDiMuonJpsiMassLS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega12 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega12->AddEntry(hMTDDiMuonJpsiMassUS, "Unlike Sign", "l");
	lega12->AddEntry(hMTDDiMuonJpsiMassLS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega12);

	tlx12_us = new TLatex();
	tlx12_us->SetNDC();
	tlx12_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx12_us);
	tlx12_ls = new TLatex();
	tlx12_ls->SetNDC();
	tlx12_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx12_ls);
	tlxmass12 = new TLatex();
	tlxmass12->SetNDC();
	tlxmass12->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlxmass12);

	index ++; //13
	hMTDDiMuonUpsilonMassUS = new TH1F("hMTDDiMuonUpsilonMassUS", "MTD DiMuon InvMass(Upsilon)", 25, 8.0, 13.);
	ph = new PlotHisto();
	ph->histo = hMTDDiMuonUpsilonMassUS;
	DiMuonPlots[index]->addHisto(ph);

	hMTDDiMuonUpsilonMassLS = new TH1F("hMTDDiMuonUpsilonMassLS", "MTD DiMuon InvMass like sign", 25, 8., 13.);
	ph = new PlotHisto();
	ph->histo = hMTDDiMuonUpsilonMassLS;
	DiMuonPlots[index]->addHisto(ph);

	TLegend *lega13 = new TLegend(0.1, 0.75, 0.35, 0.90,NULL,"brNDC");
	lega13->AddEntry(hMTDDiMuonUpsilonMassUS, "Unlike Sign", "l");
	lega13->AddEntry(hMTDDiMuonUpsilonMassLS, "Like Sign", "l");
	DiMuonPlots[index]->addElement(lega13);

	tlx13_us = new TLatex();
	tlx13_us->SetNDC();
	tlx13_us->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx13_us);
	tlx13_ls = new TLatex();
	tlx13_ls->SetNDC();
	tlx13_ls->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlx13_ls);
	tlxmass13 = new TLatex();
	tlxmass13->SetNDC();
	tlxmass13->SetTextSize(0.05);
	DiMuonPlots[index]->addElement(tlxmass13);

}


void l4Builder::defineHltPlots_UPC()
{
        index = 0;//0
	hnhits_UPC = new TH1I("nHits_UPC", "nHits_UPC", 50, 0, 50);
	ph = new PlotHisto();
	ph->histo = hnhits_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //1
	hnDedx_UPC = new TH1I("nDedx_UPC", "nDedx_UPC", 50, 0, 50);
	ph = new PlotHisto();
	ph->histo = hnDedx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //2
	hDcaXy_UPC = new TH1D("DcaXy_UPC", "DcaXy_UPC", 120, -6., 6.);
	ph = new PlotHisto();
	ph->histo = hDcaXy_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //3
	hDcaZ_UPC = new TH1D("DcaZ_UPC", "DcaZ_UPC", 120, -6., 6.);
	ph = new PlotHisto();
	ph->histo = hDcaZ_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //4
	hLn_dEdx_UPC = new TH1D("Ln_dEdx_UPC", "Ln_dEdx_UPC", 500, -14, -11.5);// previous range is -13.3, -12.3;
	ph = new PlotHisto();
	ph->histo = hLn_dEdx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //5
	HltPlots_UPC[index]->logy = 1;
	hGlob_Pt_UPC = new TH1D("Glob_Pt_UPC", "Glob_Pt_UPC", 150, 0., 15.);
	ph = new PlotHisto();
	ph->histo = hGlob_Pt_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //6
	hGlob_Phi_UPC = new TH1D("Glob_Phi_UPC", "Glob_Phi_UPC", 360, 0., twopi);
	ph = new PlotHisto();
	ph->histo = hGlob_Phi_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //7
	hGlob_Eta_UPC = new TH1D("Glob_Eta_UPC", "Glob_Eta_UPC", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hGlob_Eta_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //8
	HltPlots_UPC[index]->setDrawOpts("colz");
	hGlob_dEdx_UPC = new TH2F("Glob_dEdx_UPC", "Glob_dEdx_UPC", 200, -5, 5, 100, 0, 1.e-5);
	ph = new PlotHisto();
	ph->histo = hGlob_dEdx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //9
	HltPlots_UPC[index]->logy = 1;
	hPrim_Pt_UPC = new TH1D("Prim_Pt_UPC", "Prim_Pt_UPC", 150, 0., 15.);
	ph = new PlotHisto();
	ph->histo = hPrim_Pt_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //10
	hPrim_Phi_UPC = new TH1D("Prim_Phi_UPC", "Prim_Phi_UPC", 360, 0., twopi);
	ph = new PlotHisto();
	ph->histo = hPrim_Phi_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //11
	hPrim_Eta_UPC = new TH1D("Prim_Eta_UPC", "Prim_Eta_UPC", 120, -3, 3);
	ph = new PlotHisto();
	ph->histo = hPrim_Eta_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //12
	HltPlots_UPC[index]->setDrawOpts("colz");
	hPrim_dEdx_UPC = new TH2F("Prim_dEdx_UPC", "Prim_dEdx_UPC", 200, -5, 5, 100, 0, 1.e-5);
	ph = new PlotHisto();
	ph->histo = hPrim_dEdx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //13
	hVertexX_UPC = new TH1D("VertexX_UPC", "VertexX_UPC", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hVertexX_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //14
	hVertexY_UPC = new TH1D("VertexY_UPC", "VertexY_UPC", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hVertexY_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //15
	hVertexZ_UPC = new TH1D("VertexZ_UPC", "VertexZ_UPC", 1000, -200., 200.);
	ph = new PlotHisto();
	ph->histo = hVertexZ_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //16
	hLm_VertexX_UPC = new TH1D("Lm_VertexX_UPC", "Lm_VertexX_UPC", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexX_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //17
	hLm_VertexY_UPC = new TH1D("Lm_VertexY_UPC", "Lm_VertexY_UPC", 200, -2., 2.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexY_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //18
	hLm_VertexZ_UPC = new TH1D("Lm_VertexZ_UPC", "Lm_VertexZ_UPC", 1000, -200., 200.);
	ph = new PlotHisto();
	ph->histo = hLm_VertexZ_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //19
	HltPlots_UPC[index]->logy = 1;
	hglobalMult_UPC = new TH1I("globalMult_UPC", "globalMult_UPC", 1500, 0, 7000);
	ph = new PlotHisto();
	ph->histo = hglobalMult_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //20
	HltPlots_UPC[index]->logy = 1;
	hprimaryMult_UPC = new TH1I("primaryMult_UPC", "primaryMult_UPC", 500, 0, 1600);
	ph = new PlotHisto();
	ph->histo = hprimaryMult_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //21
	hMatchPhi_Diff_UPC = new TH1D("Emc_matchPhiDiff_UPC", "Emc_matchPhiDiff_UPC", 50, 0., 0.1);
	ph = new PlotHisto();
	ph->histo = hMatchPhi_Diff_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //22
	hTowerEnergy_UPC = new TH1D("Emc_towerEnergy_UPC", "Emc_towerEnergy_UPC", 200, 0., 20.);
	ph = new PlotHisto();
	ph->histo = hTowerEnergy_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //23
	hTowerDaqId_UPC = new TH1I("Emc_towerDaqId_UPC", "Emc_towerDaqId_UPC", 5000, 0., 5000.);
	ph = new PlotHisto();
	ph->histo = hTowerDaqId_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //24
	hTowerSoftId_UPC = new TH1I("Emc_towerSoftId_UPC", "Emc_towerSoftId_UPC", 5000, 0., 5000.);
	ph = new PlotHisto();
	ph->histo = hTowerSoftId_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //25
	hzEdge_UPC = new TH1D("Emc_zEdge_UPC", "Emc_zEdge_UPC", 100, 0., 5.);
	ph = new PlotHisto();
	ph->histo = hzEdge_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++; //26
	HltPlots_UPC[index]->setDrawOpts("colz");
	hTowerEtaPhi_UPC = new TH2F("Emc_towerEtaPhi_UPC", "Emc_towerEtaPhi_UPC", 120, -pi, pi, 40, -1, 1);
	ph = new PlotHisto();
	ph->histo = hTowerEtaPhi_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++;//27
	HltPlots_UPC[index]->setDrawOpts("colz");
	hVzvpd_Vz_UPC = new TH2F("Vzvpd_Vz_UPC", "Vzvpd_Vz_UPC", 400, -100, 100, 400, -100, 100);
	ph = new PlotHisto();
	ph->histo = hVzvpd_Vz_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++;//28
	HltPlots_UPC[index]->setDrawOpts("colz");
	HltPlots_UPC[index]->gridx = 1;
	HltPlots_UPC[index]->gridy = 1;
	hdEdx_UPC = new TH2F("dEdx_UPC", "dEdx_UPC", 500, -5, 5, 300, 0, 3.e-5);
	ph = new PlotHisto();
	ph->histo = hdEdx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	hHFM_dEdx_UPC = new TH2F("HFM_dEdx_UPC", "HFM_dEdx_UPC", 500, -5, 5, 300, 0, 3.e-5);
	ph = new PlotHisto();
	ph->histo = hHFM_dEdx_UPC;
	HltPlots_UPC[index]->addHisto(ph);

	index++;//29
	hVzDiff_UPC = new TH1D("VzDiff_UPC", "VzDiff_UPC", 200, -20, 20);
	ph = new PlotHisto();
	ph->histo = hVzDiff_UPC;
	HltPlots_UPC[index]->addHisto(ph); 

}

void l4Builder::defineUPCDiElectronPlots()
{
	index = 0; //0
	hDiElectronInvMassFullRange_UPC = new TH1D("DiElectronInvMassFullRange_UPC ", "DiElectronInvMassFullRange_UPC", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRange_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);
	hDiElectronInvMassFullRangeBG_UPC = new TH1D("DiElectronInvMassFullRangeBG_UPC", "DiElectronInvMassFullRangeBG_UPC", 130, 0., 13.);
	ph = new PlotHisto();
	ph->histo = hDiElectronInvMassFullRangeBG_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);

	index++; //1
	UPCDiElectronPlots[index]->setDrawOpts("colz");
	hdEdx_P1_UPC = new TH2F("dEdx_P1_UPC", "dEdx_P1_UPC", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P1_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);

	index++; //2
	hDaughter1P_TowerEnergy_UPC = new TH1D("Daughter1P_TowerEnergy_UPC", "Daughter1P_TowerEnergy_UPC", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter1P_TowerEnergy_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);

	index++; //3
	UPCDiElectronPlots[index]->setDrawOpts("colz");
	hdEdx_P2_UPC = new TH2F("dEdx_P2_UPC", "dEdx_P2_UPC", 200, 0., 10., 55, 0., 5.5e-06);
	ph = new PlotHisto();
	ph->histo = hdEdx_P2_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);

	index++; //4
	hDaughter2P_TowerEnergy_UPC = new TH1D("Daughter2P_TowerEnergy_UPC", "Daughter2P_TowerEnergy_UPC", 100, 0, 5);
	ph = new PlotHisto();
	ph->histo = hDaughter2P_TowerEnergy_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);

	index++;//5
	hDiLeptonRapidity_UPC = new TH1D("DiLeptonRapidity_UPC", "DiLeptonRapidity_UPC", 150, -7.5, 7.5);
	ph = new PlotHisto();
	ph->histo = hDiLeptonRapidity_UPC;
	UPCDiElectronPlots[index]->addHisto(ph);
}

void l4Builder::setAllPlots()
{
	//hdEdx_P1->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	//hdEdx_P2->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hGlob_dEdx_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hPrim_dEdx_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hTowerEtaPhi_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hdEdx_P1_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hdEdx_P2_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hVzvpd_Vz_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hdEdx_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hHFM_dEdx_UPC->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hGlob_Phi->SetMinimum(0.);
	hPrim_Phi->SetMinimum(0.);
	hBeamX->SetMarkerColor(kBlue);
	hBeamY->SetMarkerColor(kRed);
	hInnerGain->SetMarkerColor(kBlue);
	hOuterGain->SetMarkerColor(kRed);
	hMeanDcaXy->SetMarkerColor(kRed);
	hHFM_dEdx->SetMarkerStyle(30);
	hHFM_dEdx->SetMarkerSize(0.9);
	hHFM_dEdx->SetMarkerColor(2);
	hnhits->GetXaxis()->SetTitle("nHits");
	hnDedx->GetXaxis()->SetTitle("ndedx");
	hDcaXy->GetXaxis()->SetTitle("DcaXY (cm)");
	hDcaZ->GetXaxis()->SetTitle("DcaZ (cm)");
	hLn_dEdx->GetXaxis()->SetTitle("log(dEdx) (GeV/cm)");
	hGlob_Pt->GetXaxis()->SetTitle("Pt (GeV/c)");
	hGlob_Phi->GetXaxis()->SetTitle("#phi");
	hGlob_Eta->GetXaxis()->SetTitle("#eta");
	hFixedTarget_Glob_Eta->GetXaxis()->SetTitle("#eta");
	hFixedTargetMonitor_Glob_Eta->GetXaxis()->SetTitle("#eta");
	hGlob_dEdx->GetXaxis()->SetTitle("Global Momentum (GeV/c)");
	hGlob_dEdx->GetYaxis()->SetTitle("dEdx (GeV/cm)");
	hPrim_Pt->GetXaxis()->SetTitle("Pt (GeV/c)");
	hPrim_Phi->GetXaxis()->SetTitle("#phi");
	hPrim_Eta->GetXaxis()->SetTitle("#eta");
	hFixedTarget_Prim_Eta->GetXaxis()->SetTitle("#eta");
	hFixedTargetMonitor_Prim_Eta->GetXaxis()->SetTitle("#eta");
	hPrim_dEdx->GetXaxis()->SetTitle("Primary Momentum (GeV/c)");
	hPrim_dEdx->GetYaxis()->SetTitle("dEdx (GeV/cm)");
	hVertexX->GetXaxis()->SetTitle("VertexX (cm)");
	hVertexY->GetXaxis()->SetTitle("VertexY (cm)");
	hVertexZ->GetXaxis()->SetTitle("VertexZ (cm)");
	hFixedTarget_VertexZ->GetXaxis()->SetTitle("VertexZ (cm)");
	hFixedTargetMonitor_VertexZ->GetXaxis()->SetTitle("VertexZ (cm)");
	hVertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hVertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hVertexR->GetXaxis()->SetTitle("VertexR (cm)");
	hLm_VertexX->GetXaxis()->SetTitle("LmVertexX (cm)");
	hLm_VertexY->GetXaxis()->SetTitle("LmVertexY (cm)");
	hLm_VertexZ->GetXaxis()->SetTitle("LmVertexZ (cm)");
	hglobalMult->GetXaxis()->SetTitle("Multiplicity");
	hprimaryMult->GetXaxis()->SetTitle("Multiplicity");
	//    hLmPrimaryMult->GetXaxis()->SetTitle("Primary Multiplicity");
	hMatchPhi_Diff->GetXaxis()->SetTitle("matchPhiDiff");
	hTowerEnergy->GetXaxis()->SetTitle("TowerEnergy (GeV)");
	hTowerDaqId->GetXaxis()->SetTitle("TowerDaqId");
	hTowerSoftId->GetXaxis()->SetTitle("TowerSoftId");
	hzEdge->GetXaxis()->SetTitle("zEdge");
	hTowerEtaPhi->GetXaxis()->SetTitle("#phi");
	hTowerEtaPhi->GetYaxis()->SetTitle("#eta");
	hDiElectronInvMassTpxEmc->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassTpxEmcBG->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassFullRange->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassFullRangeBG->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassTpxEmc_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassTpxEmcBG_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassFullRange_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassFullRangeBG_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassCut->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassCutBG->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassCut_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassCutBG_Twr->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	//hDiMuonInvMassFullRange->GetXaxis()->SetTitle("M_{inv}(uu) GeV/c^{2}");
	//hDiMuonInvMassFullRangeBG->GetXaxis()->SetTitle("M_{inv}(uu) GeV/c^{2}");
	//hDiMuonInvMassTpxCut->GetXaxis()->SetTitle("M_{inv}(uu) GeV/c^{2}");
	//hDiMuonInvMassTpxCutBG->GetXaxis()->SetTitle("M_{inv}(uu) GeV/c^{2}")

	hMTDQmInvMassUS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");  //zaochen MTDQm
	hMTDQmInvMassLS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");

	hMTDQmJpsiMass_ptcut0_US->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");  //zaochen MTDQm Jpsi
	hMTDQmJpsiMass_ptcut0_LS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");
	hMTDQmJpsiMass_ptcut2_US->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");  //zaochen MTDQm Jpsi
	hMTDQmJpsiMass_ptcut2_LS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");
	hMTDQmJpsiMass_ptcut4_US->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");  //zaochen MTDQm Jpsi
	hMTDQmJpsiMass_ptcut4_LS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");

	hMTDQmUpsilonMassUS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");  //zaochen MTDQm Upsilon
	hMTDQmUpsilonMassLS->GetXaxis()->SetTitle("M_{inv}(#mu#mu) GeV/c^{2}");

	hdEdx_P1->GetXaxis()->SetTitle("Daughter1 Momentum");
	hdEdx_P1->GetYaxis()->SetTitle("dEdx (GeV/cm)");
	hdEdx_P1_Twr->GetXaxis()->SetTitle("Daughter1 Momentum");
	hdEdx_P1_Twr->GetYaxis()->SetTitle("dEdx (GeV/cm)");
	hDaughter1P_TowerEnergy->GetXaxis()->SetTitle("TowerEnergy/P");
	hDaughter1P_TowerEnergy_Twr->GetXaxis()->SetTitle("TowerEnergy/P");
	hDaughter1TpxEmcInverseBeta->GetXaxis()->SetTitle("1/#beta");
	hDaughter1TpxEmcInverseBeta_Twr->GetXaxis()->SetTitle("1/#beta");
	hdEdx_P2->GetXaxis()->SetTitle("Daughter2 Momentum");
	hdEdx_P2->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hdEdx_P2_Twr->GetXaxis()->SetTitle("Daughter2 Momentum");
	hdEdx_P2_Twr->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hDaughter2P_TowerEnergy->GetXaxis()->SetTitle("TowerEnergy/P");
	hDaughter2P_TowerEnergy_Twr->GetXaxis()->SetTitle("TowerEnergy/P");
	hDaughter2TpxEmcInverseBeta->GetXaxis()->SetTitle("1/#beta");
	hDaughter2TpxEmcInverseBeta_Twr->GetXaxis()->SetTitle("1/#beta");
	hDiLeptonRapidity->GetXaxis()->SetTitle("Rapidity");
	hDiLeptonRapidity_Twr->GetXaxis()->SetTitle("Rapidity");
	hLocalZ->GetXaxis()->SetTitle("LocalZ");
	hLocalY->GetXaxis()->SetTitle("LocalY");
	//    hTofprimaryMult->GetXaxis()->SetTitle("Primary Multiplicity");
	//    hTofprimaryMult->GetYaxis()->SetTitle("Tof Multiplicity");
	hInverseBeta->GetXaxis()->SetTitle("Momentum (GeV/c)");
	hInverseBeta->GetYaxis()->SetTitle("1/#beta");
	hMatchId_fiberId->GetXaxis()->SetTitle("matchId");
	hMatchId_fiberId->GetYaxis()->SetTitle("fiberId");
	hTrayID_TrgTime->GetXaxis()->SetTitle("TrayId");
	hTrayID_TrgTime->GetYaxis()->SetTitle("TriggerTime");
	hchannelID->GetXaxis()->SetTitle("ChannelId");
	hVzvpd_Vz->GetXaxis()->SetTitle("pvpd VertexZ (cm)");
	hVzvpd_Vz->GetYaxis()->SetTitle("LmVertexZ (cm)");
	hVzDiff->GetXaxis()->SetTitle("Vzvpd - LmVertexZ (cm)");
	hdEdx->SetTitle("dEdx");
	hdEdx->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hdEdx->GetXaxis()->SetTitle("Momentum");
	hHFM_dEdx->GetXaxis()->SetTitle("Primary Momentum");
	hHFM_dEdx->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hDiPionInvMassFullRange->GetXaxis()->SetTitle("M_{inv}(#pi#pi) GeV/c^{2}");
	hDiPionInvMassFullRangeBG->GetXaxis()->SetTitle("M_{inv}(#pi#pi) GeV/c^{2}");
	hDiPionDeltphi->GetXaxis()->SetTitle("delta#phi");
	hBeamX->GetXaxis()->SetTitle("run number");
	hBeamY->GetXaxis()->SetTitle("run number");
	hBeamX->GetXaxis()->SetTitleOffset(2.5);
	hBeamY->GetXaxis()->SetTitleOffset(2.5);
	hBeamX->SetLabelSize(0.04, "X");
	//   hBeamX->SetName("beam position");
	//   hBeamX->SetTitle("beam position");
	hInnerGain->GetXaxis()->SetTitle("run number");
	hInnerGain->SetLabelSize(0.04, "X");
	hOuterGain->GetXaxis()->SetTitle("run number");
	hInnerGain->GetXaxis()->SetTitleOffset(2.5);
	hOuterGain->GetXaxis()->SetTitleOffset(2.5);
	hInnerGain->SetName("gain parameters");
	hOuterGain->SetTitle("gain parameters");
	hMeanDcaXy->GetXaxis()->SetTitle("run number");
	hMeanDcaXy->GetXaxis()->SetTitleOffset(2.5);
	hMeanDcaXy->SetName("dcaXy mean value");
	hMeanDcaXy->SetTitle("dcaXy mean value");
	hMeanDcaXy->SetLabelSize(0.04, "X");
	hBesGoodVertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hBesGoodVertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hBesGoodVrVsVz->GetXaxis()->SetTitle("VertexZ (cm)");
	hBesGoodVrVsVz->GetYaxis()->SetTitle("VertexR (cm)");
	hBesGoodVertexZ->GetXaxis()->SetTitle("VertexZ (cm)");
	//hBesGoodVertexXY->GetZaxis()->SetRangeUser(1.e-10,1.e30);
	hBesGoodVr->GetXaxis()->SetTitle("VertexR (cm)");
	hBesGoodprimaryMult->GetXaxis()->SetTitle("Multiplicity");
	hHLTGood2VertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hHLTGood2VertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hHLTGood2VertexZ->GetXaxis()->SetTitle("VertexZ (cm)");
	hHLTGood2Vr->GetXaxis()->SetTitle("VertexR (cm)");
	hHLTGood2primaryMult->GetXaxis()->SetTitle("Multiplicity");
	hBesMonitorVertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hBesMonitorVertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hBesMonitorVertexXY->GetZaxis()->SetRangeUser(1.e-10, 1.e30);
	hBesMonitorVr->GetXaxis()->SetTitle("VertexR (cm)");
	hFixedTargetVertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hFixedTargetVertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hFixedTargetVr->GetXaxis()->SetTitle("VertexR (cm)");
	//hFixedTargetVertexXY->GetZaxis()->SetRangeUser(1.e-10,1.e30);
	hFixedTargetMonitorVertexXY->GetXaxis()->SetTitle("VertexX (cm)");
	hFixedTargetMonitorVertexXY->GetYaxis()->SetTitle("VertexY (cm)");
	hFixedTargetMonitorVr->GetXaxis()->SetTitle("VertexR (cm)");
	//hFixedTargetMonitorVertexXY->GetZaxis()->SetRangeUser(1.e-10,1.e30);
	hEvtsAccpt->GetXaxis()->SetBinLabel(1, "DiMuon");
	hEvtsAccpt->GetXaxis()->SetBinLabel(2, "MTDQuarkonium");	
	hEvtsAccpt->GetXaxis()->SetBinLabel(3, "HLTGood2");  
	hEvtsAccpt->GetXaxis()->SetBinLabel(4, "DiElectron");
	hEvtsAccpt->GetXaxis()->SetBinLabel(5, "DIEP2Twr");
	hEvtsAccpt->GetXaxis()->SetBinLabel(6, "HeavyFragment"); 

	hGlob_Phi_UPC->SetMinimum(0.);
	hPrim_Phi_UPC->SetMinimum(0.);
	//   hHFM_dEdx_UPC->SetMarkerStyle(30);
	//   hHFM_dEdx_UPC->SetMarkerSize(0.9);
	//   hHFM_dEdx_UPC->SetMarkerColor(2);
	hnhits_UPC->GetXaxis()->SetTitle("nHits");
	hnDedx_UPC->GetXaxis()->SetTitle("ndedx");
	hDcaXy_UPC->GetXaxis()->SetTitle("DcaXY");
	hDcaZ_UPC->GetXaxis()->SetTitle("DcaZ");
	hLn_dEdx_UPC->GetXaxis()->SetTitle("log(dEdx) GeV/cm)");
	hGlob_Pt_UPC->GetXaxis()->SetTitle("Pt");
	hGlob_Phi_UPC->GetXaxis()->SetTitle("#phi");
	hGlob_Eta_UPC->GetXaxis()->SetTitle("#eta");
	hGlob_dEdx_UPC->GetXaxis()->SetTitle("Global Momentum");
	hGlob_dEdx_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hPrim_Pt_UPC->GetXaxis()->SetTitle("Pt");
	hPrim_Phi_UPC->GetXaxis()->SetTitle("#phi");
	hPrim_Eta_UPC->GetXaxis()->SetTitle("#eta");
	hPrim_dEdx_UPC->GetXaxis()->SetTitle("Primary Momentum");
	hPrim_dEdx_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hVertexX_UPC->GetXaxis()->SetTitle("VertexX");
	hVertexY_UPC->GetXaxis()->SetTitle("VertexY");
	hVertexZ_UPC->GetXaxis()->SetTitle("VertexZ");
	hLm_VertexX_UPC->GetXaxis()->SetTitle("LmVertexX");
	hLm_VertexY_UPC->GetXaxis()->SetTitle("LmVertexY");
	hLm_VertexZ_UPC->GetXaxis()->SetTitle("LmVertexZ");
	hglobalMult_UPC->GetXaxis()->SetTitle("Multiplicity");
	hprimaryMult_UPC->GetXaxis()->SetTitle("Multiplicity");
	hMatchPhi_Diff_UPC->GetXaxis()->SetTitle("matchPhiDiff_UPC");
	hzEdge_UPC->GetXaxis()->SetTitle("zEdge");
	hDiElectronInvMassFullRange_UPC->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hDiElectronInvMassFullRangeBG_UPC->GetXaxis()->SetTitle("M_{inv}(ee) GeV/c^{2}");
	hdEdx_P1_UPC->GetXaxis()->SetTitle("Daughter1 Momentum");
	hdEdx_P1_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hDaughter1P_TowerEnergy_UPC->GetXaxis()->SetTitle("TowerEnergy/P");
	hdEdx_P2_UPC->GetXaxis()->SetTitle("Daughter2 Momentum");
	hdEdx_P2_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hDaughter2P_TowerEnergy_UPC->GetXaxis()->SetTitle("TowerEnergy/P");
	hDiLeptonRapidity_UPC->GetXaxis()->SetTitle("Rapidity");
	hVzvpd_Vz_UPC->GetXaxis()->SetTitle("pvpd VertexZ");
	hVzvpd_Vz_UPC->GetYaxis()->SetTitle("LmVertexZ");
	hVzDiff_UPC->GetXaxis()->SetTitle("Vzvpd - LmVertexZ");
	hdEdx_UPC->GetXaxis()->SetTitle("Primary Momentum");
	hdEdx_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
	hHFM_dEdx_UPC->GetXaxis()->SetTitle("Primary Momentum");
	hHFM_dEdx_UPC->GetYaxis()->SetTitle("dEdx in GeV/cm");
}
