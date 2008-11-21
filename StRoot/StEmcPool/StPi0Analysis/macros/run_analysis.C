#if !defined(__CINT__) || defined(__MAKECINT__)

#include <TSystem.h>
#include <TFolder.h>
#include <TBrowser.h>
#include <TCanvas.h>
#include <TDatime.h>
#include <TStopwatch.h>
#include <TMath.h>
#include <TF1.h>
#include <TFile.h>
#include <TError.h>

#include <StEmcPool/StPi0Analysis/TCuts.h>
#include <StEmcPool/StPi0Analysis/TCandidateDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TEventDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TPointDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TClusterDataProcessor.h>
#include <StEmcPool/StPi0Analysis/THitDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TMCGammaDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TSimuDataProcessor.h>
#include <StEmcPool/StPi0Analysis/TDataProcessorPool.h>
#include <StEmcPool/StPi0Analysis/TInvariantMassDistribution.h>
#include <StEmcPool/StPi0Analysis/TWeightCalculator.h>

#include <StEmcPool/StPi0Common/StPi0CommonUtil.h>

#include <StEmcPool/StPi0Common/StPi0DataStructures.h>

#endif

void addPtBin(    Float_t lowPt, Float_t highPt, Float_t stepPt, const Char_t *tag, const Char_t *titleTag
		, TCandidateDataProcessor *candidateDataProcessor, TSimuDataProcessor *simuDataProcessor, Int_t distr
		, TPointDataProcessor *pointDataProcessor = 0
		, TF1 *ptShiftFunc = 0
) {
    for (Int_t ptShiftPass = 0;ptShiftPass <= (ptShiftFunc ? 1 : 0);ptShiftPass++) {
	for (Float_t pT = lowPt;pT < highPt;pT += stepPt) {
		Float_t ptLow = pT;
		Float_t ptHigh = pT + stepPt;
		Int_t numPt = Int_t((ptLow - lowPt) / stepPt);
		Int_t numPtMax = Int_t((highPt - lowPt) / stepPt);
		TString numPtStr;
		if ((numPtMax > 100) && (numPt < 100)) numPtStr += "0";
		if ((numPtMax > 10) && (numPt < 10)) numPtStr += "0";
		numPtStr += numPt;
		if ((ptShiftPass == 1) && ptShiftFunc) {
			ptLow = ptShiftFunc->Eval(ptLow);
			ptHigh = ptShiftFunc->Eval(ptHigh);
		}	
		TString name = distr ? "mult" : "inv"; name += tag; name += "_"; name += numPtStr; name += "_"; name += ptShiftPass;
		TString nameBin = "bin"; nameBin += tag; nameBin += "_"; nameBin += numPtStr; name += "_"; name += ptShiftPass;
		TString nameBinStat = "binStat"; nameBinStat += tag; nameBinStat += "_"; nameBinStat += numPtStr; name += "_"; name += ptShiftPass;
		TString title = titleTag; title += ", "; title += ptLow; title += " < p_{T} < "; title += ptHigh; title += " GeV/c";
		if (distr == 0) title += ";M_{#gamma#gamma}, GeV/c^{2}";
		else if (distr == 1) title += ";TPC Multiplicity";
		else if (distr == 2) title += ";BEMC Multiplicity";
		else if (distr == 3) title += ";BEMC Neutral Multiplicity";
		else if (distr == 4) title += ";Simulated p_{T}, GeV/c";
		else if (distr == 5) title += ";Point p_{T}, GeV/c";
		else if (distr == 6) title += ";Point-to-track distance";
		else if (distr == 7) title += ";Point-to-track distance 2";
		TBinParameters binPar(nameBin.Data(), title.Data());
		binPar.variable = (TBinVariable)0;
		binPar.min = ptLow;
		binPar.max = ptHigh;
		Int_t nbins = 1;
		if (distr == 0) nbins = 1800;
		else if (distr == 1) nbins = 100;
		else if (distr == 2) nbins = 20;
		else if (distr == 3) nbins = 20;
		else if (distr == 4) nbins = 30;
		else if (distr == 5) nbins = 30;
		else if (distr == 6) nbins = 200;
		else if (distr == 7) nbins = 200;
		Float_t range = 1;
		if (distr == 0) range = 6.0;
		else if (distr == 1) range = 100.0;
		else if (distr == 2) range = 20;
		else if (distr == 3) range = 20;
		else if (distr == 4) range = 30;
		else if (distr == 5) range = 30;
		else if (distr == 6) range = 1.0;
		else if (distr == 7) range = 1.0;
		
		TH1F invHist(name.Data(), title.Data(), nbins, 0, range);
		//TH2F invHistTower(name.Data(), title.Data(), nbins, 0, range, 2400, 0-0.5, 2400-0.5);
		//TH2F invHistEtaCoord(name.Data(), title.Data(), nbins, 0, range, 80, -1.0, +1.0);
		invHist.Sumw2();
		//invHistTower.Sumw2();
		//invHistEtaCoord.Sumw2();
		TInvariantMassDistribution inv(name.Data(), title.Data());
		inv.setBinParameters(binPar);
		inv.setDistribution(invHist);
		//inv.setDistributionTower(invHistTower);
		//inv.setDistributionEtaCoord(invHistEtaCoord);
		if (candidateDataProcessor) {
			if (distr == 0) {
				candidateDataProcessor->invariantMassDistributions.push_back(inv);
			} else if (distr == 1) {
				candidateDataProcessor->multiplicityPrimaryDistributions.push_back(inv);
			} else if (distr == 2) {
				candidateDataProcessor->multiplicityPointsDistributions.push_back(inv);
			//} else if (distr == 3) {
			//	candidateDataProcessor->multiplicityNeutralPointsDistributions.push_back(inv);
			} else if (distr == 4) {
				candidateDataProcessor->simulatedPtDistributions.push_back(inv);
			} else if (distr == 5) {
				candidateDataProcessor->pointPtDistributions.push_back(inv);
			} else if (distr == 6) {
				candidateDataProcessor->pointTrackDistDistributions.push_back(inv);
			} else if (distr == 7) {
				candidateDataProcessor->pointTrackDist2Distributions.push_back(inv);
			}
		}
		if (pointDataProcessor) {
			if (distr == 0) {
			} else if (distr == 1) {
				pointDataProcessor->multiplicityPrimaryDistributions.push_back(inv);
			} else if (distr == 2) {
				pointDataProcessor->multiplicityPointsDistributions.push_back(inv);
			//} else if (distr == 3) {
			//	pointDataProcessor->multiplicityNeutralPointsDistributions.push_back(inv);
			} else if (distr == 6) {
				pointDataProcessor->pointTrackDistDistributions.push_back(inv);
			}
		}
		TBinStatistics binStat(nameBinStat.Data(), title.Data());
		binStat.setParameters(binPar);
		if (simuDataProcessor && (distr == 0)) simuDataProcessor->binStatistics.push_back(binStat);
	}
    }
}

void run_analysis(const Char_t *filelist = "filelist.list", const Char_t *weightFileStr = "/dev/null", const Char_t *outFile = "/dev/null", const Char_t *configurationStr = "noweight nocentral useMB useHT1 useHT2 dAu2003") {
	Info(__FILE__, "================== STARTED ===================");
	TStopwatch timer;
	timer.Start();
	TDatime startTime;
	Info(__FILE__, "Started: %s", startTime.AsSQLString());

	Bool_t useHighTowerEt = true;
	Bool_t useHighTowerSimulated = true;
	Bool_t useHighTowerOnlineTrigger = true;
	Bool_t jetMixRequiresJetInEvent = false;
	Bool_t jetMixRequiresPointsInJet = false;
	Bool_t jetMixRejectEnergyMatch = true;
	Bool_t mixRejectEnergyMatch = true;
	Bool_t requireTPCVertex = true;
	Bool_t requireBBCPresent = true;
	Bool_t rejectBeamBgEvents = true;
	Bool_t usePythiaPartonicPtBinWeight = false;
	Bool_t usePythiaPartonicPtBinXWeight = true;
	Bool_t usePythiaPartonicPtWeight = false;
	Bool_t usePythiaBadEventCut = false;
	Bool_t usePythiaPartonicPtBadEventFiles = false;

	TString weightFile = findFile(weightFileStr);
	TString configuration = configurationStr;
	Info(__FILE__, "Configuration: %s", configuration.Data());
	Bool_t useMB = configuration.Contains("useMB");
	Bool_t useHT1 = configuration.Contains("useHT1");
	Bool_t useHT2 = configuration.Contains("useHT2");
	Info(__FILE__, "useMB: %i, useHT1: %i, useHT2: %i", useMB, useHT1, useHT2);
	Bool_t simulation = configuration.Contains("simulation");
	Bool_t nocentral = configuration.Contains("nocentral");
	Bool_t simulation_eta = configuration.Contains("eta_sim");
	Bool_t simulation_eta_bg = configuration.Contains("eta_bg");
	Bool_t simulation_gamma = configuration.Contains("gamma");
	Bool_t simulation_nbar = configuration.Contains("nbar");
	Bool_t simulation_pythia = configuration.Contains("pythia");
	Bool_t processor_compare_ignore_weight = configuration.Contains("processor_compare_ignore_weight");
	Bool_t processor_compare_ignore_cuts = configuration.Contains("processor_compare_ignore_weight");
	TDataProcessor::compareIgnoreWeight = processor_compare_ignore_weight;
	TDataProcessor::compareIgnoreCuts = processor_compare_ignore_cuts;
	if (TDataProcessor::compareIgnoreWeight) {Info(__FILE__, "Ignoring weight when comparing data processors!");}
	if (TDataProcessor::compareIgnoreCuts) {Info(__FILE__, "Ignoring cuts when comparing data processors!");}
	if (simulation) useHighTowerOnlineTrigger = false;
	Bool_t useNloPqcdPP = false;

	if (configuration.Contains("dAu2003")) {
	    useHighTowerEt = true;
	    useHighTowerSimulated = true;
	    requireTPCVertex = true;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = true;
    	    useNloPqcdPP = false;
	} else if (configuration.Contains("pp2005")) {
	    useHighTowerEt = true;
	    useHighTowerSimulated = true;
	    requireTPCVertex = false;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = false;
    	    useNloPqcdPP = true;
	} else if (configuration.Contains("pp2006")) {
	    useHighTowerEt = true;
	    useHighTowerSimulated = true;
	    requireTPCVertex = false;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = false;
    	    useNloPqcdPP = true;
	} else if (configuration.Contains("dAu2008")) {
	    useHighTowerEt = true;
	    useHighTowerSimulated = true;
	    requireTPCVertex = false;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = false;
    	    useNloPqcdPP = false;
	} else if (configuration.Contains("pp2008")) {
	    useHighTowerEt = false;
	    useHighTowerSimulated = false;
	    requireTPCVertex = false;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = false;
    	    useNloPqcdPP = false;
	}
	Bool_t useZSimuSmearing = !requireTPCVertex;
	Bool_t useZSimuSmearingZero = !requireBBCPresent && false;
	if (simulation && !simulation_pythia) {
	    requireTPCVertex = false;
	    requireBBCPresent = false;
	    rejectBeamBgEvents = false;
	}

	TWeightCalculator weightZSimuSmearingPtMB("weightZSimuSmearingPtMB", "Z smearing probability in the simulation vs. pion p_{T} - MinBias");
	TWeightCalculator weightZSimuSmearingPtHT1("weightZSimuSmearingPtHT1", "Z smearing probability in the simulation vs. pion p_{T} - HighTower-1");
	TWeightCalculator weightZSimuSmearingPtHT2("weightZSimuSmearingPtHT2", "Z smearing probability in the simulation vs. pion p_{T} - HighTower-2");
	TWeightCalculator weightZSimuSmearingZeroPtMB("weightZSimuSmearingZeroPtMB", "Z zero probability in the simulation vs. pion p_{T} - MinBias");
	TWeightCalculator weightZSimuSmearingZeroPtHT1("weightZSimuSmearingZeroPtHT1", "Z zero probability in the simulation vs. pion p_{T} - HighTower-1");
	TWeightCalculator weightZSimuSmearingZeroPtHT2("weightZSimuSmearingZeroPtHT2", "Z zero probability in the simulation vs. pion p_{T} - HighTower-2");

	TCutParameters cutParameters("cutParameters", "Cut parameters");
	cutParameters.simulation = simulation;
	cutParameters.jetRotate = 0; // 0 - none, 1 - align jets, 2 - align jets back-to-back
	cutParameters.zCutLow = -60;
	cutParameters.zCutHigh = +60;
	cutParameters.neutralPointsCutLow = -1;
	cutParameters.neutralPointsCutHigh = 1000;
	cutParameters.useZTPC = true;
	cutParameters.useZBBC = true;
	cutParameters.zSimuSmearing = simulation && !simulation_pythia && useZSimuSmearing;
	cutParameters.zSimuSmearingZero = simulation && !simulation_pythia && useZSimuSmearingZero;
	cutParameters.calibrationSlope = 0.0;
	TString dataSetStr;
	if (configuration.Contains("dAu2003")) {
	    dataSetStr = "dAu2003";
	    cutParameters.HT1AdcThreshold = 8 * 32;
	    cutParameters.HT2AdcThreshold = 13 * 32;
	    cutParameters.HT1EtThreshold = 2.5 + 0.5;
	    cutParameters.HT2EtThreshold = 4.5 + 0.5;
	    cutParameters.triggersMB = 1 + 2;
	    cutParameters.triggersHT1 = 4;
	    cutParameters.triggersHT2 = 8;
	    cutParameters.bunchCrossingIdOffsetsFilename = "bunchCrossingId7bitOffsets_dAu2003.txt";
	    cutParameters.zBBCcoeff0 = 6.7;
	    cutParameters.zBBCcoeff1 = 2.2;
	    cutParameters.zSimuSmearingPt = 0;
	    cutParameters.zSimuMeanCoeff0 = 0.0;
	    cutParameters.zSimuMeanCoeff1 = 1.0;
	    cutParameters.zSimuSpreadCoeff0 = 40.0;
	    cutParameters.zSimuSpreadCoeff1 = 0.0;
	    cutParameters.zSimuSmearingZeroPt = 0;
	    cutParameters.calibrationSlope = 0.0330;
	} else if (configuration.Contains("pp2005")) {
	    dataSetStr = "pp2005";
	    cutParameters.HT1AdcThreshold = 13 * 32;
	    cutParameters.HT2AdcThreshold = 17 * 32;
	    cutParameters.HT1EtThreshold = 2.6 + 0.5;
	    cutParameters.HT2EtThreshold = 3.5 + 0.5;
	    cutParameters.triggersMB = 1+2;//1;
	    cutParameters.triggersHT1 = 4+8;//2;
	    cutParameters.triggersHT2 = 16+32;//4;
            if (simulation_nbar || useMB) {
                // new trees
                cutParameters.triggersMB = 1;
                cutParameters.triggersHT1 = 2;
                cutParameters.triggersHT2 = 4;
	    }
	    cutParameters.bunchCrossingIdOffsetsFilename = "bunchCrossingId7bitOffsets_pp2005.txt";
	    cutParameters.zBBCcoeff0 = 11.0045;
	    cutParameters.zBBCcoeff1 =  2.8236;
	    cutParameters.zSimuSmearingPt = 0;
	    cutParameters.zSimuMeanCoeff0 = 0.0;
	    cutParameters.zSimuMeanCoeff1 = 1.0;
	    cutParameters.zSimuSpreadCoeff0 = 40.0;
	    cutParameters.zSimuSpreadCoeff1 = 0.0;
	    cutParameters.zSimuSmearingZeroPt = 0;
	    cutParameters.calibrationSlope = 0.0419;
	} else if (configuration.Contains("pp2006")) {
	    dataSetStr = "pp2006";
	    cutParameters.HT1AdcThreshold = 17 * 16;
	    cutParameters.HT2AdcThreshold = 24 * 16;
	    cutParameters.HT1EtThreshold = 3.8 + 0.5;
	    cutParameters.HT2EtThreshold = 5.4 + 0.5;
	    cutParameters.triggersMB = 1;
	    cutParameters.triggersHT1 = 2;
	    cutParameters.triggersHT2 = 4 + 8 + 16 + 32 + 64;
	    cutParameters.bunchCrossingIdOffsetsFilename = "bunchCrossingId7bitOffsets_pp2006.txt";
	    cutParameters.zBBCcoeff0 = 6.7;
	    cutParameters.zBBCcoeff1 = 2.2;
	    cutParameters.zSimuSmearingPt = 0;
	    cutParameters.zSimuMeanCoeff0 = 0.0;
	    cutParameters.zSimuMeanCoeff1 = 1.0;
	    cutParameters.zSimuSpreadCoeff0 = 25.0;
	    cutParameters.zSimuSpreadCoeff1 = 0.0;
	    cutParameters.zSimuSmearingZeroPt = 0;
	    cutParameters.calibrationSlope = 0.0;
	} else if (configuration.Contains("dAu2008")) {
	    dataSetStr = "dAu2008";
	    cutParameters.HT1AdcThreshold = 17 * 16;
	    cutParameters.HT2AdcThreshold = 24 * 16;
	    cutParameters.HT1EtThreshold = 3.8 + 0.5;
	    cutParameters.HT2EtThreshold = 5.4 + 0.5;
	    cutParameters.triggersMB = 1; // HT0
	    cutParameters.triggersHT1 = 2; // HT1
	    cutParameters.triggersHT2 = 4; // HT2
	    cutParameters.bunchCrossingIdOffsetsFilename = "bunchCrossingId7bitOffsets_dAu2008.txt";
	    cutParameters.zBBCcoeff0 = 6.7;
	    cutParameters.zBBCcoeff1 = 2.2;
	    cutParameters.zSimuSmearingPt = 0;
	    cutParameters.zSimuMeanCoeff0 = 0.0;
	    cutParameters.zSimuMeanCoeff1 = 1.0;
	    cutParameters.zSimuSpreadCoeff0 = 25.0;
	    cutParameters.zSimuSpreadCoeff1 = 0.0;
	    cutParameters.zSimuSmearingZeroPt = 0;
	    cutParameters.calibrationSlope = 0.0;
	} else if (configuration.Contains("pp2008")) {
	    dataSetStr = "pp2008";
	    cutParameters.HT1AdcThreshold = 17 * 16;
	    cutParameters.HT2AdcThreshold = 24 * 16;
	    cutParameters.HT1EtThreshold = 3.8 + 0.5;
	    cutParameters.HT2EtThreshold = 5.4 + 0.5;
	    cutParameters.triggersMB = 8+16; // FMS-slow
	    cutParameters.triggersHT1 = 2; // HT1
	    cutParameters.triggersHT2 = 4; // HT2
	    cutParameters.bunchCrossingIdOffsetsFilename = "bunchCrossingId7bitOffsets_pp2008.txt";
	    cutParameters.zBBCcoeff0 = 6.7;
	    cutParameters.zBBCcoeff1 = 2.2;
	    cutParameters.zSimuSmearingPt = 0;
	    cutParameters.zSimuMeanCoeff0 = 0.0;
	    cutParameters.zSimuMeanCoeff1 = 1.0;
	    cutParameters.zSimuSpreadCoeff0 = 25.0;
	    cutParameters.zSimuSpreadCoeff1 = 0.0;
	    cutParameters.zSimuSmearingZeroPt = 0;
	    cutParameters.calibrationSlope = 0.0;
	}
	if (simulation) cutParameters.calibrationSlope = 0.0;
	cutParameters.trackDistCutLow = 0.04;
	cutParameters.trackDistCutHigh = -1;
	cutParameters.gammaDistCutLow = 0.00;
	cutParameters.gammaDistCutHigh = 0.05;
	cutParameters.asymCutLow = 0.0;
	cutParameters.asymCutHigh = 0.7;
	cutParameters.ptLow = 14;
	cutParameters.ptHigh = 1000;
	cutParameters.ptBinStart = 0.0;
	cutParameters.ptBinStep = 0.5;
	cutParameters.gammaConversionRadiusLow = 180;
	cutParameters.gammaConversionRadiusHigh = 10000;
	cutParameters.massRegionLeft = 0.3;//0.07;
	cutParameters.massRegionLeftPt = 0;//0.001;
	cutParameters.massRegionRight = 5;//0.186;
	cutParameters.massRegionRightPt = 0;//0.01;
	cutParameters.pointEnergyLow = 0.0;
	cutParameters.pointEnergyHigh = 1000;
	cutParameters.smdEnergyLow = 2.5;
	cutParameters.smdEnergyHigh = 1000;
	cutParameters.smdSizeLow = 2;
	cutParameters.smdSizeHigh = 1000;
	cutParameters.etaLow = 0.0;
	cutParameters.etaHigh = 1.0;
	cutParameters.etaCoordLow = 0.1;
	cutParameters.etaCoordHigh = 0.9;
	cutParameters.phiCoordLow = -TMath::TwoPi();
	cutParameters.phiCoordHigh = +TMath::TwoPi();
	cutParameters.openAngleMinFraction = 0.0;
        cutParameters.openAngleMinOffset = useMB ? 0.05 : (TMath::Sqrt(((1.0/150.0)*(1.0/150.0)) + ((TMath::TwoPi()/(60.0*16.0))*(TMath::TwoPi()/(60.0*16.0)))) * 1.5);
	cutParameters.clustersPerTrackLow = 0.0;
	cutParameters.clustersPerTrackHigh = 0.5;
	cutParameters.EMCEtNeutralToTotalLow = -0.5;
	cutParameters.EMCEtNeutralToTotalHigh = -0.05;
	cutParameters.EMCENeutralToTotalLow = -0.5;
	cutParameters.EMCENeutralToTotalHigh = 1.5;
	cutParameters.TPCPtToEMCEtLow = 0.2;
	cutParameters.TPCPtToEMCEtHigh = 10000.0;
	cutParameters.jetEtLow = 5.0;
	cutParameters.jetDistCutLow = 0.0;
	cutParameters.jetDistCutHigh = 0.7;
	/*
	// my beam bg cut
    	cutParameters.TPCPt0VsEMCEt0 = -2.0;
	cutParameters.TPCPt1VsEMCEt0 = +1.0;
	cutParameters.TPCPt0VsEMCEt1 = 0.0;
	cutParameters.TPCPt1VsEMCEt1 = 0.0;
	cutParameters.TPCPt2VsEMCEt0 = 0.0;
	cutParameters.TPCPt0VsEMCEt2 = -0.0045;
	*/
	// Martijn's E_t / (E_t + P_t) < 0.8 beam bg cut
	cutParameters.TPCPt0VsEMCEt0 = 0.0;
	cutParameters.TPCPt1VsEMCEt0 = +0.8;
	cutParameters.TPCPt0VsEMCEt1 = +0.8 - 1.0;
	cutParameters.TPCPt1VsEMCEt1 = 0.0;
	cutParameters.TPCPt2VsEMCEt0 = 0.0;
	cutParameters.TPCPt0VsEMCEt2 = 0.0;
	cutParameters.tpcRefMultLow = -1;
	cutParameters.tpcRefMultHigh = 1000;
	cutParameters.highestAdcLow = -1;
	cutParameters.highestAdcHigh = 500;
	cutParameters.highestEtLow = 5.5;
	cutParameters.highestEtHigh = 6.5;
	cutParameters.badEventsListFilename = "";
	cutParameters.pythiaPi0PtCutoff = -1;
	TString pythiaPtBinStr;
	if (configuration.Contains("weight_pythia_00_02gev")) {
	    cutParameters.pythiaPi0PtCutoff = 3.0;
	    pythiaPtBinStr = "00_02gev";
	} else if (configuration.Contains("weight_pythia_02_03gev")) {
	    cutParameters.pythiaPi0PtCutoff = 3.0;
	    pythiaPtBinStr = "02_03gev";
	} else if (configuration.Contains("weight_pythia_03_04gev")) {
	    cutParameters.pythiaPi0PtCutoff = 4.0;
	    pythiaPtBinStr = "03_04gev";
	} else if (configuration.Contains("weight_pythia_04_05gev")) {
	    cutParameters.pythiaPi0PtCutoff = 5.0;
	    pythiaPtBinStr = "04_05gev";
	} else if (configuration.Contains("weight_pythia_05_07gev")) {
	    cutParameters.pythiaPi0PtCutoff = 6.0;
	    pythiaPtBinStr = "05_07gev";
	} else if (configuration.Contains("weight_pythia_07_09gev")) {
	    cutParameters.pythiaPi0PtCutoff = 7.0;
	    pythiaPtBinStr = "07_09gev";
	} else if (configuration.Contains("weight_pythia_09_11gev")) {
	    cutParameters.pythiaPi0PtCutoff = 9.0;
	    pythiaPtBinStr = "09_11gev";
	} else if (configuration.Contains("weight_pythia_11_15gev")) {
	    cutParameters.pythiaPi0PtCutoff = 10.0;
	    pythiaPtBinStr = "11_15gev";
	} else if (configuration.Contains("weight_pythia_15_25gev")) {
	    cutParameters.pythiaPi0PtCutoff = 15.0;
	    pythiaPtBinStr = "15_25gev";
	} else if (configuration.Contains("weight_pythia_25_35gev")) {
	    cutParameters.pythiaPi0PtCutoff = 25.0;
	    pythiaPtBinStr = "25_35gev";
	} else if (configuration.Contains("weight_pythia_35_infgev")) {
	    cutParameters.pythiaPi0PtCutoff = -1;
	    pythiaPtBinStr = "35_infgev";
	} else if (configuration.Contains("weight_pythia_minbias")) {
	    cutParameters.pythiaPi0PtCutoff = -1;
	    pythiaPtBinStr = "minbias";
	}
	if (configuration.Contains("0-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 1000;
	} else if (configuration.Contains("40-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("20-40")) {
		cutParameters.ftpcRefMultLow = 10;
		cutParameters.ftpcRefMultHigh = 17;
	} else if (configuration.Contains("0-20")) {
		cutParameters.ftpcRefMultLow = 17;
		cutParameters.ftpcRefMultHigh = 1000;
	} else if (configuration.Contains("40-80")) {
		cutParameters.ftpcRefMultLow = 3;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("40-75")) {
		cutParameters.ftpcRefMultLow = 4;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("40-70")) {
		cutParameters.ftpcRefMultLow = 5;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("40-65")) {
		cutParameters.ftpcRefMultLow = 6;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("40-60")) {
		cutParameters.ftpcRefMultLow = 7;
		cutParameters.ftpcRefMultHigh = 10;
	} else if (configuration.Contains("80-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 3;
	} else if (configuration.Contains("75-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 4;
	} else if (configuration.Contains("70-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 5;
	} else if (configuration.Contains("65-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 6;
	} else if (configuration.Contains("60-100")) {
		cutParameters.ftpcRefMultLow = 0;
		cutParameters.ftpcRefMultHigh = 7;
	} else {
		cutParameters.ftpcRefMultLow = -1;
		cutParameters.ftpcRefMultHigh = 1000;
		nocentral = true;
	}
	cutParameters.isBadRun = 11000011 + (nocentral ? 0 : 100000);

	TString badEventsFilenameMB = "badEvents_pythiaPi0Pt_";
	badEventsFilenameMB += dataSetStr;
	badEventsFilenameMB += "_MB";
	if (usePythiaPartonicPtBadEventFiles) {
	    badEventsFilenameMB += "_";
	    badEventsFilenameMB += pythiaPtBinStr;
	}
	badEventsFilenameMB += ".txt";

	TString badEventsFilenameHT1 = "badEvents_pythiaPi0Pt_";
	badEventsFilenameHT1 += dataSetStr;
	badEventsFilenameHT1 += "_HT1";
	if (usePythiaPartonicPtBadEventFiles) {
	    badEventsFilenameHT1 += "_";
	    badEventsFilenameHT1 += pythiaPtBinStr;
	}
	badEventsFilenameHT1 += ".txt";

	TString badEventsFilenameHT2 = "badEvents_pythiaPi0Pt_";
	badEventsFilenameHT2 += dataSetStr;
	badEventsFilenameHT2 += "_HT2";
	if (usePythiaPartonicPtBadEventFiles) {
	    badEventsFilenameHT2 += "_";
	    badEventsFilenameHT2 += pythiaPtBinStr;
	}
	badEventsFilenameHT2 += ".txt";

	TCutParameters cutParametersEvent = cutParameters;
        cutParametersEvent.SetNameTitle("parametersEvent", "Parameters - event");
	//cutParametersEvent.etaCoordLow = 0.3;
	//cutParametersEvent.etaCoordHigh = 0.7;

	TCutParameters cutParametersEventPPMinBias = cutParametersEvent;
        cutParametersEventPPMinBias.SetNameTitle("parametersEventPPMinBias", "Parameters - event (pp2005 MinBias)");
	cutParametersEventPPMinBias.isBadRun = ((Int_t(cutParametersEvent.isBadRun / 10000000)) * 10000000) + 2000000 + (cutParametersEvent.isBadRun % 1000000);
	cutParametersEventPPMinBias.zSimuSmearingPt = &weightZSimuSmearingPtMB;
	cutParametersEventPPMinBias.zSimuSmearingZeroPt = &weightZSimuSmearingZeroPtMB;
	cutParametersEventPPMinBias.badEventsListFilename = badEventsFilenameMB;

	TCutParameters cutParametersEventHT1 = cutParametersEvent;
        cutParametersEventHT1.SetNameTitle("parametersEventHT1", "Parameters - event (HighTower-1)");
	cutParametersEventHT1.zSimuSmearingPt = &weightZSimuSmearingPtHT1;
	cutParametersEventHT1.zSimuSmearingZeroPt = &weightZSimuSmearingZeroPtHT1;
	cutParametersEventHT1.badEventsListFilename = badEventsFilenameHT1;

	TCutParameters cutParametersEventHT2 = cutParametersEvent;
        cutParametersEventHT2.SetNameTitle("parametersEventHT1", "Parameters - event (HighTower-2)");
	cutParametersEventHT2.zSimuSmearingPt = &weightZSimuSmearingPtHT2;
	cutParametersEventHT2.zSimuSmearingZeroPt = &weightZSimuSmearingZeroPtHT2;
	cutParametersEventHT2.badEventsListFilename = badEventsFilenameHT2;

	TCutParameters cutParametersPoint = cutParameters;
        cutParametersPoint.SetNameTitle("parametersPoint", "Parameters - point");

	TCutParameters cutParametersPointCharged = cutParametersPoint;
        cutParametersPointCharged.SetNameTitle("parametersPointCharged", "Parameters - charged point");
	cutParametersPointCharged.trackDistCutLow = 0.03;

	TCutParameters cutParametersCandidate = cutParameters;
        cutParametersCandidate.SetNameTitle("parametersCandidate", "Parameters - candidate");

	TCutParameters cutParametersCandidateJetmix = cutParameters;
        cutParametersCandidateJetmix.SetNameTitle("parametersCandidateJetmix", "Parameters - jet-mixed candidate");
	cutParametersCandidateJetmix.jetRotate = 1;

	TCutParameters cutParametersCandidateJetmixBack = cutParameters;
        cutParametersCandidateJetmixBack.SetNameTitle("parametersCandidateJetmixBack", "Parameters - jet-mixed back-to-back candidate");
	cutParametersCandidateJetmixBack.jetRotate = 2;

	TCutParameters cutParametersGamma = cutParameters;
        cutParametersGamma.SetNameTitle("parametersGamma", "Parameters - gamma");
        
	TCutParameters cutParametersPion = cutParameters;
        cutParametersPion.SetNameTitle("parametersPion", "Parameters - pion");
                                        

	//Bool_t wantPhotonRcp = false;
	Bool_t wantMultiplicityRcp = false;
	Bool_t includeCandidateEvents = true;
	Bool_t includeCandidatePoints = true;
	Bool_t addPtShiftBins = false;//simulation && (configuration.Contains("noweight_sim") || configuration.Contains("noweight_1gamma")) && false;
	Bool_t includeGammaProcessor = simulation/* && (configuration.Contains("noweight_1gamma") || configuration.Contains("weight_1gamma"))*/;
	Bool_t includePi0Processor = simulation/* && (!includeGammaProcessor)*/;

	TWeightCalculator weightCalculator("weight", "Weight calculator");
	getNLOpQCD(0, 0);
	getNLOpQCDPP(0, 0);
	if (configuration.Contains("noweight") || configuration.Contains("noweight_sim")) {
		// No weight
		weightCalculator.SetName("noweight");
		weightCalculator.mult =      1.0;
		weightCalculator.multDrift =   0;
		weightCalculator.pt0 =         0;
		weightCalculator.power =       0;
		weightCalculator.rangeLow =   -1;
		weightCalculator.rangeHigh = 100;
	} else if (configuration.Contains("pQCDweight")) {
		// pQCD weight
		weightCalculator.SetName("pQCDweight");
		weightCalculator = pQCDweight;
		weightCalculator.rangeLow =    0;
		weightCalculator.rangeHigh = 100;
	} else if (configuration.Contains("pQCDPPweight")) {
		// pQCD p+p weight
		weightCalculator.SetName("pQCDPPweight");
		weightCalculator = pQCDPPweight;
		weightCalculator.rangeLow =    0;
		weightCalculator.rangeHigh = 100;
	} else if (configuration.Contains("noweight_1gamma")) {
		// 1 gamma, no weight
		weightCalculator.SetName("noweight_1gamma");
		weightCalculator.mult =      1.0;
		weightCalculator.multDrift = 0.0;
		weightCalculator.pt0 =  0.327481;
		weightCalculator.power = 1.10996;
		weightCalculator.rangeLow =    0;
		weightCalculator.rangeHigh = 100;
	} else if (configuration.Contains("weight_pythia")) {
	    // PYTHIA simulation
	    weightCalculator.SetName("weight_pythia");
	    weightCalculator.mult =      1.0;
	    weightCalculator.multDrift =   0;
	    weightCalculator.pt0 =         0;
	    weightCalculator.power =       0;
	    weightCalculator.rangeLow =   -1;
	    weightCalculator.rangeHigh = 100;
	    if (usePythiaPartonicPtWeight) {
		    TFile f(weightFile);
	    	    if (f.IsOpen()) {
			TWeightCalculator *weight = (TWeightCalculator*)f.Get(weightCalculator.GetName());
			if (weight) {
			    weightCalculator = *weight;
			} else {Error(__FILE__, "Weight %s not found in %s !", weightCalculator.GetName(), weightFile.Data());}
		    } else {Error(__FILE__, "Cannot open weights file %s", weightFile.Data());}
	    }
	    if (usePythiaPartonicPtBinWeight || usePythiaPartonicPtBinXWeight) {
		// w = cross-section / N_events, for each partonic pT bin
		// Cross sections are taken from:
		// http://www.star.bnl.gov/HyperNews-star/protected/get/jetfinding/187.html
		// http://www.star.bnl.gov/HyperNews-star/protected/get/jetfinding/257.html
		if (configuration.Contains("weight_pythia_00_02gev")) { // Soft processes, cross section calculated as x_minbias - sum(x_other_bins)
		    weightCalculator.SetName("weight_pythia_00_02gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 18.24400 : 1.0) / 600000.0;
		} else if (configuration.Contains("weight_pythia_02_03gev")) {
		    weightCalculator.SetName("weight_pythia_02_03gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 8.110e+0 : 1.0) / 508000.0;
		} else if (configuration.Contains("weight_pythia_03_04gev")) {
		    weightCalculator.SetName("weight_pythia_03_04gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 1.287e+0 : 1.0) / 400629.0;
		} else if (configuration.Contains("weight_pythia_04_05gev")) {
		    weightCalculator.SetName("weight_pythia_04_05gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 3.117e-1 : 1.0) / 600980.0;
		} else if (configuration.Contains("weight_pythia_05_07gev")) {
		    weightCalculator.SetName("weight_pythia_05_07gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 1.360e-1 : 1.0) / 431000.0;
		} else if (configuration.Contains("weight_pythia_07_09gev")) {
		    weightCalculator.SetName("weight_pythia_07_09gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 2.305e-2 : 1.0) / 414245.0;
		} else if (configuration.Contains("weight_pythia_09_11gev")) {
		    weightCalculator.SetName("weight_pythia_09_11gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 5.494e-3 : 1.0) / 416000.0;
		} else if (configuration.Contains("weight_pythia_11_15gev")) {
		    weightCalculator.SetName("weight_pythia_11_15gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 2.228e-3 : 1.0) / 422780.0;
		} else if (configuration.Contains("weight_pythia_15_25gev")) {
		    weightCalculator.SetName("weight_pythia_15_25gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 3.895e-4 : 1.0) / 408000.0;
		} else if (configuration.Contains("weight_pythia_25_35gev")) {
		    weightCalculator.SetName("weight_pythia_25_35gev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 1.016e-5 : 1.0) / 408000.0;
		} else if (configuration.Contains("weight_pythia_35_infgev")) {
		    weightCalculator.SetName("weight_pythia_35_infgev");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 5.299e-7 : 1.0) / 104000.0;
		} else if (configuration.Contains("weight_pythia_minbias")) {
		    weightCalculator.SetName("weight_pythia_minbias");
		    weightCalculator.mult *= (usePythiaPartonicPtBinXWeight ? 28.12e+0 : 1.0) /  97295.0;
		}
	    }
	} else {
	    TString simulationWeight;
#define TEST_WEIGHT(w) if (configuration.Contains(w)) simulationWeight = w;
	    TEST_WEIGHT("weight_pi0");
	    TEST_WEIGHT("weight_eta");
	    TEST_WEIGHT("weight_1gamma_pi0");
	    TEST_WEIGHT("weight_1gamma_eta");
	    TEST_WEIGHT("weight_1gamma_data_MB");
	    TEST_WEIGHT("weight_1gamma_data_HT1");
	    TEST_WEIGHT("weight_1gamma_data_HT2");
	    if (simulationWeight == "") {
    		Int_t simWInd = configuration.Index("weight=");
    		if (simWInd != -1) {
		    Int_t size = configuration.Index(" ", simWInd);
		    if (size == -1) size = configuration.Length();
		    size -= simWInd;
		    simulationWeight = configuration(simWInd + 7, size);
		}
	    }
	    weightCalculator.SetName(simulationWeight);
	    TFile f(weightFile);
	    if (f.IsOpen()) {
	    } else {Error(__FILE__, "Cannot open weights file %s", weightFile.Data());}
		if (simulationWeight != "") {
		    if (((simulationWeight == "weight_pi0") || (simulationWeight == "weight_eta")) && (!f.IsOpen() || !f.Get(simulationWeight))) {
			if (useNloPqcdPP) {
			    {Error(__FILE__, "Weight %s not found in %s, using pQCD p+p weight", simulationWeight.Data(), weightFile.Data());}
			    // pQCD p+p weight
			    weightCalculator.SetName("pQCDPPweight");
			    weightCalculator = pQCDPPweight;
			} else {
			    {Error(__FILE__, "Weight %s not found in %s, using pQCD weight", simulationWeight.Data(), weightFile.Data());}
			    // pQCD weight
			    weightCalculator.SetName("pQCDweight");
			    weightCalculator = pQCDweight;
			}
			weightCalculator.rangeLow =    0;
			weightCalculator.rangeHigh = 100;
		    } else if (((simulationWeight == "weight_1gamma_pi0") || (simulationWeight == "weight_1gamma_eta")) && (!f.Get(simulationWeight))) {
			TString subst = "weight_1gamma_data_MB";
			{Error(__FILE__, "Weight %s not found in %s, using %s weight instead", simulationWeight.Data(), weightFile.Data(), subst.Data());}
			TWeightCalculator *weight = f.IsOpen() ? (TWeightCalculator*)f.Get(subst) : 0;
			if (weight) {
			    weightCalculator = *weight;
			} else {Error(__FILE__, "Weight %s not found in %s !", subst.Data(), weightFile.Data());}
			weightCalculator.rangeLow =    0;
			weightCalculator.rangeHigh = 100;
		    } else {
			TWeightCalculator *weight = (TWeightCalculator*)f.Get(simulationWeight);
			if (weight) {
			    weightCalculator = *weight;
			} else {Error(__FILE__, "Weight %s not found in %s !", simulationWeight.Data(), weightFile.Data());}
		    }
		}
	}
	weightCalculator.Print("");

	weightZSimuSmearingPtMB.mult = 0;
	weightZSimuSmearingPtHT1.mult = 0;
	weightZSimuSmearingPtHT2.mult = 0;
	weightZSimuSmearingZeroPtMB.mult = 0;
	weightZSimuSmearingZeroPtHT1.mult = 0;
	weightZSimuSmearingZeroPtHT2.mult = 0;
	if (simulation && !simulation_pythia) {
	    TFile f(weightFile);
	    if (f.IsOpen()) {
		TH1F *weightZSimuSmearingPtHistogramMB = (TH1F*)f.Get("zSimuSmearingPtMB");
		weightZSimuSmearingPtMB.histogram = weightZSimuSmearingPtHistogramMB;
		TH1F *weightZSimuSmearingPtHistogramHT1 = (TH1F*)f.Get("zSimuSmearingPtHT1");
		weightZSimuSmearingPtHT1.histogram = weightZSimuSmearingPtHistogramHT1;
		TH1F *weightZSimuSmearingPtHistogramHT2 = (TH1F*)f.Get("zSimuSmearingPtHT2");
		weightZSimuSmearingPtHT2.histogram = weightZSimuSmearingPtHistogramHT2;
		TH1F *weightZSimuSmearingZeroPtHistogramMB = (TH1F*)f.Get("zSimuSmearingZeroPtMB");
		weightZSimuSmearingZeroPtMB.histogram = weightZSimuSmearingZeroPtHistogramMB;
		TH1F *weightZSimuSmearingZeroPtHistogramHT1 = (TH1F*)f.Get("zSimuSmearingZeroPtHT1");
		weightZSimuSmearingZeroPtHT1.histogram = weightZSimuSmearingZeroPtHistogramHT1;
		TH1F *weightZSimuSmearingZeroPtHistogramHT2 = (TH1F*)f.Get("zSimuSmearingZeroPtHT2");
		weightZSimuSmearingZeroPtHT2.histogram = weightZSimuSmearingZeroPtHistogramHT2;
	    } else {Error(__FILE__, "Cannot open weights file %s", weightFile.Data());}
	    weightZSimuSmearingPtMB.Print("");
	    weightZSimuSmearingPtHT1.Print("");
	    weightZSimuSmearingPtHT2.Print("");
	    weightZSimuSmearingZeroPtMB.Print("");
	    weightZSimuSmearingZeroPtHT1.Print("");
	    weightZSimuSmearingZeroPtHT2.Print("");
	}

	TF1 *ptShiftMB = 0;
	TF1 *ptShiftHT1 = 0;
	TF1 *ptShiftHT2 = 0;
	if (addPtShiftBins) {
		const Char_t *ptShiftFuncStr = "[0] + ([1] * x) + ([2] * x * x)";
		ptShiftMB = new TF1("ptShiftMB", ptShiftFuncStr);
		ptShiftMB->SetParameter(0, 0.288918);
		ptShiftMB->SetParameter(1, 0.78274);
		ptShiftMB->SetParameter(2, 0.000772084);
		ptShiftHT1 = new TF1("ptShiftHT1", ptShiftFuncStr);
		ptShiftHT1->SetParameter(0, 0.83276);
		ptShiftHT1->SetParameter(1, 0.74119);
		ptShiftHT1->SetParameter(2, 0.00111408);
		ptShiftHT2 = new TF1("ptShiftHT2", ptShiftFuncStr);
		ptShiftHT2->SetParameter(0, 2.11314);
		ptShiftHT2->SetParameter(1, 0.617406);
		ptShiftHT2->SetParameter(2, 0.00404404);
	}
	Float_t pTlimit = 20.0;
	
	TCuts cuts("cuts", "Cuts");
	cuts.setParametersEvent(cutParametersEvent);
	cuts.setParametersPoint(cutParametersPoint);
	cuts.setParametersCandidate(cutParametersCandidate);
	cuts.setParametersGamma(cutParametersGamma);
	cuts.setParametersPion(cutParametersPion);
	TCuts cutsDummy = cuts;

	cuts.EVENT_ALL_CUTS =      EVENT_VALID_CUT
				 | ((simulation && !simulation_pythia) ? EVENT_MC_VALID_CUT : 0)
				 | ((simulation && !simulation_eta_bg && !(simulation_gamma || simulation_nbar) && !simulation_pythia) ? EVENT_MC_VALID_DECAY_CUT : 0)
				 | (simulation ? 0 : EVENT_BAD_RUNS_CUT)
				 | (requireTPCVertex ? EVENT_TPC_VERTEX_CUT : 0)
				 | (requireBBCPresent ? EVENT_BBC_VERTEX_CUT : 0)
				 | EVENT_Z_CUT
				 | (nocentral ? 0 : EVENT_FTPC_REFMULT_CUT)
				 | (simulation ? 0 : EVENT_CORRUPTION_CUT)
				 | (rejectBeamBgEvents ? (/*requireTPCVertex ? EVENT_PT_VS_ET_CUT : */EVENT_PT_VS_ET_TPC_VERTEX_CUT) : 0)
				 | (simulation_pythia && usePythiaBadEventCut ? EVENT_BAD_EVENTS_CUT : 0)
				// | (simulation ? 0 : EVENT_NOBADBUNCH_CUT)
				// | EVENT_NEUTRAL_POINTS_CUT
				// | EVENT_HIGHEST_ADC_CUT
				// | EVENT_HIGHEST_ET_CUT
				// | EVENT_HIGHEST_ADC_STUCK_CUT
				 ;
	cuts.EVENT_ALL_CUTS_NOT = ((simulation && simulation_eta_bg && !(simulation_gamma || simulation_nbar) && !simulation_pythia) ? EVENT_MC_VALID_DECAY_CUT : 0);
	//cuts.EVENT_ALL_CUTS_NOT |= (simulation ? 0 : EVENT_PT_VS_ET_CUT);
	//cuts.EVENT_ALL_CUTS_NOT |= (simulation ? 0 : EVENT_NOBADBUNCH_CUT);
	cuts.POINT_ALL_CUTS =      POINT_VALID_CUT 
				 | POINT_CPV_CUT 
				 | (useMB ? 0 : (POINT_TYPE_SMDE_CUT | POINT_TYPE_SMDP_CUT))
				 | POINT_ETA_COORD_CUT 
				// | POINT_ENERGY_CUT 
				// | POINT_SMDE_ENERGY_CUT | POINT_SMDP_ENERGY_CUT
				 | (useHT1 ? (POINT_SMDE_SIZE_CUT | POINT_SMDP_SIZE_CUT) : 0)
				// | POINT_SMDE_HIGHEST_ADC_CUT | POINT_SMDP_HIGHEST_ADC_CUT
				// | (simulation ? (POINT_ASSOCIATED_CUT) : 0)
				// | (simulation ? (POINT_CLOSEST_CUT) : 0)
				 ;
	//cuts.POINT_ALL_CUTS_NOT = POINT_CPV_CUT;
	//cuts.POINT_ALL_CUTS_NOT = (simulation ? (POINT_ASSOCIATED_CUT | POINT_CLOSEST_CUT) : 0);
	cuts.CANDIDATE_ALL_CUTS =  CANDIDATE_VALID_CUT 
				 | CANDIDATE_ASYMETRY_CUT
				 | CANDIDATE_ETA_CUT
				// | CANDIDATE_PT_CUT
				// | CANDIDATE_MASS_CUT
				;
	cuts.GAMMA_ALL_CUTS =      GAMMA_VALID_CUT
				;
	cuts.PION_ALL_CUTS =       PION_VALID_CUT
				 | (!simulation_eta_bg ? PION_VALID_DECAY_CUT : 0)
				 | PION_ETA_CUT
				// | PION_ACCEPTANCE_NOMINAL_CUT
				;
	cuts.PION_ALL_CUTS_NOT = (simulation_eta_bg ? PION_VALID_DECAY_CUT : 0);
/*
	TCuts cutsAllValidEvents(cuts);
	cutsAllValidEvents.SetNameTitle("cutsAllValidEvents", "Cuts - all valid events");
	cutsAllValidEvents.EVENT_ALL_CUTS = EVENT_VALID_CUT;
	cutsAllValidEvents.EVENT_ALL_CUTS_NOT = 0;

	TCuts cutsAllValidZEvents(cuts);
	cutsAllValidZEvents.SetNameTitle("cutsAllValidZEvents", "Cuts - all valid events + Z cuts");
	cutsAllValidZEvents.EVENT_ALL_CUTS = EVENT_VALID_CUT | EVENT_Z_CUT | EVENT_Z_ZERO_CUT;
	cutsAllValidZEvents.EVENT_ALL_CUTS_NOT = 0;

	TCuts cutsAllValidZCorruptEvents(cuts);
	cutsAllValidZCorruptEvents.SetNameTitle("cutsAllValidZCorruptEvents", "Cuts - all valid events + Z + Corruption cuts");
	cutsAllValidZCorruptEvents.EVENT_ALL_CUTS = EVENT_VALID_CUT | EVENT_Z_CUT | EVENT_Z_ZERO_CUT | EVENT_CORRUPTION_1_CUT;
	cutsAllValidZCorruptEvents.EVENT_ALL_CUTS_NOT = 0;

	TCuts cutsAllValidZCorruptBadEvents(cuts);
	cutsAllValidZCorruptBadEvents.SetNameTitle("cutsAllValidZCorruptBadEvents", "Cuts - all valid events + Z + Corruption + Bad runs cuts");
	cutsAllValidZCorruptBadEvents.EVENT_ALL_CUTS = EVENT_VALID_CUT | EVENT_Z_CUT | EVENT_Z_ZERO_CUT | EVENT_CORRUPTION_1_CUT | EVENT_BAD_RUNS_CUT;
	cutsAllValidZCorruptBadEvents.EVENT_ALL_CUTS_NOT = 0;

	TCuts cutsAllValidZCorruptBadEventsMB(cuts);
	cutsAllValidZCorruptBadEventsMB.SetNameTitle("cutsAllValidZCorruptBadEventsMB", "Cuts - all valid events + Z + Corruption + Bad runs + MinBias");
	cutsAllValidZCorruptBadEventsMB.setParametersEvent(cutParametersEventPPMinBias);
	cutsAllValidZCorruptBadEventsMB.EVENT_ALL_CUTS = EVENT_VALID_CUT | EVENT_Z_CUT | EVENT_Z_ZERO_CUT | EVENT_CORRUPTION_1_CUT | EVENT_BAD_RUNS_CUT | EVENT_MINBIAS_CUT;
	cutsAllValidZCorruptBadEventsMB.EVENT_ALL_CUTS_NOT = 0;
*/
	TCuts cutsMB(cuts);
	cutsMB.SetNameTitle("cutsMB", "Cuts - MinBias");
	cutsMB.setParametersEvent(cutParametersEventPPMinBias);
	cutsMB.EVENT_ALL_CUTS |= (simulation ? 0 : EVENT_MINBIAS_CUT);

	TCuts cutsHT1(cuts);
	cutsHT1.SetNameTitle("cutsHT1", "Cuts - HighTower-1");
	cutsHT1.setParametersEvent(cutParametersEventHT1);
	cutsHT1.EVENT_ALL_CUTS |= (useHighTowerOnlineTrigger ? EVENT_HIGHTOWER1_CUT : 0) | (useHighTowerEt ? EVENT_HIGHTOWER1_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER1_SIMULATED_CUT : 0);
	cutsHT1.CANDIDATE_ALL_CUTS |= (useHighTowerEt ? CANDIDATE_TRIGGERED_HT1_ET_CUT : 0) | ((/*useHighTowerOnlineTrigger || */useHighTowerSimulated) ? CANDIDATE_TRIGGERED_HT1_CUT : 0);

	TCuts cutsHT2(cuts);
	cutsHT2.SetNameTitle("cutsHT2", "Cuts - HighTower-2");
	cutsHT2.setParametersEvent(cutParametersEventHT2);
	cutsHT2.EVENT_ALL_CUTS |= (useHighTowerOnlineTrigger ? EVENT_HIGHTOWER2_CUT : 0) | (useHighTowerEt ? EVENT_HIGHTOWER2_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER2_SIMULATED_CUT : 0);
	cutsHT2.CANDIDATE_ALL_CUTS |= (useHighTowerEt ? CANDIDATE_TRIGGERED_HT2_ET_CUT : 0) | ((/*useHighTowerOnlineTrigger || */useHighTowerSimulated) ? CANDIDATE_TRIGGERED_HT2_CUT : 0);

	TCuts cutsSimMB(cuts);
	cutsSimMB.SetNameTitle("cutsSimMB", "Cuts - sim MinBias");
	cutsSimMB.setParametersEvent(cutParametersEventPPMinBias);

	TCuts cutsSimHT1(cuts);
	cutsSimHT1.SetNameTitle("cutsSimHT1", "Cuts - sim HighTower-1");
	cutsSimHT1.setParametersEvent(cutParametersEventHT1);

	TCuts cutsSimHT2(cuts);
	cutsSimHT2.SetNameTitle("cutsSimHT2", "Cuts - sim HighTower-2");
	cutsSimHT2.setParametersEvent(cutParametersEventHT2);

	TCuts cutsMixMB(cutsMB);
	cutsMixMB.SetNameTitle("cutsMixMB", "Cuts mix - MinBias");
	if (mixRejectEnergyMatch) {
	    cutsMixMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsMixMB.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsMixHT1(cutsHT1);
	cutsMixHT1.SetNameTitle("cutsMixHT1", "Cuts mix - HighTower-1");
	if (mixRejectEnergyMatch) {
	    cutsMixHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsMixHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsMixHT2(cutsHT2);
	cutsMixHT2.SetNameTitle("cutsMixHT2", "Cuts mix - HighTower-2");
	if (mixRejectEnergyMatch) {
	    cutsMixHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsMixHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixMB(cutsMB);
	cutsJetmixMB.SetNameTitle("cutsJetmixMB", "Cuts jet-mix - MinBias");
	cutsJetmixMB.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixMB.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixMB.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixMB.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixHT1(cutsHT1);
	cutsJetmixHT1.SetNameTitle("cutsJetmixHT1", "Cuts jet-mix - HighTower-1");
	cutsJetmixHT1.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixHT1.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixHT1.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixHT2(cutsHT2);
	cutsJetmixHT2.SetNameTitle("cutsJetmixHT2", "Cuts jet-mix - HighTower-2");
	cutsJetmixHT2.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixHT2.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixHT2.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixNotmatchedMB(cutsMB);
	cutsJetmixNotmatchedMB.SetNameTitle("cutsJetmixNotmatchedMB", "Cuts jet-mix not matched - MinBias");
	cutsJetmixNotmatchedMB.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixNotmatchedMB.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixNotmatchedMB.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	cutsJetmixNotmatchedMB.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;
	cutsJetmixNotmatchedMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;

	TCuts cutsJetmixNotmatchedHT1(cutsHT1);
	cutsJetmixNotmatchedHT1.SetNameTitle("cutsJetmixNotmatchedHT1", "Cuts jet-mix not matched - HighTower-1");
	cutsJetmixNotmatchedHT1.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixNotmatchedHT1.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixNotmatchedHT1.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	cutsJetmixNotmatchedHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;
	cutsJetmixNotmatchedHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;

	TCuts cutsJetmixNotmatchedHT2(cutsHT2);
	cutsJetmixNotmatchedHT2.SetNameTitle("cutsJetmixNotmatchedHT2", "Cuts jet-mix not matched - HighTower-2");
	cutsJetmixNotmatchedHT2.setParametersCandidate(cutParametersCandidateJetmix);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixNotmatchedHT2.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixNotmatchedHT2.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	cutsJetmixNotmatchedHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;
	cutsJetmixNotmatchedHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;

	TCuts cutsJetmixBackMB(cutsMB);
	cutsJetmixBackMB.SetNameTitle("cutsJetmixBackMB", "Cuts jet-mix back-to-back - MinBias");
	cutsJetmixBackMB.setParametersCandidate(cutParametersCandidateJetmixBack);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixBackMB.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixBackMB.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixBackMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixBackMB.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixBackHT1(cutsHT1);
	cutsJetmixBackHT1.SetNameTitle("cutsJetmixBackHT1", "Cuts jet-mix bact-to-back - HighTower-1");
	cutsJetmixBackHT1.setParametersCandidate(cutParametersCandidateJetmixBack);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixBackHT1.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixBackHT1.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixBackHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixBackHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsJetmixBackHT2(cutsHT2);
	cutsJetmixBackHT2.SetNameTitle("cutsJetmixBackHT2", "Cuts jet-mix back-to-back - HighTower-2");
	cutsJetmixBackHT2.setParametersCandidate(cutParametersCandidateJetmixBack);
	if (jetMixRequiresJetInEvent) {
	    cutsJetmixBackHT2.EVENT_ALL_CUTS |= EVENT_JET_FOUND_CUT;
	}
	if (jetMixRequiresPointsInJet) {
	    cutsJetmixBackHT2.POINT_ALL_CUTS |= POINT_IN_JET_CUT;
	}
	if (jetMixRejectEnergyMatch) {
	    cutsJetmixBackHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_POINTS_MATCHED_ENERGY_CUT;
	}
	cutsJetmixBackHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_CUT;

	TCuts cutsPSMB(cutsMB);
	cutsPSMB.SetNameTitle("cutsPSMB", "Cuts for HT enhancement calculation - MinBias");
	cutsPSMB.EVENT_ALL_CUTS |= EVENT_MINBIAS_CUT;

	TCuts cutsPSMBHT1(cutsMB);
	cutsPSMBHT1.SetNameTitle("cutsPSMBHT1", "Cuts for HT enhancement calculation - MinBias && sim(HighTower-1)");
	cutsPSMBHT1.EVENT_ALL_CUTS |= EVENT_MINBIAS_CUT | (useHighTowerEt ? EVENT_HIGHTOWER1_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER1_SIMULATED_CUT : 0);

	TCuts cutsPSMBHT2(cutsMB);
	cutsPSMBHT2.SetNameTitle("cutsPSMBHT2", "Cuts for HT enhancement calculation - MinBias && sim(HighTower-2)");
	cutsPSMBHT2.EVENT_ALL_CUTS |= EVENT_MINBIAS_CUT | (useHighTowerEt ? EVENT_HIGHTOWER2_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER2_SIMULATED_CUT : 0);

	TCuts cutsPSHT1(cutsHT1);
	cutsPSHT1.SetNameTitle("cutsPSHT1", "Cuts for HT enhancement calculation - HighTower-1");
	cutsPSHT1.EVENT_ALL_CUTS |= (useHighTowerOnlineTrigger ? EVENT_HIGHTOWER1_CUT : 0) | (useHighTowerEt ? EVENT_HIGHTOWER1_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER1_SIMULATED_CUT : 0);

	TCuts cutsPSHT1HT2(cutsHT1);
	cutsPSHT1HT2.SetNameTitle("cutsPSHT1HT2", "Cuts for HT enhancement calculation - HighTower-1 && sim(HighTower-2)");
	cutsPSHT1HT2.setParametersEvent(cutParametersEventHT2);
	cutsPSHT1HT2.EVENT_ALL_CUTS |= (useHighTowerOnlineTrigger ? EVENT_HIGHTOWER1_CUT : 0) | (useHighTowerEt ? EVENT_HIGHTOWER1_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER1_SIMULATED_CUT : 0) | (useHighTowerEt ? EVENT_HIGHTOWER2_ET_CUT : 0) | (useHighTowerSimulated ? EVENT_HIGHTOWER2_SIMULATED_CUT : 0);

	TCuts cutsMcGammaMB(cutsSimMB);
	cutsMcGammaMB.SetNameTitle("cutsMcGammaMB", "Simulated photons - MinBias");
	cutsMcGammaMB.GAMMA_ALL_CUTS |= GAMMA_ETA_CUT;

	TCuts cutsMcGammaHT1(cutsSimHT1);
	cutsMcGammaHT1.SetNameTitle("cutsMcGammaHT1", "Simulated photons - HighTower-1");
	cutsMcGammaHT1.GAMMA_ALL_CUTS |= GAMMA_ETA_CUT;

	TCuts cutsMcGammaHT2(cutsSimHT2);
	cutsMcGammaHT2.SetNameTitle("cutsMcGammaHT2", "Simulated photons - HighTower-2");
	cutsMcGammaHT2.GAMMA_ALL_CUTS |= GAMMA_ETA_CUT;

	TCuts cutsPointMB(cutsMB);
	cutsPointMB.SetNameTitle("cutsPointMB", "Points - MinBias");
	cutsPointMB.POINT_ALL_CUTS |= POINT_ETA_CUT;

	TCuts cutsPointHT1(cutsHT1);
	cutsPointHT1.SetNameTitle("cutsPointHT1", "Points - HighTower-1");
	cutsPointHT1.POINT_ALL_CUTS |= POINT_ETA_CUT;

	TCuts cutsPointHT2(cutsHT2);
	cutsPointHT2.SetNameTitle("cutsPointHT2", "Points - HighTower-2");
	cutsPointHT2.POINT_ALL_CUTS |= POINT_ETA_CUT;

	TCuts cutsPointNocpvMB(cutsPointMB);
	cutsPointNocpvMB.SetNameTitle("cutsPointNocpvMB", "Points - no cpv cut - MinBias");
	cutsPointNocpvMB.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsPointNocpvHT1(cutsPointHT1);
	cutsPointNocpvHT1.SetNameTitle("cutsPointNocpvHT1", "Points - no cpv cut - HighTower-1");
	cutsPointNocpvHT1.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsPointNocpvHT2(cutsPointHT2);
	cutsPointNocpvHT2.SetNameTitle("cutsPointNocpvHT2", "Points - no cpv cut - HighTower-2");
	cutsPointNocpvHT2.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsPointNotcpvMB(cutsPointMB);
	cutsPointNotcpvMB.SetNameTitle("cutsPointNotcpvMB", "Points - not cpv cut - MinBias");
	cutsPointNotcpvMB.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsPointNotcpvMB.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;

	TCuts cutsPointNotcpvHT1(cutsPointHT1);
	cutsPointNotcpvHT1.SetNameTitle("cutsPointNotcpvHT1", "Points - not cpv cut - HighTower-1");
	cutsPointNotcpvHT1.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsPointNotcpvHT1.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;

	TCuts cutsPointNotcpvHT2(cutsPointHT2);
	cutsPointNotcpvHT2.SetNameTitle("cutsPointNotcpvHT2", "Points - not cpv cut - HighTower-2");
	cutsPointNotcpvHT2.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsPointNotcpvHT2.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;

//	TCuts cutsMcGammaAllValid(cuts);
//	cutsMcGammaAllValid.SetNameTitle("cutsMcGammaAllValid", "All valid simulated photons");

	TCuts cutsPionMB(cutsSimMB);
	cutsPionMB.SetNameTitle("cutsPionMB", "Simulated pions - MinBias");

	TCuts cutsPionHT1(cutsSimHT1);
	cutsPionHT1.SetNameTitle("cutsPionHT1", "Simulated pions - HighTower-1");

	TCuts cutsPionHT2(cutsSimHT2);
	cutsPionHT2.SetNameTitle("cutsPionHT2", "Simulated pions - HighTower-2");

//	TCuts cutsPionAllValid(cuts);
//	cutsPionAllValid.SetNameTitle("cutsPionAllValid", "All valid simulated pions");
//	cutsPionAllValid.PION_ALL_CUTS = PION_VALID_CUT;

	TCuts cutsNocpvMB(cutsMB);
	cutsNocpvMB.SetNameTitle("cutsNocpvMB", "Cuts w/o CPV - MinBias");
	cutsNocpvMB.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsNocpvHT1(cutsHT1);
	cutsNocpvHT1.SetNameTitle("cutsNocpvHT1", "Cuts w/o CPV - HighTower-1");
	cutsNocpvHT1.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsNocpvHT2(cutsHT2);
	cutsNocpvHT2.SetNameTitle("cutsNocpvHT2", "Cuts w/o CPV - HighTower-2");
	cutsNocpvHT2.POINT_ALL_CUTS &= ~POINT_CPV_CUT;

	TCuts cutsNotcpvMB(cutsMB);
	cutsNotcpvMB.SetNameTitle("cutsNotcpvMB", "Cuts w/ not CPV - MinBias");
	cutsNotcpvMB.setParametersPoint(cutParametersPointCharged);
	cutsNotcpvMB.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsNotcpvMB.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;
	//cutsNotcpvMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_CPV_CUT;

	TCuts cutsNotcpvHT1(cutsHT1);
	cutsNotcpvHT1.SetNameTitle("cutsNotcpvHT1", "Cuts w/ not CPV - HighTower-1");
	cutsNotcpvHT1.setParametersPoint(cutParametersPointCharged);
	cutsNotcpvHT1.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsNotcpvHT1.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;
	//cutsNotcpvHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_CPV_CUT;

	TCuts cutsNotcpvHT2(cutsHT2);
	cutsNotcpvHT2.SetNameTitle("cutsNotcpvHT2", "Cuts w/ not CPV - HighTower-2");
	cutsNotcpvHT2.setParametersPoint(cutParametersPointCharged);
	cutsNotcpvHT2.POINT_ALL_CUTS &= ~POINT_CPV_CUT;
	cutsNotcpvHT2.POINT_ALL_CUTS_NOT |= POINT_CPV_CUT;
	//cutsNotcpvHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_CPV_CUT;

/*
	TCuts cutsPeakMB(cutsMB);
	cutsPeakMB.SetNameTitle("cutsPeakMB", "Candidates in the peak region - MinBias");
	cutsPeakMB.CANDIDATE_ALL_CUTS |= CANDIDATE_MASS_CUT;

	TCuts cutsPeakHT1(cutsHT1);
	cutsPeakHT1.SetNameTitle("cutsPeakHT1", "Candidates in the peak region - HighTower-1");
	cutsPeakHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_MASS_CUT;

	TCuts cutsPeakHT2(cutsHT2);
	cutsPeakHT2.SetNameTitle("cutsPeakHT2", "Candidates in the peak region - HighTower-2");
	cutsPeakHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_MASS_CUT;

	TCuts cutsKinTrueMB(cutsMB);
	cutsKinTrueMB.SetNameTitle("cutsKinTrueMB", "Candidates with kinem. constraint - MinBias");
	cutsKinTrueMB.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsKinTrueHT1(cutsHT1);
	cutsKinTrueHT1.SetNameTitle("cutsKinTrueHT1", "Candidates with kinem. constraint - HighTower-1");
	cutsKinTrueHT1.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsKinTrueHT2(cutsHT2);
	cutsKinTrueHT2.SetNameTitle("cutsKinTrueHT2", "Candidates with kinem. constraint - HighTower-2");
	cutsKinTrueHT2.CANDIDATE_ALL_CUTS |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsKinTrueNotMB(cutsMB);
	cutsKinTrueNotMB.SetNameTitle("cutsKinTrueNotMB", "Candidates with inverse kinem. constraint - MinBias");
	cutsKinTrueNotMB.CANDIDATE_ALL_CUTS &= ~CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;
	cutsKinTrueNotMB.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsKinTrueNotHT1(cutsHT1);
	cutsKinTrueNotHT1.SetNameTitle("cutsKinTrueNotHT1", "Candidates with inverse kinem. constraint - HighTower-1");
	cutsKinTrueNotHT1.CANDIDATE_ALL_CUTS &= ~CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;
	cutsKinTrueNotHT1.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsKinTrueNotHT2(cutsHT2);
	cutsKinTrueNotHT2.SetNameTitle("cutsKinTrueNotHT2", "Candidates with inverse kinem. constraint - HighTower-2");
	cutsKinTrueNotHT2.CANDIDATE_ALL_CUTS &= ~CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;
	cutsKinTrueNotHT2.CANDIDATE_ALL_CUTS_NOT |= CANDIDATE_OPENANGLE_KINEMATIC_TRUE_CUT;

	TCuts cutsNobadstripMB(cutsMB);
	cutsNobadstripMB.SetNameTitle("cutsNobadstripMB", "Candidates without adjacent bad strips - MinBias");
	cutsNobadstripMB.POINT_ALL_CUTS |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;

	TCuts cutsNobadstripHT1(cutsHT1);
	cutsNobadstripHT1.SetNameTitle("cutsNobadstripHT1", "Candidates without adjacent bad strips - HighTower-1");
	cutsNobadstripHT1.POINT_ALL_CUTS |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;

	TCuts cutsNobadstripHT2(cutsHT2);
	cutsNobadstripHT2.SetNameTitle("cutsNobadstripHT2", "Candidates without adjacent bad strips - HighTower-2");
	cutsNobadstripHT2.POINT_ALL_CUTS |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;

	TCuts cutsBadstripMB(cutsMB);
	cutsBadstripMB.SetNameTitle("cutsBadstripMB", "Candidates with adjacent bad strips - MinBias");
	cutsBadstripMB.POINT_ALL_CUTS_NOT |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;

	TCuts cutsBadstripHT1(cutsHT1);
	cutsBadstripHT1.SetNameTitle("cutsBadstripHT1", "Candidates with adjacent bad strips - HighTower-1");
	cutsBadstripHT1.POINT_ALL_CUTS_NOT |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;

	TCuts cutsBadstripHT2(cutsHT2);
	cutsBadstripHT2.SetNameTitle("cutsBadstripHT2", "Candidates with adjacent bad strips - HighTower-2");
	cutsBadstripHT2.POINT_ALL_CUTS_NOT |= POINT_SMDE_BADCLOSE_CUT | POINT_SMDP_BADCLOSE_CUT;
*/
/*
	TCuts cutsAbortgapMB(cutsMB);
	cutsAbortgapMB.SetNameTitle("cutsAbortgapMB", "Abort gap events - MinBias");
	cutsAbortgapMB.EVENT_ALL_CUTS |= EVENT_ABORT_GAP_CUT;
	cutsAbortgapMB.EVENT_ALL_CUTS_NOT &= ~EVENT_ABORT_GAP_CUT;

	TCuts cutsAbortgapHT1(cutsHT1);
	cutsAbortgapHT1.SetNameTitle("cutsAbortgapHT1", "Abort gap events - HighTower-1");
	cutsAbortgapHT1.EVENT_ALL_CUTS |= EVENT_ABORT_GAP_CUT;
	cutsAbortgapHT1.EVENT_ALL_CUTS_NOT &= ~EVENT_ABORT_GAP_CUT;

	TCuts cutsAbortgapHT2(cutsHT2);
	cutsAbortgapHT2.SetNameTitle("cutsAbortgapHT2", "Abort gap events - HighTower-2");
	cutsAbortgapHT2.EVENT_ALL_CUTS |= EVENT_ABORT_GAP_CUT;
	cutsAbortgapHT2.EVENT_ALL_CUTS_NOT &= ~EVENT_ABORT_GAP_CUT;
*/
	TH1F histEta("histEta", "Eta;#eta", 200, -0.4, 1.4);
	TH1F histPhi("histPhi", "Phi;#phi", 360, -TMath::Pi(), +TMath::Pi());
	TH2F histEtaPhi("histEtaPhi", "Eta-Phi;#eta;#phi", 100, -0.4, 1.4, 180, -TMath::Pi(), +TMath::Pi());
	TH1F histEtaCoord("histEtaCoord", "EtaCoord;#eta", 200, -0.4, 1.4);
	TH1F histPhiCoord("histPhiCoord", "PhiCoord;#phi", 360, -TMath::Pi(), +TMath::Pi());
	TH2F histEtaPhiCoord("histEtaPhiCoord", "EtaCoord-PhiCoord;#eta;#phi", 100, -0.4, 1.4, 180, -TMath::Pi(), +TMath::Pi());
	TH2F histEtaPhiCoordCircle("histEtaPhiCoordCircle", "Inside is #eta=0, outside is #eta=1;x;y", 40, -1, 1, 40, -1, +1);
	TH1F histOpenAngle("histOpenAngle", "Angle;open angle", 2000, 0, +TMath::Pi()/2);
	TH1F histOpenAngleResolution("histOpenAngleResolution", "Angle reconstruction resolution;reconstructed / simulated open angle", 200, 0, 2);
	TH2F histOpenAngleResolutionEnergy("histOpenAngleResolutionEnergy", "Opening angle resolution;Simulated #pi^{0} energy, GeV;Reconstructed / Simulated opening angle", 60, 0, 30, 200, 0, 2);
	TH2F histOpenAngleResolutionPt("histOpenAngleResolutionPt", "Angle reconstruction resolution vs. simulated p_{T};simulated p_{T};reconstructed / simulated open angle", 60, 0, 15, 200, 0, 2);
	TH1F histYieldDay("histYieldDay", "Yield per day;day", 200, 0, 200);
	TH1F histStatDay("histStatDay", "Statistics per day;day", 200, 0, 200);
	TH1F histStatYear("histStatYear", "Statistics per year;year", 30, 0, 30);
	TH1F histAsymmetry("histAsymmetry", "Asymmetry;asymmetry", 100, -0.1, 1.1);
	TH2F histPtResolution("histPtResolution", "#pi^{0} p_{T} resolution - p_{T MC} vs. p_{T reco};p_{T MC};p_{T reco}", 100, 0, 30, 100, 0, 30);
	TH2F histPtResolutionPercent("histPtResolutionPercent", "#pi^{0} p_{T} resolution;p_{T MC};p_{T reco} / p_{T MC}", 40, 0, 20, 40, 0, 1.5);
	TH1F histPt("histPt", "p_{T};p_{T}", 300, 0, 30);
	TH1F histPartonicPt("histPartonicPt", "PYTHIA partonic p_{T};p_{T}", 800, 0, 40);
	TH2F histPtPartonicPt("histPtPartonicPt", "#pi^{0} vs. PYTHIA partonic p_{T};#pi^{0} p_{T};PYTHIA partonic p_{T}", 80, 0, 20, 200, 0, 50);
	//TH3F histPointPtEtaPhiCoord("histPointPtEtaPhiCoord", "Point p_{T} vs. #eta and #phi;p_{T};#eta;#phi", 40, 0, 20, 20, 0, 1, 60, -TMath::Pi(), +TMath::Pi());
	TH2F histPtMult("histPtMult", "Multiplicity vs. #pi^{0} p_{T};#pi^{0} p_{T}, GeV/c;Multiplicity", 40, 0, 20, 30, 0, 30);
	TH2F histPtM("histPtM", "#pi^{0} M_{inv} vs. p_{T};p_{T}, GeV/c;M_{inv}, GeV/c^{2}", 40, 0, 20, 100, 0, 0.4);
	TH1F histMReco("histMReco", "Reconstructed M_{inv};Reconstructed M_{#gamma #gamma}", 600, 0, 7);
	TH2F histPtOpenAngle("histPtOpenAngle", "#pi^{0} opening angle vs. p_{T};p_{T}, GeV/c;Opening angle", 200, -1, 20, 90, 0, +TMath::Pi()/4);
	TH2F histEnergyOpenAngle("histEnergyOpenAngle", "#pi^{0} opening angle vs. energy;Energy, GeV;Opening angle", 150, 0, 24.99, 150, 0, 0.1);
	TH2F histEnergyOpenAngleMin("histEnergyOpenAngleMin", "#pi^{0} min. opening angle vs. energy;Energy, GeV;Min. opening angle", 200, -1, 30, 90, 0, +TMath::Pi()/4);
	TH2F histEnergyOpenAngleMinTrue("histEnergyOpenAngleMinTrue", "#pi^{0} min. opening angle (true pion mass) vs. energy;Energy, GeV;Min. open angle", 200, -1, 30, 90, 0, +TMath::Pi()/4);
	TH1F histZ("histZ", "Z_{vert};Z_{vert}", 200, -200, 200);
	TH2F histZBBCvsTPC("histZBBCtoTPC", "Z_{BBC} vs. Z_{TPC};Z_{TPC};Z_{BBC}", 60, -100, +100, 60, -100, +100);
	TH1F histZBBCMinusTPC("histZBBCMinusTPC", "Z_{BBC} - Z_{TPC};Z_{BBC} - Z_{TPC}", 200, -200, +200);
	TH2F histBBCWMinusEvsTPC("histBBCWMinusEtoTPC", "BBC W-E vs. Z_{TPC};Z_{TPC};BBC W-E", 60, -100, +100, 100, -50, +50);
	TH2F histZBBCtoTPC("histZBBCtoTPC", "Z_{BBC} - Z_{TPC} vs. Z_{TPC};Z_{TPC};Z_{BBC} - Z_{TPC}", 60, -100, +100, 61, -60, +60);
	TH1F histZDiff("histZDiff", "Z_{vert} - Z_{#pi^{0}};Z_{vert} - Z_{#pi^{0}}", 150, -150, 150);
	TH1F histGammaEnergy("histGammaEnergy", "Simulated #gamma energy;Energy, GeV", 100, -2, 30);
	TH1F histGammaStopRadius("histGammaStopRadius", "Simulated #gamma conversion radius;Conversion radius", 300, 0, 300);
	TH2F histGammaAssociationDelta("histGammaAssociationDelta", "Simulated #gamma association with points;#eta_{#gamma} - #eta_{point};#phi_{#gamma} - #phi_{point}", 100, -0.1, +0.1, 100, -0.1, +0.1);
	TH2F histGammaAssociationEnergy("histGammaAssociationEnergy", "Simulated #gamma energy vs. associated point energy;E_{gamma};E_{point}", 90, 0, 30, 90, 0, 30);
	TH1F histEnergyTotal("histEnergyTotal", "Total energy in event;Energy, GeV", 250, -50, 200);
	TH1F histTracksNumber("histTracksNumber", "Number of primary tracks;Number of primary tracks", 500, 0, 500);
	TH1F histPointsNumber("histPointsNumber", "Number of BEMC points;Number of BEMC points", 100, 0, 100);
	TH1F histClustersNumber("histClustersNumber", "Number of tower clusters;Number of tower clusters", 100, 0, 100);
	TH1F histClustersTracksRatio("histClustersTracksRatio", "Number of tower clusters / Number of primary tracks;Number of tower clusters / Number of primary tracks", 500, -3, 50);
	TH1F histNeutralPointsNumber("histNeutralPointsNumber", "Number of neutral BEMC points;Number of neutral BEMC points", 100, 0, 100);
	TH1F histHighestAdc("histHighestAdc", "Highest ADC in event;Highest tower ADC in the event", 800, 0, 800);
	TH1F histHighestAdcEmbed("histHighestAdcEmbed", "Highest ADC in event - embed;Highest tower ADC in the event", 800, 0, 800);
	TH1F histHighestAdcFinal("histHighestAdcFinal", "Highest ADC in event - final;Highest tower ADC in the event", 800, 0, 800);
	TH2F histDayRun("histDayRun", "Statistics per run;Day;Run", 200, 0, 200, 200, 0, 200);
	//TH3F histDayRunBunchCrossing("histDayRunBunchCrossing", "Bunch crossing 7bit;Day;Run;Bunch crossing 7bit", 69, 111, 180, 180, 0, 180, 128, 0, 128);
	TH1F histNumFtpcPrimaries("histNumFtpcPrimaries", "Number of FTPC primaries;Number of FTPC primaries", 150, 0, 150);
	TH1F histNumFtpcPrimariesEast("histNumFtpcPrimariesEast", "Number of East FTPC primaries;Number of East FTPC primaries", 150, 0, 150);
	TH1F histNumFtpcPrimariesWest("histNumFtpcPrimariesWest", "Number of West FTPC primaries;Number of West FTPC primaries", 150, 0, 150);
	TH2F histNumFtpcPrimariesEastDay("histNumFtpcPrimariesEastDay", "Number of East FTPC primaries vs. day;Day;Number of East FTPC primaries", 100, 0, 100, 50, 0, 50);
	TH2F histNumFtpcPrimariesWestDay("histNumFtpcPrimariesWestDay", "Number of West FTPC primaries vs. day;Day;Number of West FTPC primaries", 100, 0, 100, 50, 0, 50);
	TH2F histNumFtpcPrimariesPrimary("histNumFtpcPrimariesPrimary", "Number of FTPC primaries - all primaries;Number of FTPC primaries;Number of all primaries", 50, 0, 50, 100, 0, 300);
	TH2F histNumFtpcPrimariesPoints("histNumFtpcPrimariesPoints", "Number of FTPC primaries - BEMC points;Number of FTPC primaries;Number of BEMC points", 50, 0, 50, 70, 0, 70);
	TH2F histNumFtpcPrimariesNeutralPoints("histNumFtpcPrimariesNeutralPoints", "Number of FTPC primaries - BEMC neutral points;Number of FTPC primaries;Number of neutral BEMC points", 50, 0, 50, 50, 0, 100);
	TH2F histNumFtpcPrimariesClusters("histNumFtpcPrimariesClusters", "Number of FTPC primaries - BEMC clusters;Number of FTPC primaries;Number of BEMC clusters", 50, 0, 50, 70, 0, 70);
	TH2F histNumTpcPrimariesPoints("histNumTpcPrimariesPoints", "Number of all primaries - BEMC points;Number of all primaries;Number of BEMC points", 100, 0, 300, 70, 0, 70);
	TH2F histNumTpcPrimariesNeutralPoints("histNumTpcPrimariesNeutralPoints", "Number of all primaries - BEMC neutral points;Number of all primaries;Number of neutral BEMC points", 100, 0, 300, 70, 0, 70);
	TH1F histJetEnergy("histJetEnergy", "\"Jet\" energy;\"Jet\" energy, GeV", 160, -5, 30);
	TH2F histTotalEMCJetEnergy("histTotalEmcJetEnergy", "Total EMC vs. \"Jet\" energy;Total EMC energy, GeV;\"Jet\" energy, GeV", 20, 0, 30, 20, 0, 30);
	TH2F histTotalEMCJetSumEnergy("histTotalEmcJetSumEnergy", "Total EMC vs. \"Jet\" + \"Jet2\" energy;Total EMC energy, GeV;\"Jet1\" + \"Jet2\" energy, GeV", 20, 0, 30, 20, 0, 30);
	TH2F histJetEnergy12("histJetEnergy12", "\"Jet1\" vs. \"Jet2\" energy;\"Jet1\" energy, GeV;\"Jet2\" energy, GeV", 20, 0, 30, 20, 0, 30);
	TH1F histEnergyPointTotal("histEnergyPointTotal", "Total energy in point;Energy, GeV", 160, -5, 30);
	TH1F histEnergyPointSmde("histEnergyPointSmde", "Smde energy in point;Energy, GeV", 160, -5, 30);
	TH1F histEnergyPointSmdp("histEnergyPointSmdp", "Smdp energy in point;Energy, GeV", 160, -5, 30);
	TH2F histEnergyPointSmdeSmdp("histEnergyPointSmdeSmdp", "SMDE vs. SMDP energy in point;SMDE energy, GeV;SMDP energy, GeV", 30, 0, 30, 30, 0, 30);
	TH2F histEnergyPointTotalSmde("histEnergyPointTotalSmde", "Total vs. SMDE energy in point;Total energy, GeV;SMDE energy, GeV", 30, 0, 30, 30, 0, 30);
	TH2F histEnergyPointTotalSmdp("histEnergyPointTotalSmdp", "Total vs. SMDP energy in point;Total energy, GeV;SMDP energy, GeV", 30, 0, 30, 30, 0, 30);
	TH2F histEnergyPointTowerSmde("histEnergyPointTowerSmde", "Tower vs. SMDE energy in point;Tower energy, GeV;SMDE energy, GeV", 30, 0, 30, 30, 0, 30);
	TH2F histEnergyPointTowerSmdp("histEnergyPointTowerSmdp", "Tower vs. SMDP energy in point;Tower energy, GeV;SMDP energy, GeV", 30, 0, 30, 30, 0, 30);
	TH2F histEnergyPointTotalTower("histEnergyPointTotalTower", "Total vs. tower energy in point;Total energy, GeV;Tower energy, GeV", 30, 0, 30, 60, 0, 60);
	TH1F histEnergyPointRatioSmdep("histEnergyPointRatioSmdep", "SMDE/SMDP energy ratio in point;Energy ratio", 60, 0, 4);
	TH1F histEnergyPointAsymmetrySmdep("histEnergyPointAsymmetrySmdep", "SMDE and SMDP energy asymmetry in point;Energy asymmetry", 60, -0.1, 1.1);
	TH1F histClusterSizeTower("histClusterSizeTower", "Tower cluster size;Cluster size", 10, -1, 8);
	TH1F histClusterSizeSmde("histClusterSizeSmde", "SMDE cluster size;Cluster size", 10, -1, 8);
	TH1F histClusterSizeSmdp("histClusterSizeSmdp", "SMDP cluster size;Cluster size", 10, -1, 8);
	TH2F histDistTrackMC("histDistTrackMC", "Distance to MC photon;#delta#eta;#delta#phi", 20, -0.01, +0.01, 20, -0.01, +0.01);
	TH2F histDistTrack("histDistTrack", "Distance to charged track;#delta#eta;#delta#phi", 40, -0.7, +0.7, 40, -0.7, +0.7);
	TH1F histDistTrack1D("histDistTrack1D", "Distance to charged track;#sqrt{#delta#eta^{2} + #delta#phi^{2}}", 500, 0, 2.0);
	TH2F histDistTowerCenter("histDistTowerCenter", "Distance to tower center;#delta#eta;#delta#phi", 100, -0.05, +0.05, 100, -0.05, +0.05);
	TH2F histDistTowerCenterEnergy("histDistTowerCenterEnergy", "Distance to tower center - energy;Distance to tower center;Point energy", 30, 0.0, 0.07, 60, -1, 10);
	TH2F histEta1Eta2Coord("histEta1Eta2Coord", "Eta1 vs. Eta2;#eta_{1};#eta_{2}", 50, 0, 1, 50, 0, 1);
	TH2F histPhi1Phi2Coord("histPhi1Phi2Coord", "Phi1 vs. Phi2;#phi_{1};#phi_{2}", 100, -TMath::Pi(), +TMath::Pi(), 100, -TMath::Pi(), +TMath::Pi());
	TH1F histDeltaEtaCoord("histDeltaEtaCoord", "Eta1-Eta2;#eta_{1} - #eta_{2}", 100, -0.5, +0.5);
	TH1F histDeltaPhiCoord("histDeltaPhiCoord", "Phi1-Phi2;#phi_{1} - #phi_{2}", 100, -0.5, +0.5);
	TH2F histDeltaEtaPhiCoord("histDeltaEtaPhiCoord", "Eta1-Eta2 vs. Phi1-Phi2;#eta_{1} - #eta_{2};#phi_{1} - #phi_{2}", 50, -0.3, +0.3, 50, -0.3, +0.3);
	TH2F histJetDeltaEtaPhiCoord("histJetDeltaEtaPhiCoord", "\"Jet\" Eta1-Eta2 vs. Phi1-Phi2;#eta_{1} - #eta_{2};#phi_{1} - #phi_{2}", 20, -1.3, +1.3, 20, -TMath::TwoPi(), +TMath::TwoPi());
	TH1F histTpcRefmult("histTpcRefmult", "Number of primary TPC tracks;Number of primary TPC tracks", 500, 0, 500);
	TH2F histFtpcTpcRefmult("histFtpcTpcRefmult", "Number of FTPC primaries - TPC primaries;Number of FTPC primaries;Number of TPC primaries", 50, 0, 50, 100, 0, 300);
	TH2F histTpcRefmultPoints("histTpcRefmultPoints", "Number of TPC primaries - BEMC points;Number of TPC primaries;Number of BEMC points", 100, 0, 100, 70, 0, 70);
	TH1F histBadStripClose("histBadStripClose", "Bad strip is close (0-none, 1-SMDE, 2-SMDP, 3-both)", 10, 0, 5);
	TH2F histZFtpcRefmult("histZFtpcRefmult", "Number of primary FTPC tracks vs Z;Z;Number of primary FTPC tracks", 80, -80, 80, 50, 0, 100);
	TH2F histZTpcRefmult("histZTpcRefmult", "Number of primary TPC tracks vs Z;Z;Number of primary TPC tracks", 80, -80, 80, 50, 0, 150);
	TH1F histDetector("histDetector", "Detector", 15, 0, 15);
	TH1F histId("histId", "ID", 18000, 0, 18000);
	TH1F histAdc("histAdc", "ADC", 801, -0.5, 800.5);
	TH1F histAdc2("histAdc2", "ADC2", 512+64, -0.5, 4096 - 0.5 + (64*4096/512));
	TH1F histAdc3("histAdc3", "ADC3", 256, 4096 - 240 - 0.5, 4096 + 16 - 0.5);
	TH1F histHitEnergy("histHitEnergy", "Hit energy", 60, 0, 30);
	TH1F histHitEt("histHitEt", "Hit Et", 150, 0, 15);
	TH1F histTotalEMCEt("histTotalEMCEt", "TotalEMCEt", 100, 0, 100);
	TH1F histTotalEMCEtNeutral("histTotalEMCEtNeutral", "TotalEMCEtNeutral", 100, 0, 100);
	TH1F histEMCEtNeutralToTotal("histEMCEtNeutralToTotal", "EMCEtNeutralToTotal", 100, -0.2, 1.2);
	TH1F histTotalEMCE("histTotalEMCE", "TotalEMCE", 100, 0, 100);
	TH1F histTotalEMCENeutral("histTotalEMCENeutral", "TotalEMCENeutral", 100, 0, 100);
	TH1F histEMCENeutralToTotal("histEMCENeutralToTotal", "EMCENeutralToTotal", 100, -0.2, 1.2);
	TH1F histTotalTPCPt("histTotalTPCPt", "TotalTPCPt", 100, 0, 60);
	TH1F histTPCPtToEMCEt("histTPCPtToEMCEt", "TotalTPCPtToEMCEt", 1000, -0.2, 10);
	TH1F histEMCEtToSum("histEMCEtToSum", "TotalEMCEtToSum", 100, -0.2, 1);
	TH2F histTPCPtEMCEt("histTPCPtEMCEt", "Total TPC p_{T} vs. EMC E_{T};Total TPC p_{T}, GeV/c;Total EMC E_{T}, GeV", 60, 0, 60, 40, 0, 100);
	TH1F histTpcGlobals("histTpcGlobals", "Number of global TPC tracks;Number of global TPC tracks", 300, 0, 600);
	//TH2F histNumTpcPrimariesGlobals("histNumTpcPrimariesGlobals", "Number of primary vs. global TPC tracks;Number of global TPC tracks", 50, 0, 150, 100, 0, 700);
	//TH2F histTPCGlobalsEMCEt("histTPCGlobalsEMCEt", "Num of global TPC tracks vs. EMC E_{T};Number of global TPC tracks, GeV/c;Total EMC E_{T}, GeV", 100, 0, 700, 40, 0, 100);
	TH2F histCandidatePtEventEt("histCandidatePtEventEt", "Candidate p_{T} vs. event E_{T};#pi^{0} p_{T}, GeV/c;Event E_{T}, GeV", 30, 0, 15, 30, 0, 15);
	TH2F histCandidatePtJetEt("histCandidatePtJetEt", "Candidate p_{T} vs. jet E_{T};#pi^{0} p_{T}, GeV/c;Jet E_{T}, GeV", 30, 0, 15, 30, 0, 30);
	TH2F histCandidateEEventE("histCandidateEEventE", "Candidate energy vs. event energy;#pi^{0} energy, GeV;Event energy, GeV", 30, 0, 15, 30, 0, 15);
	TH2F histPointJetDist("histPointJetDist", "Point-to-jet distance;#eta_{point} - #eta_{jet};#phi_{point} - #phi_{jet}", 30, -1.3, +1.3, 30, -TMath::TwoPi(), +TMath::TwoPi());
	TH2F histBbcWE("histBbcWE", "BbcWE", 40, -10, +270, 40, -10, +270);
	TH1F histBbcWMinusE("histBbcWMinusE", "BbcWMinusE", 512, -256, +256);
	TH2F histDayPhiCoord("histDayPhiCoord", "#phi coord. vs. day;Day;#phi coord", 100, 0, 100, 36, -TMath::Pi(), +TMath::Pi());
	TH1F histClusterSigmaEta("histClusterSigmaEta", "Eta RMS;#eta RMS", 100, 0, 0.03);
	TH1F histClusterSigmaPhi("histClusterSigmaPhi", "Phi RMS;#phi RMS", 100, 0, 0.03);
	TH2F histSigmaPointSmdeSmdp("histSigmaPointSmdeSmdp", "SMDE vs. SMDP sigma in point;SMDE sigma;SMDP sigma", 50, 0, 0.03, 50, 0, 0.03);
	TH2F histEnergyPointSmdeSigma("histEnergyPointSmdeSigma", "SMDE sigma vs. energy in point;Energy, GeV;SMDE sigma", 50, 0, 30, 50, 0, 0.03);
	TH2F histEnergyPointSmdpSigma("histEnergyPointSmdpSigma", "SMDP sigma vs. energy in point;Energy, GeV;SMDP sigma", 50, 0, 30, 50, 0, 0.03);
	TH2F histEnergyPointSmdeSize("histEnergyPointSmdeSize", "SMDE size vs. energy in point;Energy, GeV;SMDE size", 50, 0, 30, 5, 0.5, 5.5);
	TH2F histEnergyPointSmdpSize("histEnergyPointSmdpSize", "SMDP size vs. energy in point;Energy, GeV;SMDP size", 50, 0, 30, 5, 0.5, 5.5);
	TH2F histNumberHitsBTOWstuckbit("histNumberHitsBTOWstuckbit", "Number of BTOW hits: stuck bits vs. total;Number of BTOW hits;With stuck bits", 200, 0, 4000, 60, 0, 600);
	TH2F histHighestHitToTotalEnergy("histHighestHitToTotalEnergy", "Highset hit / cluster energy;Cluster energy, GeV;Highest hit / cluster energy", 30, 0, 15, 20, 0.2, 1.1);
	TH2F histTpcRefmultTrackDist("histTpcRefmultTrackDist", "Mult. vs track dist;TPC Refmult;Closest track", 25, 0, 50, 20, 0, 0.5);

	THitDataProcessor hitDataProcessor("hitDataProcessor", "Hits processor");
	hitDataProcessor.setTreeName(hitTreeName);
	hitDataProcessor.setBranchName(hitBranchName);
	hitDataProcessor.setCuts(cutsDummy);
	hitDataProcessor.setWeightCalculator(weightCalculator);
	hitDataProcessor.setDetector(histDetector);
	hitDataProcessor.setId(histId);
	hitDataProcessor.setAdc(histAdc);
	hitDataProcessor.setAdc2(histAdc2);
	hitDataProcessor.setAdc3(histAdc3);
	hitDataProcessor.setAdcPed(histAdc);
	hitDataProcessor.setEtaCoord(histEtaCoord);
	hitDataProcessor.setPhiCoord(histPhiCoord);
	hitDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	hitDataProcessor.setEnergy(histHitEnergy);
	hitDataProcessor.setEt(histHitEt);

	TClusterDataProcessor clusterDataProcessor("clusterDataProcessor", "Clusters processor");
	clusterDataProcessor.setTreeName(clusterTreeName);
	clusterDataProcessor.setBranchName(clusterBranchName);
	clusterDataProcessor.setCuts(cutsDummy);
	clusterDataProcessor.setWeightCalculator(weightCalculator);
	clusterDataProcessor.setDetector(histDetector);
	clusterDataProcessor.setSize(histClusterSizeTower);
	clusterDataProcessor.setEnergy(histHitEnergy);
	clusterDataProcessor.setEtaCoord(histEtaCoord);
	clusterDataProcessor.setPhiCoord(histPhiCoord);
	clusterDataProcessor.setSigmaEta(histClusterSigmaEta);
	clusterDataProcessor.setSigmaEtaNarrow(histClusterSigmaEta);
	clusterDataProcessor.setSigmaEtaWide(histClusterSigmaEta);
	clusterDataProcessor.setSigmaPhi(histClusterSigmaPhi);
	clusterDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	clusterDataProcessor.setDeadStripClose(histBadStripClose);
	clusterDataProcessor.setHighestHitToTotalEnergy(histHighestHitToTotalEnergy);
	clusterDataProcessor.highestEnergyHit = hitDataProcessor;
	clusterDataProcessor.highestEnergyHit.SetNameTitle("highestEnergyHit", "Highest energy hit in cluster");

	TPointDataProcessor pointDataProcessor("pointDataProcessor", "Points processor");
	pointDataProcessor.setTreeName(pointTreeName);
	pointDataProcessor.setBranchName(pointBranchName);
	pointDataProcessor.setCuts(cutsDummy);
	pointDataProcessor.setWeightCalculator(weightCalculator);
	pointDataProcessor.clusterTower = clusterDataProcessor;
	pointDataProcessor.clusterTower.SetNameTitle("clusterTower", "Tower cluster");
	pointDataProcessor.clusterSMDE = clusterDataProcessor;
	pointDataProcessor.clusterSMDE.SetNameTitle("clusterSMDE", "SMDE cluster");
	pointDataProcessor.clusterSMDP = clusterDataProcessor;
	pointDataProcessor.clusterSMDP.SetNameTitle("clusterSMDP", "SMDP cluster");
	pointDataProcessor.clusters = clusterDataProcessor;
	pointDataProcessor.clusters.SetNameTitle("clusters", "All clusters in point");
	pointDataProcessor.setEta(histEta);
	pointDataProcessor.setPhi(histPhi);
	pointDataProcessor.setEtaPhi(histEtaPhi);
	pointDataProcessor.setEtaCoord(histEtaCoord);
	pointDataProcessor.setPhiCoord(histPhiCoord);
	pointDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	pointDataProcessor.setEtaPhiCoordCircle(histEtaPhiCoordCircle);
	pointDataProcessor.setPt(histPt);
//	if (wantPhotonRcp) pointDataProcessor.setPtEtaPhiCoord(histPointPtEtaPhiCoord);
	pointDataProcessor.setEnergyTotal(histEnergyPointTotal);
	pointDataProcessor.setEnergySmdeSmdp(histEnergyPointSmdeSmdp);
	pointDataProcessor.setEnergyTotalSmde(histEnergyPointTotalSmde);
	pointDataProcessor.setEnergyTotalSmdp(histEnergyPointTotalSmdp);
	pointDataProcessor.setEnergyTowerSmde(histEnergyPointTowerSmde);
	pointDataProcessor.setEnergyTowerSmdp(histEnergyPointTowerSmdp);
	pointDataProcessor.setEnergyTotalTower(histEnergyPointTotalTower);
	pointDataProcessor.setEnergyAsymmetrySmdep(histEnergyPointAsymmetrySmdep);
	if (simulation) pointDataProcessor.setDistTrackMC(histDistTrackMC);
	pointDataProcessor.setDistTrack(histDistTrack);
	pointDataProcessor.setDistTrack1D(histDistTrack1D);
	pointDataProcessor.setDistTowerCenter(histDistTowerCenter);
	pointDataProcessor.setDistTowerCenterEnergy(histDistTowerCenterEnergy);
	pointDataProcessor.setDistJet(histPointJetDist);
	pointDataProcessor.setDayPhiCoord(histDayPhiCoord);
	pointDataProcessor.setSigmaSmdeSmdp(histSigmaPointSmdeSmdp);
	pointDataProcessor.setEnergyTotalSmdeSigma(histEnergyPointSmdeSigma);
	pointDataProcessor.setEnergyTotalSmdpSigma(histEnergyPointSmdpSigma);
	pointDataProcessor.setEnergyTotalSmdeSize(histEnergyPointSmdeSize);
	pointDataProcessor.setEnergyTotalSmdpSize(histEnergyPointSmdpSize);
	pointDataProcessor.setPtWithSmd(histPt);
	pointDataProcessor.setPtWithSmd1(histPt);
	pointDataProcessor.setPtWithSmdSize(histPt);
	pointDataProcessor.setPtWithSmdSize1(histPt);

	TEventDataProcessor eventDataProcessor("eventDataProcessor", "Events processor");
	eventDataProcessor.setTreeName(eventTreeName);
	eventDataProcessor.setBranchName(eventBranchName);
	eventDataProcessor.setCuts(cutsDummy);
	eventDataProcessor.setWeightCalculator(weightCalculator);
	eventDataProcessor.highestAdcHit = hitDataProcessor;
	eventDataProcessor.highestAdcHit.SetNameTitle("highestAdcHit", "Highest ADC hit in the event");
	eventDataProcessor.highestEtHit = hitDataProcessor;
	eventDataProcessor.highestEtHit.SetNameTitle("highestEtHit", "Highest Et hit in the event");
	eventDataProcessor.setEnergyTotal(histEnergyTotal);
	eventDataProcessor.setZ(histZ);
	eventDataProcessor.setZused(histZ);
	eventDataProcessor.setZbbc(histZ);
	eventDataProcessor.setZBBCvsTPC(histZBBCvsTPC);
	eventDataProcessor.setZBBCMinusTPC(histZBBCMinusTPC);
	eventDataProcessor.setBBCWMinusEvsTPC(histBBCWMinusEvsTPC);
	eventDataProcessor.setZBBCtoTPC(histZBBCtoTPC);
	eventDataProcessor.setTracksNumber(histTracksNumber);
	eventDataProcessor.setPointsNumber(histPointsNumber);
	eventDataProcessor.setClustersNumber(histClustersNumber);
	eventDataProcessor.setClustersTracksRatio(histClustersTracksRatio);
	eventDataProcessor.setHighestAdc(histHighestAdc);
	eventDataProcessor.setHighestAdcEmbed(histHighestAdcEmbed);
	eventDataProcessor.setHighestAdcFinal(histHighestAdcFinal);
	eventDataProcessor.setDayRunAcceptanceBTOW(histDayRun);
	//eventDataProcessor.setDayRunAcceptanceBPRS(histDayRun);
	eventDataProcessor.setDayRunAcceptanceBSMDE(histDayRun);
	eventDataProcessor.setDayRunAcceptanceBSMDP(histDayRun);
	eventDataProcessor.setDayRun(histDayRun);
	eventDataProcessor.setStatDay(histStatDay);
	eventDataProcessor.setStatYear(histStatYear);
	eventDataProcessor.setNumFtpcPrimaries(histNumFtpcPrimaries);
	eventDataProcessor.setNumFtpcPrimariesEast(histNumFtpcPrimariesEast);
	eventDataProcessor.setNumFtpcPrimariesWest(histNumFtpcPrimariesWest);
	eventDataProcessor.setNumFtpcPrimariesEastDay(histNumFtpcPrimariesEastDay);
	eventDataProcessor.setNumFtpcPrimariesWestDay(histNumFtpcPrimariesWestDay);
	//eventDataProcessor.setNumFtpcPrimariesPrimary(histNumFtpcPrimariesPrimary);
	eventDataProcessor.setNumFtpcPrimariesPoints(histNumFtpcPrimariesPoints);
	//eventDataProcessor.setNumFtpcPrimariesNeutralPoints(histNumFtpcPrimariesNeutralPoints);
	//eventDataProcessor.setNumFtpcPrimariesClusters(histNumFtpcPrimariesClusters);
	//eventDataProcessor.setNumTpcPrimariesPoints(histNumTpcPrimariesPoints);
	//eventDataProcessor.setNumTpcPrimariesNeutralPoints(histNumTpcPrimariesNeutralPoints);
	eventDataProcessor.setTpcRefmult(histTpcRefmult);
	eventDataProcessor.setFtpcTpcRefmult(histFtpcTpcRefmult);
	eventDataProcessor.setTpcRefmultPoints(histTpcRefmultPoints);
	//eventDataProcessor.setZFtpcRefmult(histZFtpcRefmult);
	//eventDataProcessor.setZTpcRefmult(histZTpcRefmult);
	eventDataProcessor.setTotalEMCEt(histTotalEMCEt);
	eventDataProcessor.setTotalEMCE(histTotalEMCE);
	eventDataProcessor.setTotalTPCPt(histTotalTPCPt);
	eventDataProcessor.setTPCPtToEMCEt(histTPCPtToEMCEt);
	eventDataProcessor.setTPCPtEMCEt(histTPCPtEMCEt);
	eventDataProcessor.setEMCEtToSum(histEMCEtToSum);
	eventDataProcessor.setTpcGlobals(histTpcGlobals);
	//eventDataProcessor.setNumTpcPrimariesGlobals(histNumTpcPrimariesGlobals);
	//eventDataProcessor.setTPCGlobalsEMCEt(histTPCGlobalsEMCEt);
	eventDataProcessor.setJetEta(histEta);
	eventDataProcessor.setJetPhi(histPhi);
	eventDataProcessor.setJetEnergy(histJetEnergy);
	eventDataProcessor.setBbcWE(histBbcWE);
	eventDataProcessor.setBbcWMinusE(histBbcWMinusE);
	eventDataProcessor.setNumberHitsBTOWstuckbit(histNumberHitsBTOWstuckbit);
	eventDataProcessor.setPartonicPt(histPartonicPt);

	TCandidateDataProcessor candidateDataProcessor("candidateDataProcessor", "#pi^{0} candidates processor");
	candidateDataProcessor.setTreeName(candidateTreeName);
	candidateDataProcessor.setBranchName(candidateBranchName);
	candidateDataProcessor.setCuts(cutsDummy);
	candidateDataProcessor.setWeightCalculator(weightCalculator);
	if (includeCandidatePoints) candidateDataProcessor.points = pointDataProcessor;
	if (includeCandidatePoints) candidateDataProcessor.points.SetNameTitle("points", "Both points");
	if (includeCandidateEvents) candidateDataProcessor.event1 = eventDataProcessor;
	if (includeCandidateEvents) candidateDataProcessor.event1.SetNameTitle("event1", "First event");
	candidateDataProcessor.setEta(histEta);
	candidateDataProcessor.setPhi(histPhi);
	candidateDataProcessor.setEtaPhi(histEtaPhi);
	candidateDataProcessor.setEtaCoord(histEtaCoord);
	candidateDataProcessor.setPhiCoord(histPhiCoord);
	candidateDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	candidateDataProcessor.setOpenAngle(histOpenAngle);
	candidateDataProcessor.setYieldDay(histYieldDay);
	candidateDataProcessor.setAsymmetry(histAsymmetry);
	if (simulation) candidateDataProcessor.setPtResolution(histPtResolution);
	if (simulation) candidateDataProcessor.setPtResolutionPercent(histPtResolutionPercent);
	candidateDataProcessor.setDayRun(histDayRun);
	candidateDataProcessor.setPt(histPt);
	candidateDataProcessor.setPtMult(histPtMult);
	candidateDataProcessor.setPtM(histPtM);
	candidateDataProcessor.setM(histMReco);
	candidateDataProcessor.setPtOpenAngle(histPtOpenAngle);
	candidateDataProcessor.setEnergyOpenAngle(histEnergyOpenAngle);
//	candidateDataProcessor.setEnergyOpenAngleMin(histEnergyOpenAngleMin);
//	candidateDataProcessor.setEnergyOpenAngleMinTrue(histEnergyOpenAngleMinTrue);
	candidateDataProcessor.setEta1Eta2Coord(histEta1Eta2Coord);
	candidateDataProcessor.setPhi1Phi2Coord(histPhi1Phi2Coord);
	candidateDataProcessor.setDeltaEtaCoord(histDeltaEtaCoord);
	candidateDataProcessor.setDeltaPhiCoord(histDeltaPhiCoord);
	candidateDataProcessor.setDeltaEtaPhiCoord(histDeltaEtaPhiCoord);
	candidateDataProcessor.setCandidatePtEventEt(histCandidatePtEventEt);
	candidateDataProcessor.setCandidatePtJetEt(histCandidatePtJetEt);
	candidateDataProcessor.setCandidateEEventE(histCandidateEEventE);
	candidateDataProcessor.setTPCVertYes(histPt);
	candidateDataProcessor.setTPCVertNo(histPt);
	candidateDataProcessor.setTPCVertYesBBCVertYes(histPt);
	candidateDataProcessor.setTPCVertYesBBCVertNo(histPt);
	candidateDataProcessor.setTPCVertNoBBCVertYes(histPt);
	candidateDataProcessor.setTPCVertNoBBCVertNo(histPt);
	candidateDataProcessor.setJetYes(histPt);
	//candidateDataProcessor.setJetNo(histPt);
	candidateDataProcessor.setInJetPointYes(histPt);
	//candidateDataProcessor.setInJetPointNo(histPt);
	candidateDataProcessor.setInJetYes(histPt);
	//candidateDataProcessor.setInJetNo(histPt);
	candidateDataProcessor.setInJBkPtYes(histPt);
	candidateDataProcessor.setPtSmd(histPt);
	candidateDataProcessor.setPtSmd1(histPt);
	candidateDataProcessor.setPtSmd01(histPt);
	candidateDataProcessor.setPtSmdSz(histPt);
	candidateDataProcessor.setPtSmdSz1(histPt);
	candidateDataProcessor.setPtSmdSz01(histPt);
	candidateDataProcessor.setTpcRefmultTrackDist(histTpcRefmultTrackDist);

	TMCGammaDataProcessor mcGammaDataProcessor("mcGamma", "Simulated photons processor");
	mcGammaDataProcessor.setTreeName(simulation_nbar ? mcNbarTreeName : mcGammaTreeName);
	mcGammaDataProcessor.setBranchName(simulation_nbar ? mcNbarBranchName : mcGammaBranchName);
	mcGammaDataProcessor.setCuts(cutsDummy);
	mcGammaDataProcessor.setWeightCalculator(weightCalculator);
	mcGammaDataProcessor.associatedPoint = pointDataProcessor;
	mcGammaDataProcessor.associatedPoint.SetNameTitle("associatedPoint", "Associated EMC point");
	mcGammaDataProcessor.setEta(histEta);
	mcGammaDataProcessor.setPhi(histPhi);
	mcGammaDataProcessor.setEtaPhi(histEtaPhi);
	mcGammaDataProcessor.setEtaCoord(histEtaCoord);
	mcGammaDataProcessor.setPhiCoord(histPhiCoord);
	mcGammaDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	mcGammaDataProcessor.setPt(histPt);
	mcGammaDataProcessor.setStopRadius(histGammaStopRadius);
	mcGammaDataProcessor.setAssociationDelta(histGammaAssociationDelta);
	mcGammaDataProcessor.setAssociationEnergy(histGammaAssociationEnergy);

	TSimuDataProcessor simuDataProcessor("simulated", "Simulated pions processor");
	simuDataProcessor.setTreeName(simulation_eta ? mcEtaTreeName : mcPionTreeName);
	simuDataProcessor.setBranchName(simulation_eta ? mcEtaBranchName : mcPionBranchName);
//	simuDataProcessor.gamma1 = mcGammaDataProcessorMB;
//	simuDataProcessor.gamma1.SetNameTitle("gamma1", "First gamma");
//	simuDataProcessor.gamma2 = mcGammaDataProcessorMB;
//	simuDataProcessor.gamma2.SetNameTitle("gamma2", "Second gamma");
	simuDataProcessor.gammas = mcGammaDataProcessor;
	simuDataProcessor.gammas.SetNameTitle("gammas", "Both gammas");
	simuDataProcessor.setCuts(cutsDummy);
	simuDataProcessor.setWeightCalculator(weightCalculator);
	simuDataProcessor.setPt(histPt);
	if (simulation_pythia) simuDataProcessor.setPtPartonicPt(histPtPartonicPt);
	simuDataProcessor.setMReco(histMReco);
	simuDataProcessor.setEta(histEta);
	simuDataProcessor.setPhi(histPhi);
	simuDataProcessor.setEtaPhi(histEtaPhi);
	simuDataProcessor.setEtaCoord(histEtaCoord);
	simuDataProcessor.setPhiCoord(histPhiCoord);
	simuDataProcessor.setEtaPhiCoord(histEtaPhiCoord);
	simuDataProcessor.setZ(histZ);
	simuDataProcessor.setZDiff(histZDiff);
	simuDataProcessor.setAsymmetry(histAsymmetry);
	simuDataProcessor.setOpenAngleSimu(histOpenAngle);
	simuDataProcessor.setOpenAngleReco(histOpenAngle);
	simuDataProcessor.setOpenAngleResolution(histOpenAngleResolution);
	simuDataProcessor.setOpenAngleResolutionEnergy(histOpenAngleResolutionEnergy);
	simuDataProcessor.setOpenAngleResolutionPt(histOpenAngleResolutionPt);
	simuDataProcessor.setDayRun(histDayRun);
	simuDataProcessor.setStatDay(histStatDay);
	simuDataProcessor.setEnergyOpenAngleSimu(histEnergyOpenAngle);

	TCandidateDataProcessor candidateDataProcessorMB(candidateDataProcessor);
	candidateDataProcessorMB.SetNameTitle("candidateMB", "#pi^{0} candidates processor - MinBias");
	candidateDataProcessorMB.setCuts(cutsMB);

	TCandidateDataProcessor candidateDataProcessorHT1(candidateDataProcessor);
	candidateDataProcessorHT1.SetNameTitle("candidateHT1", "#pi^{0} candidates processor - HighTower-1");
	candidateDataProcessorHT1.setCuts(cutsHT1);

	TCandidateDataProcessor candidateDataProcessorHT2(candidateDataProcessor);
	candidateDataProcessorHT2.SetNameTitle("candidateHT2", "#pi^{0} candidates processor - HighTower-2");
	candidateDataProcessorHT2.setCuts(cutsHT2);

	TMCGammaDataProcessor mcGammaDataProcessorMB(mcGammaDataProcessor);
	mcGammaDataProcessorMB.SetNameTitle("mcGammaMB", "Simulated photons processor - MinBias");
	mcGammaDataProcessorMB.setCuts(cutsMcGammaMB);

	TMCGammaDataProcessor mcGammaDataProcessorHT1(mcGammaDataProcessorMB);
	mcGammaDataProcessorHT1.SetNameTitle("mcGammaHT1", "Simulated photons processor - HighTower-1");
	mcGammaDataProcessorHT1.setCuts(cutsMcGammaHT1);

	TMCGammaDataProcessor mcGammaDataProcessorHT2(mcGammaDataProcessorMB);
	mcGammaDataProcessorHT2.SetNameTitle("mcGammaHT2", "Simulated photons processor - HighTower-2");
	mcGammaDataProcessorHT2.setCuts(cutsMcGammaHT2);

//	TMCGammaDataProcessor mcGammaDataProcessorAllValid(mcGammaDataProcessorMB);
//	mcGammaDataProcessorAllValid.SetNameTitle("mcGammaAllValid", "All valid simulated photons");
//	mcGammaDataProcessorAllValid.setCuts(cutsMcGammaAllValid);

	TSimuDataProcessor simuDataProcessorMB(simuDataProcessor);
	simuDataProcessorMB.SetNameTitle("simulatedMB", "Simulated pions processor - MinBias");
	simuDataProcessorMB.setCuts(cutsPionMB);

	TSimuDataProcessor simuDataProcessorHT1(simuDataProcessorMB);
	simuDataProcessorHT1.SetNameTitle("simulatedHT1", "Simulated pions processor - HighTower-1");
	simuDataProcessorHT1.setCuts(cutsPionHT1);

	TSimuDataProcessor simuDataProcessorHT2(simuDataProcessorMB);
	simuDataProcessorHT2.SetNameTitle("simulatedHT2", "Simulated pions processor - HighTower-2");
	simuDataProcessorHT2.setCuts(cutsPionHT2);

//	TSimuDataProcessor simuDataProcessorAllValid(simuDataProcessorMB);
//	simuDataProcessorAllValid.SetNameTitle("simulatedAllValid", "All valid simulated pions");
//	simuDataProcessorAllValid.setCuts(cutsPionAllValid);

	TDataProcessor dataProcessorEventSummary;
	dataProcessorEventSummary.SetNameTitle("eventSummary", "Events summary");
	dataProcessorEventSummary.setHistogramName(eventSummaryName);

	TDataProcessor dataProcessorTriggerSummary;
	dataProcessorTriggerSummary.SetNameTitle("triggerSummary", "Trigger summary");
	dataProcessorTriggerSummary.setHistogramName(triggerSummaryName);

	TEventDataProcessor eventDataProcessorMB(eventDataProcessor);
	eventDataProcessorMB.SetNameTitle("eventMB", "Events processor - MinBias");
	eventDataProcessorMB.setCuts(cutsMB);

	TEventDataProcessor eventDataProcessorHT1(eventDataProcessor);
	eventDataProcessorHT1.SetNameTitle("eventHT1", "Events processor - HighTower-1");
	eventDataProcessorHT1.setCuts(cutsHT1);

	TEventDataProcessor eventDataProcessorHT2(eventDataProcessor);
	eventDataProcessorHT2.SetNameTitle("eventHT2", "Events processor - HighTower-2");
	eventDataProcessorHT2.setCuts(cutsHT2);

	TEventDataProcessor eventDataProcessorPSMB(eventDataProcessor);
	eventDataProcessorPSMB.SetNameTitle("eventPSMB", "Events processor - HT enhancement - MinBias");
	eventDataProcessorPSMB.setCuts(cutsPSMB);
	TBinParameters binParEventPSMB("binEventPSMB", "PSMB ev num");
	binParEventPSMB.variable = (TBinVariable)0;
	binParEventPSMB.min = 0.0;
	binParEventPSMB.max = 0.0;
	TBinStatistics binStatEventPSMB("binEventPSMB", "PSMB events number");
	binStatEventPSMB.setParameters(binParEventPSMB);
	eventDataProcessorPSMB.binStatistics.push_back(binStatEventPSMB);

	TEventDataProcessor eventDataProcessorPSMBHT1(eventDataProcessor);
	eventDataProcessorPSMBHT1.SetNameTitle("eventPSMBHT1", "Events processor - HT enhancement - MinBias && sim(HighTower-1)");
	eventDataProcessorPSMBHT1.setCuts(cutsPSMBHT1);
	TBinParameters binParEventPSMBHT1("binEventPSMBHT1", "PSMBHT1 ev num");
	binParEventPSMBHT1.variable = (TBinVariable)0;
	binParEventPSMBHT1.min = 0.0;
	binParEventPSMBHT1.max = 0.0;
	TBinStatistics binStatEventPSMBHT1("binEventPSMBHT1", "PSMBHT1 events number");
	binStatEventPSMBHT1.setParameters(binParEventPSMBHT1);
	eventDataProcessorPSMBHT1.binStatistics.push_back(binStatEventPSMBHT1);

	TEventDataProcessor eventDataProcessorPSMBHT2(eventDataProcessor);
	eventDataProcessorPSMBHT2.SetNameTitle("eventPSMBHT2", "Events processor - HT enhancement - MinBias && sim(HighTower-2)");
	eventDataProcessorPSMBHT2.setCuts(cutsPSMBHT2);
	TBinParameters binParEventPSMBHT2("binEventPSMBHT2", "PSMBHT2 ev num");
	binParEventPSMBHT2.variable = (TBinVariable)0;
	binParEventPSMBHT2.min = 0.0;
	binParEventPSMBHT2.max = 0.0;
	TBinStatistics binStatEventPSMBHT2("binEventPSMBHT2", "PSMBHT2 events number");
	binStatEventPSMBHT2.setParameters(binParEventPSMBHT2);
	eventDataProcessorPSMBHT2.binStatistics.push_back(binStatEventPSMBHT2);

	TEventDataProcessor eventDataProcessorPSHT1(eventDataProcessor);
	eventDataProcessorPSHT1.SetNameTitle("eventPSHT1", "Events processor - HT enhancement - HighTower-1");
	eventDataProcessorPSHT1.setCuts(cutsPSHT1);
	TBinParameters binParEventPSHT1("binEventPSHT1", "PSHT1 ev num");
	binParEventPSHT1.variable = (TBinVariable)0;
	binParEventPSHT1.min = 0.0;
	binParEventPSHT1.max = 0.0;
	TBinStatistics binStatEventPSHT1("binEventPSHT1", "PSHT1 events number");
	binStatEventPSHT1.setParameters(binParEventPSHT1);
	eventDataProcessorPSHT1.binStatistics.push_back(binStatEventPSHT1);

	TEventDataProcessor eventDataProcessorPSHT1HT2(eventDataProcessor);
	eventDataProcessorPSHT1HT2.SetNameTitle("eventPSHT1HT2", "Events processor - HT enhancement - HighTower-1 && sim(HighTower-2)");
	eventDataProcessorPSHT1HT2.setCuts(cutsPSHT1HT2);
	TBinParameters binParEventPSHT1HT2("binEventPSHT1HT2", "PSHT1HT2 ev num");
	binParEventPSHT1HT2.variable = (TBinVariable)0;
	binParEventPSHT1HT2.min = 0.0;
	binParEventPSHT1HT2.max = 0.0;
	TBinStatistics binStatEventPSHT1HT2("binEventPSHT1HT2", "PSHT1HT2 events number");
	binStatEventPSHT1HT2.setParameters(binParEventPSHT1HT2);
	eventDataProcessorPSHT1HT2.binStatistics.push_back(binStatEventPSHT1HT2);

/*
	TEventDataProcessor eventDataProcessorAllValid(eventDataProcessor);
	eventDataProcessorAllValid.SetNameTitle("eventAllValid", "All valid events");
	eventDataProcessorAllValid.setCuts(cutsAllValidEvents);
	eventDataProcessorAllValid.setDayRunBunchCrossing(histDayRunBunchCrossing);
	eventDataProcessorAllValid.setDayRunBunchCrossingOffset(histDayRunBunchCrossing);

	TEventDataProcessor eventDataProcessorAllValidZ(eventDataProcessor);
	eventDataProcessorAllValidZ.SetNameTitle("eventAllValidZ", "All valid events + Z cuts");
	eventDataProcessorAllValidZ.setCuts(cutsAllValidZEvents);

	TEventDataProcessor eventDataProcessorAllValidZCorrupt(eventDataProcessor);
	eventDataProcessorAllValidZCorrupt.SetNameTitle("eventAllValidZCorrupt", "All valid events + Z + Corruption cuts");
	eventDataProcessorAllValidZCorrupt.setCuts(cutsAllValidZCorruptEvents);

	TEventDataProcessor eventDataProcessorAllValidZCorruptBad(eventDataProcessor);
	eventDataProcessorAllValidZCorruptBad.SetNameTitle("eventAllValidZCorruptBad", "All valid events + Z + Corruption + Bad runs cuts");
	eventDataProcessorAllValidZCorruptBad.setCuts(cutsAllValidZCorruptBadEvents);

	TEventDataProcessor eventDataProcessorAllValidZCorruptBadMB(eventDataProcessor);
	eventDataProcessorAllValidZCorruptBadMB.SetNameTitle("eventAllValidZCorruptBadMB", "All valid events + Z + Corruption + Bad runs + MinBias");
	eventDataProcessorAllValidZCorruptBadMB.setCuts(cutsAllValidZCorruptBadEventsMB);

	TEventDataProcessor eventDataProcessorNotvsMB(eventDataProcessor);
	eventDataProcessorNotvsMB.SetNameTitle("eventDataProcessorNotvsMB", "Events processor - beam bg - MinBias");
	eventDataProcessorNotvsMB.setCuts(cutsNotvsMB);

	TEventDataProcessor eventDataProcessorNotvsHT1(eventDataProcessor);
	eventDataProcessorNotvsHT1.SetNameTitle("eventDataProcessorNotvsHT1", "Events processor - beam bg - HighTower-1");
	eventDataProcessorNotvsHT1.setCuts(cutsNotvsHT1);

	TEventDataProcessor eventDataProcessorNotvsHT2(eventDataProcessor);
	eventDataProcessorNotvsHT2.SetNameTitle("eventDataProcessorNotvsHT2", "Events processor - beam bg - HighTower-2");
	eventDataProcessorNotvsHT2.setCuts(cutsNotvsHT2);

	TEventDataProcessor eventDataProcessorAbortgapMB(eventDataProcessor);
	eventDataProcessorAbortgapMB.SetNameTitle("eventDataProcessorAbortgapMB", "Events processor - abort gap - MinBias");
	eventDataProcessorAbortgapMB.setCuts(cutsAbortgapMB);

	TEventDataProcessor eventDataProcessorAbortgapHT1(eventDataProcessor);
	eventDataProcessorAbortgapHT1.SetNameTitle("eventDataProcessorAbortgapHT1", "Events processor - abort gap - HighTower-1");
	eventDataProcessorAbortgapHT1.setCuts(cutsAbortgapHT1);

	TEventDataProcessor eventDataProcessorAbortgapHT2(eventDataProcessor);
	eventDataProcessorAbortgapHT2.SetNameTitle("eventDataProcessorAbortgapHT2", "Events processor - abortgap - HighTower-2");
	eventDataProcessorAbortgapHT2.setCuts(cutsAbortgapHT2);
*/
	TPointDataProcessor pointDataProcessorMB(pointDataProcessor);
	pointDataProcessorMB.SetNameTitle("pointMB", "Points processor - MinBias");
	pointDataProcessorMB.setCuts(cutsPointMB);
	//if (wantPhotonRcp) pointDataProcessorMB.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorHT1(pointDataProcessor);
	pointDataProcessorHT1.SetNameTitle("pointHT1", "Points processor - HighTower-1");
	pointDataProcessorHT1.setCuts(cutsPointHT1);
	//if (wantPhotonRcp) pointDataProcessorHT1.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorHT2(pointDataProcessor);
	pointDataProcessorHT2.SetNameTitle("pointHT2", "Points processor - HighTower-2");
	pointDataProcessorHT2.setCuts(cutsPointHT2);
	//if (wantPhotonRcp) pointDataProcessorHT2.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, &simuDataProcessorMB, 0, 0, ptShiftMB);
	if (simulation) {
		addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 4, 0, ptShiftMB);
	}
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 5, 0, ptShiftMB);
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 3, 0, ptShiftMB);
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 6, (!simulation || simulation_pythia) ? &pointDataProcessorMB : 0, ptShiftMB);
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 7, 0, ptShiftMB);
/*
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorKinTrueMB, 0, 0);

	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorKinTrueNotMB, 0, 0);
*/
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 1, &pointDataProcessorMB);
	if (wantMultiplicityRcp) {
/*
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorPeakMB, 0, 1);
*/
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 2, &pointDataProcessorMB);
/*
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorPeakMB, 0, 2);
*/
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorMB, 0, 3, &pointDataProcessorMB);
/*
	addPtBin(0, pTlimit, 0.25, "MB", "MinBias", &candidateDataProcessorPeakMB, 0, 3);
*/
	}
	TBinParameters binParCumulativeMB("invMB", "MinBias, 1.5 < p_{T}");
	binParCumulativeMB.variable = (TBinVariable)0;
	binParCumulativeMB.min = 1.5;
	binParCumulativeMB.max = pTlimit;
	TH1F invHistCumulativeMB("invMB", "MinBias, 1.5 < p_{T}", 400, 0, 5.0);
	invHistCumulativeMB.Sumw2();
	TInvariantMassDistribution invCumulativeMB("invMB", "MinBias, 1.5 < p_{T}");
	invCumulativeMB.setBinParameters(binParCumulativeMB);
	invCumulativeMB.setDistribution(invHistCumulativeMB);
	candidateDataProcessorMB.invariantMassDistributions.push_back(invCumulativeMB);
//	candidateDataProcessorKinTrueMB.invariantMassDistributions.push_back(invCumulativeMB);
//	candidateDataProcessorKinTrueNotMB.invariantMassDistributions.push_back(invCumulativeMB);
	TBinStatistics binStatCumulativeMB("binStatMB", "MinBias, 1.5 < p_{T}");
	binStatCumulativeMB.setParameters(binParCumulativeMB);
	simuDataProcessorMB.binStatistics.push_back(binStatCumulativeMB);
	TBinParameters binParEventMB("binEventMB", "MB ev num");
	binParEventMB.variable = (TBinVariable)0;
	binParEventMB.min = 0.0;
	binParEventMB.max = 0.0;
	TBinStatistics binStatEventMB("binEventMB", "MB events number");
	binStatEventMB.setParameters(binParEventMB);
	eventDataProcessorMB.binStatistics.push_back(binStatEventMB);
	TBinParameters binParEvnumSim("binParEvnumSim", "MinBias events number");
	binParEvnumSim.variable = (TBinVariable)0;
	binParEvnumSim.min = 0;
	binParEvnumSim.max = 1000;
	TBinStatistics binStatEvnumSim("binStatEvnumSim", "MinBias events number");
	binStatEvnumSim.setParameters(binParEvnumSim);
	simuDataProcessorMB.binStatistics.push_back(binStatEvnumSim);

	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, &simuDataProcessorHT1, 0, 0, ptShiftHT1);
	if (simulation) {
		addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 4, 0, ptShiftHT1);
	}
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 5, 0, ptShiftHT1);
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 3, 0, ptShiftHT1);
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 6, (!simulation || simulation_pythia) ? &pointDataProcessorHT1 : 0, ptShiftHT1);
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 7, 0, ptShiftHT1);
/*
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorKinTrueHT1, 0, 0);

	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorKinTrueNotHT1, 0, 0);
*/
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 1, &pointDataProcessorHT1);
	if (wantMultiplicityRcp) {
/*
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorPeakHT1, 0, 1);
*/
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 2, &pointDataProcessorHT1);
/*
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorPeakHT1, 0, 2);
*/
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorHT1, 0, 3, &pointDataProcessorHT1);
/*
	addPtBin(0, pTlimit, 0.5, "HT1", "HighTower-1", &candidateDataProcessorPeakHT1, 0, 3);
*/
	}
	TBinParameters binParCumulativeHT1("invHT1", "HighTower-1, 2 < p_{T}");
	binParCumulativeHT1.variable = (TBinVariable)0;
	binParCumulativeHT1.min = 2.0;
	binParCumulativeHT1.max = pTlimit;
	TH1F invHistCumulativeHT1("invHT1", "HighTower-1, 2 < p_{T}", 400, 0, 5.0);
	invHistCumulativeHT1.Sumw2();
	TInvariantMassDistribution invCumulativeHT1("invHT1", "HighTower-1, 2 < p_{T}");
	invCumulativeHT1.setBinParameters(binParCumulativeHT1);
	invCumulativeHT1.setDistribution(invHistCumulativeHT1);
	candidateDataProcessorHT1.invariantMassDistributions.push_back(invCumulativeHT1);
//	candidateDataProcessorKinTrueHT1.invariantMassDistributions.push_back(invCumulativeHT1);
//	candidateDataProcessorKinTrueNotHT1.invariantMassDistributions.push_back(invCumulativeHT1);
	TBinStatistics binStatCumulativeHT1("binStatHT1", "HighTower-1, 2 < p_{T}");
	binStatCumulativeHT1.setParameters(binParCumulativeHT1);
	simuDataProcessorHT1.binStatistics.push_back(binStatCumulativeHT1);
	TBinParameters binParEventHT1("binEventHT1", "HT1 ev num");
	binParEventHT1.variable = (TBinVariable)0;
	binParEventHT1.min = 0.0;
	binParEventHT1.max = 0.0;
	TBinStatistics binStatEventHT1("binEventHT1", "HT1 events number");
	binStatEventHT1.setParameters(binParEventHT1);
	eventDataProcessorHT1.binStatistics.push_back(binStatEventHT1);

	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, &simuDataProcessorHT2, 0, 0, ptShiftHT2);
	if (simulation) {
		addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 4, 0, ptShiftHT2);
	}
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 5, 0, ptShiftHT2);
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 3, 0, ptShiftHT2);
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 6, (!simulation || simulation_pythia) ? &pointDataProcessorHT2 : 0, ptShiftHT2);
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 7, 0, ptShiftHT2);
/*
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorKinTrueHT2, 0, 0);

	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorKinTrueNotHT2, 0, 0);
*/
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 1, &pointDataProcessorHT2);
	if (wantMultiplicityRcp) {
/*
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorPeakHT2, 0, 1);
*/	
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 2, &pointDataProcessorHT2);
/*
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorPeakHT2, 0, 2);
*/
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorHT2, 0, 3, &pointDataProcessorHT2);
/*
	addPtBin(0, pTlimit, 0.5, "HT2", "HighTower-2", &candidateDataProcessorPeakHT2, 0, 3);
*/
	}
	TBinParameters binParCumulativeHT2("invHT2", "HighTower-2, 2.5 < p_{T}");
	binParCumulativeHT2.variable = (TBinVariable)0;
	binParCumulativeHT2.min = 2.5;
	binParCumulativeHT2.max = pTlimit;
	TH1F invHistCumulativeHT2("invHT2", "HighTower-2, 2.5 < p_{T}", 400, 0, 5.0);
	invHistCumulativeHT2.Sumw2();
	TInvariantMassDistribution invCumulativeHT2("invHT2", "HighTower-2, 2.5 < p_{T}");
	invCumulativeHT2.setBinParameters(binParCumulativeHT2);
	invCumulativeHT2.setDistribution(invHistCumulativeHT2);
	candidateDataProcessorHT2.invariantMassDistributions.push_back(invCumulativeHT2);
//	candidateDataProcessorKinTrueHT2.invariantMassDistributions.push_back(invCumulativeHT2);
//	candidateDataProcessorKinTrueNotHT2.invariantMassDistributions.push_back(invCumulativeHT2);
	TBinStatistics binStatCumulativeHT2("binStatHT2", "HighTower-2, 2.5 < p_{T}");
	binStatCumulativeHT2.setParameters(binParCumulativeHT2);
	simuDataProcessorHT2.binStatistics.push_back(binStatCumulativeHT2);
	TBinParameters binParEventHT2("binEventHT2", "HT2 ev num");
	binParEventHT2.variable = (TBinVariable)0;
	binParEventHT2.min = 0.0;
	binParEventHT2.max = 0.0;
	TBinStatistics binStatEventHT2("binEventHT2", "HT2 events number");
	binStatEventHT2.setParameters(binParEventHT2);
	eventDataProcessorHT2.binStatistics.push_back(binStatEventHT2);

	TCandidateDataProcessor candidateDataProcessorMBMix(candidateDataProcessorMB);
//	TCandidateDataProcessor candidateDataProcessorMBSubmix(candidateDataProcessorMB);
//	TCandidateDataProcessor candidateDataProcessorMBShuffle(candidateDataProcessorMB);
	TCandidateDataProcessor candidateDataProcessorMBJetmix(candidateDataProcessorMB);
//	TCandidateDataProcessor candidateDataProcessorMBJetmixNotmatched(candidateDataProcessorMB);
	TCandidateDataProcessor candidateDataProcessorMBJetmixBack(candidateDataProcessorMB);
	TCandidateDataProcessor candidateDataProcessorMBNocpv(candidateDataProcessorMB);
	TCandidateDataProcessor candidateDataProcessorMBNotcpv(candidateDataProcessorMB);
	candidateDataProcessorMBMix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorMBShuffle.setTreeName(candidateTreeSubmixName);
	candidateDataProcessorMBJetmix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorMBJetmixNotmatched.setTreeName(candidateTreeMixName);
	candidateDataProcessorMBJetmixBack.setTreeName(candidateTreeMixName);
	candidateDataProcessorMBMix.SetNameTitle("candidateMBMix", "Mixed events - MinBias");
	candidateDataProcessorMBMix.setCuts(cutsMixMB);
//	candidateDataProcessorMBSubmix.SetNameTitle("candidateMBSubmix", "Mixed subevents - MinBias");
//	candidateDataProcessorMBShuffle.SetNameTitle("candidateMBShuffle", "Shuffled subevents - MinBias");
	candidateDataProcessorMBJetmix.SetNameTitle("candidateMBJetmix", "Jet-mixed events - MinBias");
	candidateDataProcessorMBJetmix.setCuts(cutsJetmixMB);
//	candidateDataProcessorMBJetmixNotmatched.SetNameTitle("candidateMBJetmixNotmatched", "Jet-mixed events not matched - MinBias");
//	candidateDataProcessorMBJetmixNotmatched.setCuts(cutsJetmixNotmatchedMB);
	candidateDataProcessorMBJetmixBack.SetNameTitle("candidateMBJetmixBack", "Jet-mixed back-to-back events - MinBias");
	candidateDataProcessorMBJetmixBack.setCuts(cutsJetmixBackMB);
	candidateDataProcessorMBNocpv.SetNameTitle("candidateMBNocpv", "No CPV - MinBias");
	candidateDataProcessorMBNocpv.setCuts(cutsNocpvMB);
	candidateDataProcessorMBNotcpv.SetNameTitle("candidateMBNotcpv", "Not CPV - MinBias");
	candidateDataProcessorMBNotcpv.setCuts(cutsNotcpvMB);

//	TCandidateDataProcessor candidateDataProcessorHT1BadTrig(candidateDataProcessorHT1);
	TCandidateDataProcessor candidateDataProcessorHT1Mix(candidateDataProcessorHT1);
//	TCandidateDataProcessor candidateDataProcessorHT1Submix(candidateDataProcessorHT1);
//	TCandidateDataProcessor candidateDataProcessorHT1Shuffle(candidateDataProcessorHT1);
	TCandidateDataProcessor candidateDataProcessorHT1Jetmix(candidateDataProcessorHT1);
//	TCandidateDataProcessor candidateDataProcessorHT1JetmixNotmatched(candidateDataProcessorHT1);
	TCandidateDataProcessor candidateDataProcessorHT1JetmixBack(candidateDataProcessorHT1);
	TCandidateDataProcessor candidateDataProcessorHT1Nocpv(candidateDataProcessorHT1);
	TCandidateDataProcessor candidateDataProcessorHT1Notcpv(candidateDataProcessorHT1);
	candidateDataProcessorHT1Mix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT1Shuffle.setTreeName(candidateTreeSubmixName);
	candidateDataProcessorHT1Jetmix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT1JetmixNotmatched.setTreeName(candidateTreeMixName);
	candidateDataProcessorHT1JetmixBack.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT1BadTrig.SetNameTitle("candidateHT1BadTrig", "Bad trigger - HighTower-1");
//	candidateDataProcessorHT1BadTrig.setCuts(cutsBadTrigHT1);
	candidateDataProcessorHT1Mix.SetNameTitle("candidateHT1Mix", "Mixed events - HighTower-1");
	candidateDataProcessorHT1Mix.setCuts(cutsMixHT1);
//	candidateDataProcessorHT1Submix.SetNameTitle("candidateHT1Submix", "Mixed subevents - HighTower-1");
//	candidateDataProcessorHT1Shuffle.SetNameTitle("candidateHT1Shuffle", "Shuffled subevents - HighTower-1");
	candidateDataProcessorHT1Jetmix.SetNameTitle("candidateHT1Jetmix", "Jet-mixed events - HighTower-1");
	candidateDataProcessorHT1Jetmix.setCuts(cutsJetmixHT1);
//	candidateDataProcessorHT1JetmixNotmatched.SetNameTitle("candidateHT1JetmixNotmatched", "Jet-mixed events not matched - HighTower-1");
//	candidateDataProcessorHT1JetmixNotmatched.setCuts(cutsJetmixNotmatchedHT1);
	candidateDataProcessorHT1JetmixBack.SetNameTitle("candidateHT1JetmixBack", "Jet-mixed back-to-back events - HighTower-1");
	candidateDataProcessorHT1JetmixBack.setCuts(cutsJetmixBackHT1);
	candidateDataProcessorHT1Nocpv.SetNameTitle("candidateHT1Nocpv", "No CPV - HighTower-1");
	candidateDataProcessorHT1Nocpv.setCuts(cutsNocpvHT1);
	candidateDataProcessorHT1Notcpv.SetNameTitle("candidateHT1Notcpv", "Not CPV - HighTower-1");
	candidateDataProcessorHT1Notcpv.setCuts(cutsNotcpvHT1);

//	TCandidateDataProcessor candidateDataProcessorHT2BadTrig(candidateDataProcessorHT2);
	TCandidateDataProcessor candidateDataProcessorHT2Mix(candidateDataProcessorHT2);
//	TCandidateDataProcessor candidateDataProcessorHT2Submix(candidateDataProcessorHT2);
//	TCandidateDataProcessor candidateDataProcessorHT2Shuffle(candidateDataProcessorHT2);
	TCandidateDataProcessor candidateDataProcessorHT2Jetmix(candidateDataProcessorHT2);
//	TCandidateDataProcessor candidateDataProcessorHT2JetmixNotmatched(candidateDataProcessorHT2);
	TCandidateDataProcessor candidateDataProcessorHT2JetmixBack(candidateDataProcessorHT2);
	TCandidateDataProcessor candidateDataProcessorHT2Nocpv(candidateDataProcessorHT2);
	TCandidateDataProcessor candidateDataProcessorHT2Notcpv(candidateDataProcessorHT2);
	candidateDataProcessorHT2Mix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT2Shuffle.setTreeName(candidateTreeSubmixName);
	candidateDataProcessorHT2Jetmix.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT2JetmixNotmatched.setTreeName(candidateTreeMixName);
	candidateDataProcessorHT2JetmixBack.setTreeName(candidateTreeMixName);
//	candidateDataProcessorHT2BadTrig.SetNameTitle("candidateHT2BadTrig", "Bad trigger - HighTower-2");
//	candidateDataProcessorHT2BadTrig.setCuts(cutsBadTrigHT2);
	candidateDataProcessorHT2Mix.SetNameTitle("candidateHT2Mix", "Mixed events - HighTower-2");
	candidateDataProcessorHT2Mix.setCuts(cutsMixHT2);
//	candidateDataProcessorHT2Submix.SetNameTitle("candidateHT2Submix", "Mixed subevents - HighTower-2");
//	candidateDataProcessorHT2Shuffle.SetNameTitle("candidateHT2Shuffle", "Shuffled subevents - HighTower-2");
	candidateDataProcessorHT2Jetmix.SetNameTitle("candidateHT2Jetmix", "Jet-mixed events - HighTower-2");
	candidateDataProcessorHT2Jetmix.setCuts(cutsJetmixHT2);
//	candidateDataProcessorHT2JetmixNotmatched.SetNameTitle("candidateHT2JetmixNotmatched", "Jet-mixed events not matched - HighTower-2");
//	candidateDataProcessorHT2JetmixNotmatched.setCuts(cutsJetmixNotmatchedHT2);
	candidateDataProcessorHT2JetmixBack.SetNameTitle("candidateHT2JetmixBack", "Jet-mixed back-to-back events - HighTower-2");
	candidateDataProcessorHT2JetmixBack.setCuts(cutsJetmixBackHT2);
	candidateDataProcessorHT2Nocpv.SetNameTitle("candidateHT2Nocpv", "No CPV - HighTower-2");
	candidateDataProcessorHT2Nocpv.setCuts(cutsNocpvHT2);
	candidateDataProcessorHT2Notcpv.SetNameTitle("candidateHT2Notcpv", "Not CPV - HighTower-2");
	candidateDataProcessorHT2Notcpv.setCuts(cutsNotcpvHT2);

	TPointDataProcessor pointDataProcessorNocpvMB(pointDataProcessorMB);
	TPointDataProcessor pointDataProcessorNotcpvMB(pointDataProcessorMB);
	pointDataProcessorNocpvMB.SetNameTitle("pointNocpvMB", "Points processor - no cpv cut - MinBias");
	pointDataProcessorNotcpvMB.SetNameTitle("pointNotcpvMB", "Points processor - not cpv cut - MinBias");
	pointDataProcessorNocpvMB.setCuts(cutsPointNocpvMB);
	pointDataProcessorNotcpvMB.setCuts(cutsPointNotcpvMB);

	TPointDataProcessor pointDataProcessorNocpvHT1(pointDataProcessorHT1);
	TPointDataProcessor pointDataProcessorNotcpvHT1(pointDataProcessorHT1);
	pointDataProcessorNocpvHT1.SetNameTitle("pointNocpvHT1", "Points processor - no cpv cut - HighTower-1");
	pointDataProcessorNotcpvHT1.SetNameTitle("pointNotcpvHT1", "Points processor - not cpv cut - HighTower-1");
	pointDataProcessorNocpvHT1.setCuts(cutsPointNocpvHT1);
	pointDataProcessorNotcpvHT1.setCuts(cutsPointNotcpvHT1);

	TPointDataProcessor pointDataProcessorNocpvHT2(pointDataProcessorHT2);
	TPointDataProcessor pointDataProcessorNotcpvHT2(pointDataProcessorHT2);
	pointDataProcessorNocpvHT2.SetNameTitle("pointNocpvHT2", "Points processor - no cpv cut - HighTower-2");
	pointDataProcessorNotcpvHT2.SetNameTitle("pointNotcpvHT2", "Points processor - not cpv cut - HighTower-2");
	pointDataProcessorNocpvHT2.setCuts(cutsPointNocpvHT2);
	pointDataProcessorNotcpvHT2.setCuts(cutsPointNotcpvHT2);

/*
	TEventDataProcessor eventDataProcessorHT1BadTrig(eventDataProcessorHT1);
	eventDataProcessorHT1BadTrig.SetNameTitle("eventHT1BadTrig", "Bad trigger events - HighTower-1");
	eventDataProcessorHT1BadTrig.setCuts(cutsBadTrigHT1);

	TEventDataProcessor eventDataProcessorHT2BadTrig(eventDataProcessorHT2);
	eventDataProcessorHT2BadTrig.SetNameTitle("eventHT2BadTrig", "Bad trigger events - HighTower-2");
	eventDataProcessorHT2BadTrig.setCuts(cutsBadTrigHT2);
*/
/*
	TPointDataProcessor pointDataProcessorChargedMB(pointDataProcessorMB);
	pointDataProcessorChargedMB.SetNameTitle("pointChargedMB", "Points processor - charged MinBias");
	pointDataProcessorChargedMB.setCuts(cutsChargedPointsMB);
	if (wantPhotonRcp) pointDataProcessorChargedMB.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorChargedHT1(pointDataProcessorHT1);
	pointDataProcessorChargedHT1.SetNameTitle("pointChargedHT1", "Points processor - charged HighTower-1");
	pointDataProcessorChargedHT1.setCuts(cutsChargedPointsHT1);
	if (wantPhotonRcp) pointDataProcessorChargedHT1.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorChargedHT2(pointDataProcessorHT2);
	pointDataProcessorChargedHT2.SetNameTitle("pointChargedHT2", "Points processor - charged HighTower-2");
	pointDataProcessorChargedHT2.setCuts(cutsChargedPointsHT2);
	if (wantPhotonRcp) pointDataProcessorChargedHT2.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorAllMB(pointDataProcessorMB);
	pointDataProcessorAllMB.SetNameTitle("pointAllMB", "Points processor - all MinBias");
	pointDataProcessorAllMB.setCuts(cutsAllPointsMB);
	if (wantPhotonRcp) pointDataProcessorAllMB.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorAllHT1(pointDataProcessorHT1);
	pointDataProcessorAllHT1.SetNameTitle("pointAllHT1", "Points processor - all HighTower-1");
	pointDataProcessorAllHT1.setCuts(cutsAllPointsHT1);
	if (wantPhotonRcp) pointDataProcessorAllHT1.setPtEtaPhiCoord(histPointPtEtaPhiCoord);

	TPointDataProcessor pointDataProcessorAllHT2(pointDataProcessorHT2);
	pointDataProcessorAllHT2.SetNameTitle("pointAllHT2", "Points processor - all HighTower-2");
	pointDataProcessorAllHT2.setCuts(cutsAllPointsHT2);
	if (wantPhotonRcp) pointDataProcessorAllHT2.setPtEtaPhiCoord(histPointPtEtaPhiCoord);
*/
	TDataProcessorPool pool("pool", "Pool of data processors");
	pool.outputPrescale = 300*1000;
	
/*
	if (!simulation) {
		pool.processors.push_back(&candidateDataProcessorPeakMB);
		pool.processors.push_back(&candidateDataProcessorPeakHT1);
		pool.processors.push_back(&candidateDataProcessorPeakHT2);

//		pool.processors.push_back(&candidateDataProcessorHT1BadTrig);
//		pool.processors.push_back(&candidateDataProcessorHT2BadTrig);

//		pool.processors.push_back(&candidateDataProcessorMBSubmix);
//		pool.processors.push_back(&candidateDataProcessorHT1Submix);
//		pool.processors.push_back(&candidateDataProcessorMBShuffle);
//		pool.processors.push_back(&candidateDataProcessorHT1Shuffle);
//		pool.processors.push_back(&candidateDataProcessorHT2Shuffle);
	}

//	pool.processors.push_back(&candidateDataProcessorKinTrueMB);
//	pool.processors.push_back(&candidateDataProcessorKinTrueHT1);
//	pool.processors.push_back(&candidateDataProcessorKinTrueHT2);

//	pool.processors.push_back(&candidateDataProcessorNobadstripMB);
//	pool.processors.push_back(&candidateDataProcessorNobadstripHT1);
//	pool.processors.push_back(&candidateDataProcessorNobadstripHT2);

//	pool.processors.push_back(&candidateDataProcessorBadstripMB);
//	pool.processors.push_back(&candidateDataProcessorBadstripHT1);
//	pool.processors.push_back(&candidateDataProcessorBadstripHT2);

//	pool.processors.push_back(&candidateDataProcessorKinTrueNotMB);
//	pool.processors.push_back(&candidateDataProcessorKinTrueNotHT1);
//	pool.processors.push_back(&candidateDataProcessorKinTrueNotHT2);
*/
/*
	pool.processors.push_back(&eventDataProcessorNotvsMB);
	pool.processors.push_back(&eventDataProcessorNotvsHT1);
	pool.processors.push_back(&eventDataProcessorNotvsHT2);
*/
/*
	pool.processors.push_back(&eventDataProcessorAbortgapMB);
	pool.processors.push_back(&eventDataProcessorAbortgapHT1);
	pool.processors.push_back(&eventDataProcessorAbortgapHT2);
*/
/*
	pool.processors.push_back(&pointDataProcessorChargedMB);
	pool.processors.push_back(&pointDataProcessorChargedHT1);
	pool.processors.push_back(&pointDataProcessorChargedHT2);

	pool.processors.push_back(&pointDataProcessorAllMB);
	pool.processors.push_back(&pointDataProcessorAllHT1);
	pool.processors.push_back(&pointDataProcessorAllHT2);
*/
//	if (!simulation) {
/*
		pool.processors.push_back(&eventDataProcessorPSMB);
		pool.processors.push_back(&eventDataProcessorPSMBHT1);
		pool.processors.push_back(&eventDataProcessorPSMBHT2);
		pool.processors.push_back(&eventDataProcessorPSHT1);
		pool.processors.push_back(&eventDataProcessorPSHT1HT2);
*/
/*
		pool.processors.push_back(&eventDataProcessorHT1BadTrig);
		pool.processors.push_back(&eventDataProcessorHT2BadTrig);
*/
/*
		pool.processors.push_back(&eventDataProcessorAllValidZ);
		pool.processors.push_back(&eventDataProcessorAllValidZCorrupt);
		pool.processors.push_back(&eventDataProcessorAllValidZCorruptBad);
		pool.processors.push_back(&eventDataProcessorAllValidZCorruptBadMB);
*/
//	}

	if (true) {
	    pool.processors.push_back(&dataProcessorEventSummary);
	    pool.processors.push_back(&dataProcessorTriggerSummary);

	    if (useMB) pool.processors.push_back(&candidateDataProcessorMB);
	    if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1);
	    if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2);
	    
	    if (!simulation || simulation_pythia) {
		if (useMB) pool.processors.push_back(&candidateDataProcessorMBMix);
		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1Mix);
		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2Mix);

		if (useMB) pool.processors.push_back(&candidateDataProcessorMBJetmix);
		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1Jetmix);
		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2Jetmix);

//		if (useMB) pool.processors.push_back(&candidateDataProcessorMBJetmixNotmatched);
//		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1JetmixNotmatched);
//		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2JetmixNotmatched);

		if (useMB) pool.processors.push_back(&candidateDataProcessorMBJetmixBack);
		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1JetmixBack);
		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2JetmixBack);

//		if (useMB) pool.processors.push_back(&candidateDataProcessorMBShuffle);
//		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1Shuffle);
//		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2Shuffle);

		if (useMB) pool.processors.push_back(&candidateDataProcessorMBNocpv);
		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1Nocpv);
		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2Nocpv);

		if (useMB) pool.processors.push_back(&candidateDataProcessorMBNotcpv);
		if (useHT1) pool.processors.push_back(&candidateDataProcessorHT1Notcpv);
		if (useHT2) pool.processors.push_back(&candidateDataProcessorHT2Notcpv);
	    }
	    
	    if (useMB) pool.processors.push_back(&eventDataProcessorMB);
	    if (useHT1) pool.processors.push_back(&eventDataProcessorHT1);
	    if (useHT2) pool.processors.push_back(&eventDataProcessorHT2);

	    if (useMB) pool.processors.push_back(&pointDataProcessorMB);
	    if (useHT1) pool.processors.push_back(&pointDataProcessorHT1);
	    if (useHT2) pool.processors.push_back(&pointDataProcessorHT2);

	    if (useMB) pool.processors.push_back(&pointDataProcessorNocpvMB);
	    if (useHT1) pool.processors.push_back(&pointDataProcessorNocpvHT1);
	    if (useHT2) pool.processors.push_back(&pointDataProcessorNocpvHT2);

	    if (useMB) pool.processors.push_back(&pointDataProcessorNotcpvMB);
	    if (useHT1) pool.processors.push_back(&pointDataProcessorNotcpvHT1);
	    if (useHT2) pool.processors.push_back(&pointDataProcessorNotcpvHT2);

	    if (simulation) {
		if (includePi0Processor && useMB) pool.processors.push_back(&simuDataProcessorMB);
		if (includePi0Processor && useHT1) pool.processors.push_back(&simuDataProcessorHT1);
		if (includePi0Processor && useHT2) pool.processors.push_back(&simuDataProcessorHT2);
		//if (includePi0Processor && useMB) pool.processors.push_back(&simuDataProcessorAllValid);
		if (includeGammaProcessor && useMB) pool.processors.push_back(&mcGammaDataProcessorMB);
		if (includeGammaProcessor && useHT1) pool.processors.push_back(&mcGammaDataProcessorHT1);
		if (includeGammaProcessor && useHT2) pool.processors.push_back(&mcGammaDataProcessorHT2);
		//if (includeGammaProcessor && useMB) pool.processors.push_back(&mcGammaDataProcessorAllValid);
	    }
	    if (!simulation || simulation_pythia) {
		if (useMB) pool.processors.push_back(&eventDataProcessorPSMB);
		if (useMB) pool.processors.push_back(&eventDataProcessorPSMBHT1);
		if (useMB) pool.processors.push_back(&eventDataProcessorPSMBHT2);
		if (useHT1) pool.processors.push_back(&eventDataProcessorPSHT1);
		if (useHT1) pool.processors.push_back(&eventDataProcessorPSHT1HT2);

//		if (useMB) pool.processors.push_back(&eventDataProcessorAllValid);
	    }
	}

	{
	    TString filelistExact = findFile(filelist);	
    	    Info(__FILE__, "Processing file %s", filelistExact.Data());
	    pool.processFile(filelistExact);
	    Info(__FILE__, "Finished processing file %s", filelistExact.Data());
	}

        pool.settings.StPi0Common_Version_analysis = TMyDataAnalysisSettings::Class_Version();
        pool.settings.StPi0DataStructures_Version_analysis = TMyEventData::Class_Version();
        pool.settings.StPi0Analysis_Version_analysis = TEventDataProcessor::Class_Version();

	Info(__FILE__, "Writing output file %s", (outFile ? outFile : ""));
	pool.writeToFile(outFile);
	Info(__FILE__, "Finished writing output file %s", (outFile ? outFile : ""));
	
	TDatime stopTime;
	Info(__FILE__, "Finished: %s", stopTime.AsSQLString());
	timer.Stop();
	timer.Print();
	Info(__FILE__, "================== FINISHED ==================");
}
