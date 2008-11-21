// Default configuration to run pi0 reconstruction chain.
{
    // gDebug = 0;

    dataAnalysisSettings.isSimulation = false;
    dataAnalysisSettings.isEmbedding = false;
    dataAnalysisSettings.isPythia = false;
    dataAnalysisSettings.useLocalDB = false;
    dataAnalysisSettings.localDBpath = "~/StarDb";
    dataAnalysisSettings.timestampsFilename = "";
    dataAnalysisSettings.timeRandomNormEventsTotal = 0;
    dataAnalysisSettings.useSimCalibFlavour = false;
    dataAnalysisSettings.saveHits = false;
    dataAnalysisSettings.saveHitsPlain = false;
    dataAnalysisSettings.saveClusters = false;
    dataAnalysisSettings.saveClustersPlain = false;
    dataAnalysisSettings.savePoints = false;
    dataAnalysisSettings.savePointsPlain = true;
    dataAnalysisSettings.saveCandidates = false;
    dataAnalysisSettings.saveCandidatesPlain = true;
    dataAnalysisSettings.saveCandidatesMixed = false;
    dataAnalysisSettings.saveCandidatesMixedPlain = true;
    dataAnalysisSettings.saveCandidatesSubmixed = false;
    dataAnalysisSettings.saveCandidatesSubmixedPlain = false;
    dataAnalysisSettings.saveCandidatesWithoutSMD = true;
    dataAnalysisSettings.saveCandidatesWithoutSMDBoth = true;
    dataAnalysisSettings.saveMCGammas = false;
    dataAnalysisSettings.saveMCGammasPlain = false;
    dataAnalysisSettings.saveFirstMCGammaOnly = false;
    dataAnalysisSettings.saveMCPions = false;
    dataAnalysisSettings.saveMCPionsPlain = false;
    dataAnalysisSettings.saveFirstMCPionOnly = true;
    dataAnalysisSettings.saveMCEtas = false;
    dataAnalysisSettings.saveMCEtasPlain = false;
    dataAnalysisSettings.saveFirstMCEtaOnly = true;
    dataAnalysisSettings.saveMCNbars = false;
    dataAnalysisSettings.saveMCNbarsPlain = false;
    dataAnalysisSettings.saveFirstMCNbarOnly = true;
    dataAnalysisSettings.saveEvents = false;
    dataAnalysisSettings.saveEventsPlain = true;
    dataAnalysisSettings.saveSMDThreshold = false;
    dataAnalysisSettings.saveSMDThresholdPlain = false;
    dataAnalysisSettings.smd1Threshold = -1;
    dataAnalysisSettings.smd2Threshold = -1;
    dataAnalysisSettings.smd3Threshold = -1;
    dataAnalysisSettings.smdThresholdEnergy = false;
    dataAnalysisSettings.smdThresholdEt = true;
    dataAnalysisSettings.saveInterval = 1000;
    dataAnalysisSettings.clonesArraySize = 1000;
    dataAnalysisSettings.basketSize = 32000;//65536*4;
    dataAnalysisSettings.splitLevel = 99; // default
    dataAnalysisSettings.compressionLevel = 9;
    dataAnalysisSettings.HT1Threshold = 8;
    dataAnalysisSettings.HT2Threshold = 13;
    dataAnalysisSettings.TriggerAdc = 32;
    dataAnalysisSettings.useTriggerSimulatorOriginal = false;
    dataAnalysisSettings.useTriggerSimulatorEmbed = false;
    dataAnalysisSettings.MixedEventsNumber = 4;
    dataAnalysisSettings.MixingClassZ = true;
    dataAnalysisSettings.MixingClassZSize = 10.0; // 10 cm
    dataAnalysisSettings.MixingClassBemcMult = true;
    dataAnalysisSettings.MixingClassBemcMultSize = 2.0;
    dataAnalysisSettings.MixingClassTrigger = true;
    dataAnalysisSettings.MixingClassJetEta = true;
    dataAnalysisSettings.MixingClassJetEtaSize = 0.2;
    dataAnalysisSettings.MixingClassJetPhi = false;
    dataAnalysisSettings.MixingClassJetPhiSize = 0.2;
    dataAnalysisSettings.MixingClassJetET = false;
    dataAnalysisSettings.MixingClassJetETSize = 2.0; // 2 GeV
    dataAnalysisSettings.SubmixedEventsNumber = 4;
    dataAnalysisSettings.ShuffleSubmixEnergy = true;
    dataAnalysisSettings.ShuffleSubmixEta = true;
    dataAnalysisSettings.ShuffleSubmixPhi = true;
    dataAnalysisSettings.ClusterSizeMaxTower = 4;
    dataAnalysisSettings.ClusterEnSeedTower = 0.35;
    dataAnalysisSettings.ClusterEnAddTower = 0.035;
    dataAnalysisSettings.ClusterEnTotTower = 0.02;
    dataAnalysisSettings.ClusterCheckTower = kFALSE;
    dataAnalysisSettings.ClusterSizeMaxPreshower = 4;
    dataAnalysisSettings.ClusterEnSeedPreshower = 0.35;
    dataAnalysisSettings.ClusterEnAddPreshower = 0.035;
    dataAnalysisSettings.ClusterEnTotPreshower = 0.02;
    dataAnalysisSettings.ClusterCheckPreshower = kFALSE;
    dataAnalysisSettings.ClusterSizeMaxSMDE = 5;
    dataAnalysisSettings.ClusterEnSeedSMDE = 0.2;
    dataAnalysisSettings.ClusterEnAddSMDE = 0.0005;
    dataAnalysisSettings.ClusterEnTotSMDE = 0.1;
    dataAnalysisSettings.ClusterCheckSMDE = kFALSE;
    dataAnalysisSettings.ClusterSizeMaxSMDP = 5;
    dataAnalysisSettings.ClusterEnSeedSMDP = 0.2;
    dataAnalysisSettings.ClusterEnAddSMDP = 0.0005;
    dataAnalysisSettings.ClusterEnTotSMDP = 0.1;
    dataAnalysisSettings.ClusterCheckSMDP = kFALSE;
    dataAnalysisSettings.CalibSpreadTower = 0.0;
    dataAnalysisSettings.CalibOffsetTower = 0.0;
    dataAnalysisSettings.CalibSpreadPreshower = 0.0;
    dataAnalysisSettings.CalibOffsetPreshower = 0.0;
    dataAnalysisSettings.CalibSpreadSMDE = 0.0;
    dataAnalysisSettings.CalibOffsetSMDE = 0.0;
    dataAnalysisSettings.CalibSpreadSMDP = 0.0;
    dataAnalysisSettings.CalibOffsetSMDP = 0.0;
    dataAnalysisSettings.jetConeRadius = 0.4;
    dataAnalysisSettings.triggers[0] = 0;
    dataAnalysisSettings.triggersSim = 0;
    dataAnalysisSettings.adcToEMakerName = "Eread";
    dataAnalysisSettings.useFullJetMaker = true;
    dataAnalysisSettings.useSpinDbMaker = true;
    dataAnalysisSettings.jetMakerName = "emcJetMaker";
    dataAnalysisSettings.triggerFullSimulatorName = "FullTriggerSimulator";
    dataAnalysisSettings.triggerFullSimulatorNameEmbed = "FullTriggerSimulatorEmbed";
    dataAnalysisSettings.triggerFullSimulatorNameFinal = "FullTriggerSimulatorFinal";
    dataAnalysisSettings.jetFullMakerDoTowerSwapFix = true;
    dataAnalysisSettings.jetFullMakerUse2003Cuts = false;
    dataAnalysisSettings.jetFullMakerUse2005Cuts = false;
    dataAnalysisSettings.jetFullMakerUse2006Cuts = false;
    dataAnalysisSettings.jetFullMakerBranchName = "MidpointConeJet";
    dataAnalysisSettings.spinDbMakerName = "spinDb";
    dataAnalysisSettings.dataMakerName = "StPi0DataMaker";
    dataAnalysisSettings.datasetNameStEvent = "StEvent";
    dataAnalysisSettings.datasetNameStMcEvent = "StMcEvent";
    dataAnalysisSettings.doTowerSwapFix = true;
}
/*
Trigger IDs
---------------------
RUN III (2003)
dAu @ 200 GeV
    MinBias:     2001, 2003
    HighTower-1: 2201, (8, 2.5 GeV)
    HighTower-2: 2202, (13, 4.5 GeV)
pp @ 200 GeV
    MinBias:     1000
    HighTower-1: 1101, (8, 2.5 GeV)
    HighTower-2: 1102, (13, 4.5 GeV)
---------------------
RUN IV (2004)
AuAu @ 200 GeV
    MinBias:     15003, 15007
    HighTower-1: 15203, (13, 3 GeV / sin(theta))
pp @ 200 GeV
    pp-minbias: 45010
    bht-1-slow: 45201, (10, 2.2 GeV / sin(theta))
    bht-2-slow: 45202, (20, 4.6 GeV / sin(theta))
---------------------
RUN V (2005)
pp @ 200 GeV
    ppMinBias: 96011
    bemc-ht1-mb: 96201, (13, 2.6 GeV)
    bemc-ht2-mb: 96211, (17, 3.5 GeV or 20, 4.2 GeV)
    ppMinBias-tran: 106011
    bemc-ht1-mb-tran:106201, (13, 2.6 GeV)
    bemc-ht2-mb-tran: 106211, (17, 3.5 GeV)
---------------------
RUN 6 (2006)
pp @ 200 GeV
    mb: 117001
    bemc-http-mb: 117201 (HT 12, 2.6 GeV + TP 17, 3.8 GeV)
    bemc-ht2-mb: 117211 (22, 5 GeV)
    bemc-ht2-mb-emul: 117212 (22, 5 GeV)
    bemc-ht2-mb-emul: 127212 (West 22, 5 GeV and East 24, 5.4 GeV; transverse running)
    bemc-ht2-mb-emul: 127213 (24, 5.4 GeV; transverse running)
    bemc-ht2-mb-emul: 137213 (24, 5.4 GeV; longitudinal running 2)
---------------------
*/
