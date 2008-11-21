#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#ifdef __CINT__

// common

#pragma link C++ class TMyDataAnalysisSettings+;
#pragma link C++ global EMCRadius;
#pragma link C++ global truePionMass;
#pragma link C++ global truePionBranchingRatio;
#pragma link C++ global trueEtaMass;
#pragma link C++ global trueEtaBranchingRatio;
#pragma link C++ global trueOmegaMass;
#pragma link C++ function findFile;
#pragma link C++ function floatCompare;
#pragma link C++ function getPseudorapidity;
#pragma link C++ function getRapidity;
#pragma link C++ function setHistFontSize;
#pragma link C++ function setLegendFontSize;
#pragma link C++ function setLatexFontSize;

#pragma link C++ class StPi0CommonUtil+;

// data structures

#pragma link C++ class CL+; 
#pragma link C++ class StPi0DataStructures+; 

#pragma link C++ class TMyTriggerData+; 
#pragma link C++ class TMyTriggerSimulatedData+; 
#pragma link C++ class TMySimulatedParticleSummaryData+; 
#pragma link C++ class TMySimulatedParticleData+; 
#pragma link C++ class TMySimulatedDecayData+; 
#pragma link C++ class TMyJetData+; 
#pragma link C++ class TMyEventData+; 
#pragma link C++ class TMyHitData+; 
#pragma link C++ class TMyClusterData+; 
#pragma link C++ class TMyPointData+; 
#pragma link C++ class TMyCandidateData+; 
#pragma link C++ class TMySMDThresholdData+; 

#pragma link C++ class TMyEventTreeData+; 
#pragma link C++ class TMyMCParticleTreeData+; 
#pragma link C++ class TMyMCDecayTreeData+; 
#pragma link C++ class TMyPointTreeData+; 
#pragma link C++ class TMySMDThresholdTreeData+; 
#pragma link C++ class TMyCandidateTreeData+; 
#pragma link C++ class TMyClusterTreeData+; 
#pragma link C++ class TMyHitTreeData+; 

#pragma link C++ function getBranchType; 
#pragma link C++ function createTree; 
#pragma link C++ function createH1F; 

#pragma link C++ global mcGammaTreeName; 
#pragma link C++ global mcGammaTreePlainName; 
#pragma link C++ global mcGammaBranchName; 
#pragma link C++ global mcGammaDatasetName; 

#pragma link C++ global mcPionTreeName; 
#pragma link C++ global mcPionTreePlainName; 
#pragma link C++ global mcPionBranchName; 
#pragma link C++ global mcPionDatasetName; 

#pragma link C++ global mcEtaTreeName; 
#pragma link C++ global mcEtaTreePlainName; 
#pragma link C++ global mcEtaBranchName; 
#pragma link C++ global mcEtaDatasetName; 

#pragma link C++ global mcNbarTreeName; 
#pragma link C++ global mcNbarTreePlainName; 
#pragma link C++ global mcNbarBranchName; 
#pragma link C++ global mcNbarDatasetName; 

#pragma link C++ global candidateTreeName; 
#pragma link C++ global candidateTreePlainName; 
#pragma link C++ global candidateBranchName; 
#pragma link C++ global candidateDatasetName; 

#pragma link C++ global candidateTreeMixName; 
#pragma link C++ global candidateTreeMixPlainName; 
#pragma link C++ global candidateMixDatasetName; 

#pragma link C++ global candidateTreeSubmixName; 
#pragma link C++ global candidateTreeSubmixPlainName; 
#pragma link C++ global candidateSubmixDatasetName; 

#pragma link C++ global eventTreeName; 
#pragma link C++ global eventTreePlainName; 
#pragma link C++ global eventBranchName; 
#pragma link C++ global eventDatasetName; 

#pragma link C++ global pointTreeName; 
#pragma link C++ global pointTreePlainName; 
#pragma link C++ global pointBranchName; 
#pragma link C++ global pointDatasetName; 

#pragma link C++ global clusterTreeName; 
#pragma link C++ global clusterTreePlainName; 
#pragma link C++ global clusterBranchName; 
#pragma link C++ global clusterDatasetName; 

#pragma link C++ global hitTreeName; 
#pragma link C++ global hitTreePlainName; 
#pragma link C++ global hitBranchName; 
#pragma link C++ global hitDatasetName; 

#pragma link C++ global smdThresholdTreeName; 
#pragma link C++ global smdThresholdTreePlainName; 
#pragma link C++ global smdThresholdBranchName; 
#pragma link C++ global smdThresholdDatasetName; 

#pragma link C++ global triggerSummaryName; 
#pragma link C++ global triggerSummaryNbins; 
#pragma link C++ global triggerSummaryMin; 
#pragma link C++ global triggerSummaryMax; 

#pragma link C++ global eventSummaryName; 
#pragma link C++ global eventSummaryNbins; 
#pragma link C++ global eventSummaryMin; 
#pragma link C++ global eventSummaryMax; 

#endif

