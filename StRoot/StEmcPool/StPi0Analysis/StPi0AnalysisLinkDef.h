#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#ifdef __CINT__

#pragma link C++ class TBinParameters+;
#pragma link C++ class TBinStatistics+;

#pragma link C++ class list<TBinStatistics>+;
//#pragma link C++ class list<TBinStatistics>::iterator+;
//#pragma link C++ class list<TBinStatistics>::const_iterator+;
//#pragma link C++ class list<TBinStatistics>::reverse_iterator+;
//#pragma link C++ class list<TBinStatistics>::const_reverse_iterator+;

#pragma link C++ class TCutParameters+;
#pragma link C++ class TCuts+;
#pragma link C++ class TDataProcessor+;
#pragma link C++ class list<TDataProcessor*>+;
//#pragma link C++ class list<TDataProcessor*>::iterator+;
//#pragma link C++ class list<TDataProcessor*>::const_iterator+;
//#pragma link C++ class list<TDataProcessor*>::reverse_iterator+;
//#pragma link C++ class list<TDataProcessor*>::const_reverse_iterator+;

#pragma link C++ class TDataProcessorPool+;
#pragma link C++ global poolsList;
#pragma link C++ function removePool;

#pragma link C++ class TEventDataProcessor+;
#pragma link C++ class TInvariantMassDistribution+;

#pragma link C++ class list<TInvariantMassDistribution>+;
//#pragma link C++ class list<TInvariantMassDistribution>::iterator+;
//#pragma link C++ class list<TInvariantMassDistribution>::const_iterator+;
//#pragma link C++ class list<TInvariantMassDistribution>::reverse_iterator+;
//#pragma link C++ class list<TInvariantMassDistribution>::const_reverse_iterator+;

#pragma link C++ class TCandidateDataProcessor+;
#pragma link C++ class TPointDataProcessor+;
#pragma link C++ class TClusterDataProcessor+;
#pragma link C++ class THitDataProcessor+;
#pragma link C++ class TMCGammaDataProcessor+;
#pragma link C++ class TSimuDataProcessor+;
#pragma link C++ class TSMDThresholdDataProcessor+;
#pragma link C++ class TWeightCalculator+;

#pragma link C++ function isBadRun_default;
#pragma link C++ function isBadRun_embeddingonly;
#pragma link C++ function isBadRun_normal;
#pragma link C++ function isBadRun_strict;
#pragma link C++ function isBadRun_verystrict;
#pragma link C++ function isBadRun_beambg;
#pragma link C++ function isBadRun_bunchCrossingId7bit;
#pragma link C++ function isGoodFtpcRun;
#pragma link C++ function isBadEvent;
#pragma link C++ class event_list_type+;
#pragma link C++ function readEventListFromFile;
#pragma link C++ function writeEventListToFile;

#pragma link C++ function initCuts;
#pragma link C++ function getCutName;
#pragma link C++ function getCutTitle;
#pragma link C++ function printCutNames;

#pragma link C++ global useBinDenomInterpolation;
#pragma link C++ global pQCDweight;
#pragma link C++ function getNLOpQCD;
#pragma link C++ global pQCDPPweight;
#pragma link C++ global pQCDPPKweight;
#pragma link C++ function getNLOpQCDPP;
#pragma link C++ global pQCDPPweight_2;
#pragma link C++ function getNLOpQCDPP_2;
#pragma link C++ function getBunchCrossingId7bitOffset;

#pragma link C++ class StPi0AnalysisUtil+;
#pragma link C++ class StPi0AnalysisUtilDB+;

// from UtilDB.h
#pragma link C++ global triggerIDsMB;
#pragma link C++ global triggerIDsHT1;
#pragma link C++ global triggerIDsHT2;
#pragma link C++ global DBConnectString;
#pragma link C++ function getPrescalesFromDB; 
#pragma link C++ function getRunTimesFromDB; 
#pragma link C++ function getEventNumbersFromDB; 

#endif
