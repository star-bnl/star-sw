// common statics

#include "THit.hpp"
#include "TTracker.hpp"
#include "TMerger.hpp"
#ifdef LEDA
#include "_memory.hpp"
memory_manager std_memory_mgr;
#endif

double  TTrack::FSBField = 0.5F;

double  THit::FSErrorScaleXy = 5.0F;
double  THit::FSErrorScaleSz = 3.0F;
double  THit::FSPhiSliceMultiplier;
int     THit::FSNumberOfPhiSlices = 20;
double  THit::FSPhiMin = (-0.000001/To_deg);
double  THit::FSPhiMax = (360.000001/To_deg);
double  THit::FSEtaSliceMultiplier;
int     THit::FSNumberOfEtaSlices = 60;
double  THit::FSEtaMin = -2.2F;
double  THit::FSEtaMax = 2.2F;

int     TTrackerFTF::FSFitSz = 0;	// 1
int     TTrackerFTF::FSPhiClosed = 0;
double  TTrackerFTF::FSDPhiLimit = 0.1F;	// 0.3
double  TTrackerFTF::FSDEtaLimit = 0.1F;	// 0.3
double  TTrackerFTF::FSChi2Cut = 150.0F;	// 250
double  TTrackerFTF::FSGoodChi2 = 50.0F;
double  TTrackerFTF::FSGoodDistance = 1.0F;	// 2.0
double  TTrackerFTF::FSChi2TrackCut = 150.0F;	// 250
int     TTrackerFTF::FSMinimumHitsPerSegment = 3;
int     TTrackerFTF::FSMinimumHitsPerTrack = 5;
int     TTrackerFTF::FSMaxSearchPadrowsTrack = 4;	// 5
int     TTrackerFTF::FSMaxSearchPadrowsSegment = 2;	// 3
int     TTrackerFTF::FNPrimaryLoops = 1 ;
int     TTrackerFTF::FNSecondaryLoops = 0 ;
int     TTrackerFTF::FInnerMostRow = 1 ;
int     TTrackerFTF::FOuterMostRow = 45 ;
BOOL    TTrackerFTF::FMergePrimaries = 1;	// TRUE

WORD    TTrackerFTF::FSNumberOfEtaSlices = 60;
WORD    TTrackerFTF::FSNumberOfPhiSlices = 20;
//
// minimum and maximum of phi- and eta-slices
//
double  TTrackerFTF::FSPhiMin = (-0.000001/To_deg); 
double  TTrackerFTF::FSPhiMax = (360.000001/To_deg);
double  TTrackerFTF::FSEtaMin = -2.2F;
double  TTrackerFTF::FSEtaMax = 2.2F;
//
int     TMerger::NumberOfPsiSlices = 20;
int     TMerger::NumberOfTanLSlices = 20;
//
float   TMerger::MinSlicePsi = (-0.000001/To_deg);
float   TMerger::MaxSlicePsi = (360.000001/To_deg);
float   TMerger::MinSliceTanL = -1.0;
float   TMerger::MaxSliceTanL = 1.0;
float   TMerger::FSMinDistMerge = 1.0;	// 1
float   TMerger::FPrimaryVertexX = 0.0;
float   TMerger::FPrimaryVertexY = 0.0;
float   TMerger::FPrimaryVertexR = 0.0;
float   TMerger::FPrimaryVertexPhi = 0.0;
BOOL    TMerger::FSFitSz = FALSE;

