// common statics

#include "THit.hpp"
#include "TTracker.hpp"
#include "TMerger.hpp"
#include "_memory.hpp"

memory_manager std_memory_mgr;
double TTrack::FSBField = 0.5F;

double THit::FSErrorScaleXy = 5.0F;
double THit::FSErrorScaleSz = 3.0F;
double  THit::FSPhiSliceMultiplier;
int  THit::FSNumberOfPhiSlices = 20;
double  THit::FSPhiMin = (-0.000001/To_deg);
double  THit::FSPhiMax = (360.000001/To_deg);
double  THit::FSEtaSliceMultiplier;
int  THit::FSNumberOfEtaSlices = 60;
double  THit::FSEtaMin = -2.2F;
double  THit::FSEtaMax = 2.2F;

int  TTrackerFFT::FSFitSz = 0;	// 1
int  TTrackerFFT::FSPhiClosed = 0;
double  TTrackerFFT::FSDPhiLimit = 0.1F;	// 0.3
double  TTrackerFFT::FSDEtaLimit = 0.1F;	// 0.3
double  TTrackerFFT::FSChi2Cut = 150.0F;	// 250
double  TTrackerFFT::FSGoodChi2 = 50.0F;
double  TTrackerFFT::FSGoodDistance = 1.0F;	// 2.0
double  TTrackerFFT::FSChi2TrackCut = 150.0F;	// 250
int  TTrackerFFT::FSMinimumHitsPerSegment = 3;
int  TTrackerFFT::FSMinimumHitsPerTrack = 5;
int  TTrackerFFT::FSMaxSearchPadrowsTrack = 4;	// 5
int  TTrackerFFT::FSMaxSearchPadrowsSegment = 2;	// 3
BOOL  TTrackerFFT::FMergePrimaries = 1;	// TRUE

WORD TTrackerFFT::FSNumberOfEtaSlices = 60;
WORD TTrackerFFT::FSNumberOfPhiSlices = 20;
	// minimum and maximum of phi- and eta-slices
double TTrackerFFT::FSPhiMin = (-0.000001/To_deg); 
double TTrackerFFT::FSPhiMax = (360.000001/To_deg);
double TTrackerFFT::FSEtaMin = -2.2F;
double TTrackerFFT::FSEtaMax = 2.2F;

int TMerger::NumberOfPsiSlices = 20;
int TMerger::NumberOfTanLSlices = 20;

float TMerger::MinSlicePsi = (-0.000001/To_deg);
float TMerger::MaxSlicePsi = (360.000001/To_deg);
float TMerger::MinSliceTanL = -1.0;
float TMerger::MaxSliceTanL = 1.0;
float  TMerger::FSMinDistMerge = 1.0;	// 1
float TMerger::FPrimaryVertexX = 0.0;
float TMerger::FPrimaryVertexY = 0.0;
float TMerger::FPrimaryVertexR = 0.0;
float TMerger::FPrimaryVertexPhi = 0.0;
BOOL TMerger::FSFitSz = FALSE;

