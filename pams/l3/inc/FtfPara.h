#ifndef FTFPARA
#define FTFPARA
//:>------------------------------------------------------------------
//: FILE:       FtfPara.h
//: HISTORY:
//:             28oct1996 version 1.00
//:              7dec1998 ppy variable names changed to C++ style
//:              3jun1999 ppy add fillTracks flag
//:             11aug1999 ppy add vertexConstrainedFit variable
//:             23aug1999 ppy add ROOT option
//:
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfPara
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif

//
//           fft control parameters                          
//
   class FtfPara {          
     public:
       FtfPara ( ) { setDefaults() ; } ;
       void      setDefaults ( ) ;
       short     infoLevel;       // Level of information printed about progress
       short     segmentRowSearchRange;       // Row search range for segments 
       short     trackRowSearchRange;         // Row search range for tracks 
       short     dEdx  ;          // dEdx switch
       short     dEdxNTruncate ;  // # points to truncate in dEdx
       short     eventReset   ;   // Flag to reset event in fft 
       short     getErrors    ;   // Flag to switch error calculation
       short     fillTracks   ;   // Flag to switch FtfTrack class filling
       short     ghostFlag    ;   // =1 when there are ghost hits
       short     goBackwards  ;   // Flag to go backwards at the end of track reco
       short     init;            // Control initialization 
       short     mergePrimaries ; // Switch to control primary merging 
       short     minHitsPerTrack; // Minimum # hits per track 
       short     modRow;          // Modulo pad row number to use 
       short     nHitsForSegment; // # hits in initial segments 
       short     minHitsForFit;
       short     nEta;            // # volumes in eta 
       short     nEtaTrack;       // # Track areas in eta 
       short     nPhi;            // # volumes in nphi 
       short     nPhiTrack;       // # Track areas in nphi 
       short     nPrimaryPasses;  // # iterations looking for primaries
       short     nSecondaryPasses;// # iterations looking for secondaries
       short     vertexConstrainedFit; // 
       short     rowInnerMost;    // Row where end track search 
       short     rowOuterMost;    // Outer most row to consider tin tracking
       short     rowStart;        // Row where start track search
       short     szFitFlag;       // Switch for sz fit 
       float     bField      ;    // Magnetic field  
       float     hitChi2Cut;      // Maximum hit chi2 
       float     goodHitChi2;     // Chi2 to stop looking for next hit 
       float     trackChi2Cut;    // Maximum track chi2 
       float     deta;            // Eta search range 
       float     dphi;            // Phi search range 
       float     detaMerge ;      // Eta difference for track merge 
       float     dphiMerge ;      // Phi difference for track merge
       float     etaMin;          // Min eta to consider 
       float     etaMinTrack ;    // Track min eta to consider 
       float     etaMax;          // Max eta to consider 
       float     etaMaxTrack ;    // Track max eta to consider 
       float     goodDistance ;   // In segment building
				  // distance consider good enough 
       float     phiMin;          // Min phi to consider 
       float     phiMinTrack ;    // Track min phi to consider 
       float     phiMax;          // Max phi to consider 
       float     phiMaxTrack ;    // Track max phi to consider 
       float     phiShift      ;  // Shift in phi when calculating phi
       float     ptMinHelixFit ;  // Minimum pt to apply helix fit
       float     maxDistanceSegment; // Maximum distance for segments 
       float     segmentMaxAngle; // Maximum angle between to consecutive track pieces 
	                          // when forming segments. A piece is the connection 
	                          // two hits
       float     szErrorScale;    // sz error scale 
       float     xyErrorScale;    // xy error scale 
       float     xVertex      ;   // x position primary vertex 
       float     yVertex      ;   // y position primary vertex 
       float     dxVertex     ;
       float     dyVertex     ;
       float     zVertex      ;
       float     xyWeightVertex;  // Weight vertex in x-y
       float     phiVertex      ;
       float     rVertex        ;
       short     phiClosed ;
       short     primaries  ;
       int       nRowsPlusOne, nPhiPlusOne   ; // Number volumes + 1
       int       nEtaPlusOne, nPhiEtaPlusOne ; // Number volumes + 1 
       int       nPhiTrackPlusOne, nEtaTrackPlusOne ;                
       float     phiSlice, etaSlice ;
       float     phiSliceTrack, etaSliceTrack ;
#ifdef TRDEBUG
       int       trackDebug ;
       int       hitDebug ;
       int       debugLevel ;
#endif
   ClassDef(FtfPara,1)
   } ;
#endif

