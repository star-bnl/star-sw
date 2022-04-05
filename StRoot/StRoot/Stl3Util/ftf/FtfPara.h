#ifndef FTFPARA
#define FTFPARA
#include <stdio.h>
//:>------------------------------------------------------------------
//: FILE:       FtfPara.h
//: HISTORY:
//:             28oct1996 version 1.00
//:              7dec1998 ppy variable names changed to C++ style
//:              3jun1999 ppy add fillTracks flag
//:             11aug1999 ppy add vertexConstrainedFit variable
//:             23aug1999 ppy add ROOT option
//:             19nov1999 ppy add maxChi2Primary to decide whether track is primary
//:             11feb2000 ppy add maxTime
//:             28mar2000 ppy add parameterLocation variable
//:             30mar2000 ppy add parameters to merge secondaries
//:              8aug2000 ppy add zMax
//:
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfPara
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------

class FtfPara {          
 public:
  FtfPara ( ) { setDefaults() ; } ;
  void      setDefaults ( ) ;
  void      read  ( char* inputFile ) ;
  void      write ( char* outputFile ) ;
  void      write ( FILE* outFile ) ;

  int       infoLevel;       // Level of information printed about progress
  int       segmentRowSearchRange;       // Row search range for segments 
  int       trackRowSearchRange;         // Row search range for tracks 
  int       dEdx  ;          // dEdx switch
  int       dEdxNTruncate ;  // # points to truncate in dEdx
  int       minHitsForDedx;  // cs: min number of hits for dEdx calculation
  int       eventReset   ;   // Flag to reset event in fft 
  int       getErrors    ;   // Flag to switch error calculation
  int       fillTracks   ;   // Flag to switch FtfTrack class filling
  int       ghostFlag    ;   // =1 when there are ghost hits
  int       goBackwards  ;   // Flag to go backwards at the end of track reco
  int       init;            // Control initialization 
  int       mergePrimaries ; // Switch to control primary merging 
  int       minHitsPerTrack; // Minimum # hits per track 
  int       modRow;          // Modulo pad row number to use 
  int       nHitsForSegment; // # hits in initial segments 
  int       minHitsForFit;
  int       nEta;            // # volumes in eta 
  int       nEtaTrack;       // # Track areas in eta 
  int       nPhi;            // # volumes in nphi 
  int       nPhiTrack;       // # Track areas in nphi 
  int       nPrimaryPasses;  // # iterations looking for primaries
  int       nSecondaryPasses;// # iterations looking for secondaries
  int       vertexConstrainedFit; // 
  int       parameterLocation; // 1=inner most point, 0=closest approach
  float     maxChi2Primary ; // maximum chi2 to be considered primary 
  int       rowInnerMost;    // Row where end track search 
  int       rowOuterMost;    // Outer most row to consider tin tracking
  int       rowStart;        // Row where start track search
  int       rowEnd  ;        // Row where end   track search
  int       szFitFlag;       // Switch for sz fit 
  float     bField      ;    // Magnetic field  (magnitude = >0)
  int       bFieldPolarity;  // polarity of field (1 or -1)
  float     hitChi2Cut;      // Maximum hit chi2 
  float     goodHitChi2;     // Chi2 to stop looking for next hit 
  float     trackChi2Cut;    // Maximum track chi2 
  float     deta;            // Eta search range 
  float     dphi;            // Phi search range 
  float     detaMerge ;      // Eta difference for track merge 
  float     dphiMerge ;      // Phi difference for track merge
  float     distanceMerge ;  // Maximum distance for reference point to merge secondaries
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
  float     maxTime        ; // maxTime tracker can run
  int       phiClosed ;
  int       primaries  ;
  int       nRowsPlusOne, nPhiPlusOne   ; // Number volumes + 1
  int       nEtaPlusOne, nPhiEtaPlusOne ; // Number volumes + 1 
  int       nPhiTrackPlusOne, nEtaTrackPlusOne ;                
  float     phiSlice, etaSlice ;
  float     phiSliceTrack, etaSliceTrack ;
  float     zMax ;
#ifdef TRDEBUG
  int       trackDebug ;
  int       hitDebug ;
  int       debugLevel ;
#endif
} ;
#endif

