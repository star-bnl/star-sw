//:>------------------------------------------------------------------
//: FILE:       FtfPara.cxx
//: HISTORY:
//:             28oct1996 version 1.00
//:              7dec1998 ppy variable names changed to C++ style
//:              3jun1999 ppy add fillTracks flag
//:             11aug1999 ppy add vertexContrainedFit variable
//:             23aug1999 ppy add Root option
//:             11feb2000 ppy add maxTime 
//:                       
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfPara
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfFinder.h"

#ifdef SL3ROOT
ClassImp(FtfPara)
#endif


void FtfPara::setDefaults (void)
{
/*  Define cuts - this should be obsolete */

   modRow          = 1    ;
   infoLevel       = 0 ;
   hitChi2Cut      = 500.F  ;
   goodHitChi2     = 100.F ;
   trackChi2Cut    = 250.F ;
   maxChi2Primary  = 10. ;
   segmentRowSearchRange = 1 ;
   trackRowSearchRange   = 3 ;
   dEdx              = 0     ;
   dEdxNTruncate     = 20    ;
   dphi              = 0.10F * modRow ;
   deta              = 0.10F * modRow ;
   dphiMerge         = 0.02F  ;
   detaMerge         = 0.02F  ;
   etaMin            = -2.5F  ;
   etaMinTrack       = -2.2F  ;
   etaMax            =  2.5F  ;
   etaMaxTrack       =  2.2F  ;
   eventReset        =  1     ;
   getErrors         =  0     ;
   fillTracks        =  1     ;
   ghostFlag         =  0     ;
   goBackwards       =  0     ;
   goodDistance      =  1.F * modRow ;
   init              =  0 ;
   mergePrimaries    =  0    ;
   phiMin            =  (float)(-0.000001/toDeg)  ;
   phiMinTrack       =  (float)(-0.000001/toDeg)  ;
   phiMax            = (float)(360.2/toDeg)  ;
   phiMaxTrack       = (float)(360.2/toDeg)  ;
   maxDistanceSegment = 100.F * modRow ;
   minHitsPerTrack   = 5      ;
   nHitsForSegment   = 2      ;
   nEta              = 60     ;
   nEtaTrack         = 60     ;
   nPhi              = 20     ;
   nPhiTrack         = 60     ;
   nPrimaryPasses    = 1      ;
   nSecondaryPasses  = 0      ;
   vertexConstrainedFit = 0 ;
   rowInnerMost      = 1      ;
   rowOuterMost      = 45     ;
   rowStart          = 45     ;
   segmentMaxAngle   = 10.F/toDeg ;
   szFitFlag         = 1      ;
   xyErrorScale      = 1.0F   ;
   szErrorScale      = 1.0F   ;
   bField            = 0.5F   ;
   phiShift          = 0.0    ;
   
   ptMinHelixFit     = 100.F  ;
   rVertex           = 0.F    ;
   xVertex           = 0.F    ;
   yVertex           = 0.F    ;
   zVertex           = 0.F    ;
   dxVertex          = 0.005F ;
   dyVertex          = 0.005F ;
   phiVertex         = 0.F    ;
   maxTime           = 1.e18 ; // by default tracker can run as long as the age of the Universe

   return  ;
}
