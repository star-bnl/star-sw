#include <stdio.h>
#include <stdlib.h>
#include "StCustomFilter.h"

#include "StEvent.h"
#include "StHit.h"
#include "StTrack.h"
#include "StVertex.h"
#include "StTrackGeometry.h"
#include "StTrackDetectorInfo.h"
// For L3 filter
#include "StarClassLibrary/BetheBloch.h"
#include "StEvent/StDedxPidTraits.h"


//______________________________________________________________________________
ClassImp(StCustomFilter)
StCustomFilter::StCustomFilter(const char *name,bool active):StFilterABC(name,active)
{
   // Assing a custom data-member
  mBB = new BetheBloch();
  // Set the default values for the parameters.
  // Don't change this line !!!
  SetDefs();
  
}
//______________________________________________________________________________
StCustomFilter::~StCustomFilter()
{ // delete the custom data-member
   delete mBB;
}
//______________________________________________________________________________
const char  **StCustomFilter::GetNams() const
{
  //  Create a "Label" list to be used with the dialog box
  static const char *nams[] = {
    " pCutHigh            ",  
    " nHitsCutHighP       ",
    " pCutLow             ",
    " nHitsCutLowP        ",    
    " chargeForLowP       ",
    " dEdxMassCutHigh     ",
    " dEdxFractionCutHigh ",
    " dEdxMassCutLow      ",
    " dEdxFractionCutLow  ",
    0
  };
  return nams;
}
//______________________________________________________________________________
const float  *StCustomFilter::GetDefs() const
{
  //  Create a list of the "default values"
  // The order of the default values must match the "label" list (see: StCustomFilter::GetNams)
  static const float defs[] = {
    /* pCutHigh            */ 2.0,    // high momentum cut for RICH/Upsilon candidates 
    /* nHitsCutHighP       */ 10,     // nHits cut for all tracks
    /* pCutLow             */ 0.2,    // low momentum cut
    /* nHitsCutLowP        */ 15,    
    /* chargeForLowP       */ -1,     // charge for tracks with pCutLow < p < pCutHigh, set to 0 for all tracks
    /* dEdxMassCutHigh     */ 0.939,  // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
    /* dEdxFractionCutHigh */ 0.6,    // cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
    /* dEdxMassCutLow      */ 0.494,  // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
    /* dEdxFractionCutLow  */ 1.1,
   //-------------------------------------------------------------------------------------------
    0                                 // the last valuse MUST be zero and it MUST be present
   //-------------------------------------------------------------------------------------------
  };
  return defs;
}

//______________________________________________________________________________
Int_t StCustomFilter::Accept(StPoints3DABC *pnt, Color_t&color, Size_t&size, Style_t&style)
{
   // ---
   // "Accept" method is called by the  StEventDisplayMaker event loop 
   // once for each component of the StEvent.
   //
   // To see all possible combinations  check:
   //   StRoot/StEventUtilities/StEventHelper.cxx:StFilterDef::Accept()
   // method
   //
   // Input:    pnt - the interface to the object 3D coordinate
   //         color - the default color
   //         size  - the default size  (it if the line width for trach and the dot diameter for the hits)
   //         style - the default style (it is the line style or ROOT marker style)
   //
   // Output  color - the custom color if any
   //         size  - the custom size  if any
   //         style - the custom style if any
   //
   // return :    0 the object is rejected and it will not be drawn
   //            +n the object is accepted and will be passed to next filter if any
   //                optionally you may change the object visual attribute 
   //                like color, style and size
   //             If you have no intention to change the default values 
   //             you may simply ignore the parameters. 
   //             The attributes set by the filter do overwrite what other filter may have set
   //
   //             If the object has passed the last filter in row it will be drawn.
   // ---
   TObject *to;
   StTrack *trk;
   to = pnt->GetObject();
   if (!to) 						return 1;
   // We want to provide our own custom cut for the StTrack object only.
   if (!to->InheritsFrom(StTrack::Class()))		return 1;

   // The track component of StEvent was found let's apply our custom cut.
   trk = (StTrack*)to;
   return Accept(trk,color,size,style);
}
//______________________________________________________________________________
Int_t StCustomFilter::Accept(const StTrack* track,Color_t&color, Size_t&size, Style_t&style) 
{
   //
   // User provided the selection and cutom visual attribute fot the track
   // It is assumed this function needs the value assigned through interactive dialog
   //
   // If you don't want to change the default visual attribute you may simply ignore the input parameters
   // This concrete cut implementation was borrowed from StRoot/StMuDSTMaker/COMMON/StMStMuL3Filter
   //-----



   //-----
   // Note: The statements below make a copy of the object data-mambers to the local variables
   //-----
   //    There is no need to assign the local variable.
   //    We do that to highlight the difference between the original "StMStMuL3Filter"
   //-----

   float pCutHigh        = fpCutHigh;               // high momentum cut for RICH/Upsilon candidates 
   int   nHitsCutHighP   = int(fnHitsCutHighP);     // nHits cut for all tracks

   // following cuts apply only for tracks with pCutLow < p <pHigh
   float pCutLow             = fpCutLow;            // low momentum cut
   int   nHitsCutLowP        = int(fnHitsCutLowP);    
   int   chargeForLowP       = int(fchargeForLowP); // charge for tracks with pCutLow < p < fpCutHigh, set to 0 for all tracks
   float dEdxMassCutHigh     = fdEdxMassCutHigh;    // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
   float dEdxFractionCutHigh = fdEdxFractionCutHigh;// cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
   float dEdxMassCutLow      = fdEdxMassCutLow;     // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
   float dEdxFractionCutLow  = fdEdxFractionCutLow;
   // --  The copy of the all data-members have been made

   int iret = 0;
   int chargeOK = 0;
   int dedxOK = 0;

   float magnitude = track->geometry()->momentum().magnitude();
   int   nPoints   = track->detectorInfo()->numberOfPoints();

   if  (   magnitude > pCutHigh && nPoints >= nHitsCutHighP)   iret = 1;
   else {
      if ( magnitude > pCutLow  && nPoints >= nHitsCutLowP ) 
      {
         // check charge
         if (chargeForLowP==0) 
            chargeOK = 1;
         else if (track->geometry()->charge() == chargeForLowP) 
            chargeOK = 1;

         // check dEdx
         //	      if (mBB==0) mBB = new BetheBloch();
         float dedxHigh = dEdxFractionCutHigh * mBB->Sirrf(magnitude/dEdxMassCutHigh);
         float dedxLow  = dEdxFractionCutLow  * mBB->Sirrf(magnitude/dEdxMassCutLow);
         float dedx     = 0;

         // get track dEdx
         const StSPtrVecTrackPidTraits& traits = track->pidTraits();
         StDedxPidTraits* dedxPidTr;
         for (unsigned int itrait = 0; itrait < traits.size(); itrait++){
            dedxPidTr = 0;
            if (traits[itrait]->detector() == kTpcId) {
               StTrackPidTraits* thisTrait = traits[itrait];
               dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
               if (dedxPidTr && dedxPidTr->method() == kTruncatedMeanId) {
                  // adjust L3 dE/dx by a factor of 2 to match offline
                  dedx = 2 * dedxPidTr->mean();
               }
            }
         }
         if (dedx > dedxHigh && dedx > dedxLow) 
            dedxOK = 1;
         // final answer
         iret = chargeOK * dedxOK;
      } // if (pCutLow && nHitsCutLowP)
   }
   return iret;
}
