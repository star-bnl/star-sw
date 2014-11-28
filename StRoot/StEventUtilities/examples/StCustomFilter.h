// Author: Victor Perev   08/04/01
#ifndef ROOT_StCustomFilter
#define ROOT_StCustomFilter


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StCustomFilter                                                       //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StEventHelper.h"

class StEvent;
class StTrack;
class StarClassLibrary;
class BetheBloch;


//______________________________________________________________________________

class StCustomFilter : public StFilterABC { // An example of default filter
public:
   		StCustomFilter(const char *name,bool active=true);
               ~StCustomFilter();
virtual float        *GetPars() const {return (float*)(&fFirst+1);}
virtual const float  *GetDefs() const;
virtual const char  **GetNams() const;

protected:
  virtual Int_t  Accept(StPoints3DABC *pnt) {;}
  virtual Int_t  AcceptCB(StPoints3DABC *pnt, Color_t&, Size_t&, Style_t&);
private:
          Int_t Accept(const StTrack *track); // proxy for /StMuDSTMaker/COMMON/StMStMuL3Filter

protected:
  BetheBloch* mBB;

private:
  float fFirst;          // do not touch this data member

  // --
  // This filter custom data members go here:
  // --
    float fpCutHigh;       // high momentum cut for RICH/Upsilon candidates 
    float fnHitsCutHighP;  // nHits cut for all tracks

    // following cuts apply only for tracks with pCutLow < p <pHigh
    float fpCutLow;             // low momentum cut
    float fnHitsCutLowP;    
    float fchargeForLowP;       // charge for tracks with pCutLow < p < pCutHigh, set to 0 for all tracks
    float fdEdxMassCutHigh;     // cut below BetheBloch(p/dEdxMassCutHigh), e.g. proton-band
    float fdEdxFractionCutHigh; // cut fraction of dEdx-band, i.e. dEdxFractionCut * BetheBloch(p/dEdxMassCut)
    float fdEdxMassCutLow;      // cut above BetheBloch(p/dEdxMassCutLow), e.g. kaon-band
    float fdEdxFractionCutLow;
  // --  the last custom data-member  --

  float fLast;                // do not touch this data member

  ClassDef(StCustomFilter,0)
};

#endif //ROOT_StCustomFilter
