#ifndef StHFHists__h
#define StHFHists__h

/* **************************************************
 *  A class to create and save production QA
 *  histograms.
 *
 * **************************************************
 *
 *  Initial Authors:
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *          **Giacomo Contin  (gcontin@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

#include "TNamed.h"
#include "TList.h"
#include "StPicoPrescales/StPicoPrescales.h"

class TH1F;
class TH2F;
class TFile;
class TString;
class StPicoPrescales;

class StPicoEvent;
class StPicoHFEvent;

class StHFPair;
class StHFTriplet;


class StHFHists: public TNamed
{
 public:
  StHFHists();
  StHFHists(const char* name);

  virtual ~StHFHists();

  void init(TList *outList, unsigned int mode);
  void fillEventHists(StPicoEvent const &, StPicoHFEvent const &);
  //  void fillEventHists(StPicoEvent const &, StPicoHFEvent const &, unsigned int const nHftTracks);
  void fillGoodEventHists(StPicoEvent const &, StPicoHFEvent const &);
  void fillSecondaryPairHists(StHFPair const *, bool fillMass);
  void fillTertiaryPairHists(StHFPair const *, bool fillMass);
  void fillTripletHists(StHFTriplet const *, bool fillMass);

 private:
  
  TList *mEventList;
  TList *mSecondaryPairList;
  TList *mTertiaryPairList;
  TList *mTripletList;

  // general event hists
  StPicoPrescales* mPrescales;
 
  int mNRuns;

 
   ClassDef(StHFHists, 1)
};
#endif
