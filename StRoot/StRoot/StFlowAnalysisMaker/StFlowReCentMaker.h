///////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowReCentMaker.h,v 1.2 2014/08/06 11:43:14 jeromel Exp $
//
// Authors: Art Poskanzer, Sep 2009
//
///////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to produce ReCent files
//
///////////////////////////////////////////////////////////////////////////////

#ifndef StFlowReCentMaker_H
#define StFlowReCentMaker_H
#include <Stiostream.h>
#include "StMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "TString.h"
class StFlowEvent;
class StFlowSelection;
class TFile;
class TProfile;

class StFlowReCentMaker : public StMaker {
  //  Makes histograms for ReCent and writes them out.
  //  It reads particle quantities from StFlowEvent.
 
public:

  /// Constructor
           StFlowReCentMaker(const Char_t* name="FlowReCent");

  virtual  ~StFlowReCentMaker();

  Int_t    Init();
  Int_t    Make();
  Int_t    Finish();

  virtual  const char *GetCVS() const {static const char cvs[]=
    "Tag $Name:  $ $Id: StFlowReCentMaker.h,v 1.2 2014/08/06 11:43:14 jeromel Exp $ built " __DATE__ " " __TIME__ ;
    return cvs;}

private:

  static const int nCens = 10;

  TFile* reCentFile[nCens];

  void FillEventHistograms();

  StFlowEvent*     pFlowEvent;  //! pointer to StFlowEvent
  StFlowSelection* pFlowSelect; //! selection object

  // for each harmonic, each selection, and each centrality
  struct histHars {
    TProfile*   mHistReCentX;
    TProfile*   mHistReCentY;
  };

  struct histCens;	
  friend struct histCens;
  struct histCens {
    struct histHars histHar[Flow::nHars];
  };

  struct hists;	
  friend struct hists;
  struct hists {
    struct histCens histCen[nCens];
  };
  struct hists hist[Flow::nSels]; //!

  TString      MakerName;

  ClassDef(StFlowReCentMaker,0)              // macro for rootcint
};

#endif

///////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowReCentMaker.h,v $
// Revision 1.2  2014/08/06 11:43:14  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2009/11/24 19:29:16  posk
// Added reCenter to remove acceptance correlations as an option instead of phiWgt.
//
//
//
/////////////////////////////////////////////////////////////////////////////////
