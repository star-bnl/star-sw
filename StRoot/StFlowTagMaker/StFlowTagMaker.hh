#ifndef StFlowTagMaker_HH
#define StFlowTagMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker.hh
// $Id: StFlowTagMaker.hh,v 1.2 1999/12/04 00:13:36 posk Exp $
//
// Description: 
//  Maker to fill Tag database for flow analysis
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
//////////////////////////////////////////////////////////////////////
//
// History:
// $Log: StFlowTagMaker.hh,v $
// Revision 1.2  1999/12/04 00:13:36  posk
// Works with StFlowEvent which works with the new StEvent
//
// Revision 1.1  1999/11/11 23:13:02  posk
// Rearrangement of files.
//
// Revision 1.5  1999/09/24 01:23:07  fisyak
// Reduced Include Path
//
// Revision 1.4  1999/08/17 21:47:42  fisyak
// iostream => iostream.h for HP
//
// Revision 1.3  1999/08/09 21:43:06  snelling
// removed parameters from cxx file
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "../StFlowMaker/StFlowEvent.hh"
#include "FlowTag.h"
class TH1F;
class TH1D;
class TProfile;

class StFlowTagMaker : public StMaker 
{

public:

               StFlowTagMaker(const Char_t *name="FlowTag");
  virtual      ~StFlowTagMaker();
  Int_t        Init();
  Int_t        Make();
  void         PrintInfo();
  Int_t        Finish();
  FlowTag_st*  TagPointer() const;        // returns pointer to the tag table

private:

  void         fillFlowTag();
  Int_t        fillHistograms();
  void         printTag(ostream& = cout);  // output Tag info to screen

  FlowTag_st*  pFlowTag;    //! the tag table to fill
  StFlowEvent* pFlowEvent;  //! the event to fill from

  enum {nHars = 4, nSels = 2, nSubs = 2};

  // limits of histograms
  enum {nPsiBins = 100, nMeanPtBins = 100, nMultBins = 100, n_qBins = 100}; 

  struct histHarmonic {
    TH1F *mHistPsi;
    TH1F *mHistMeanPt;
    TH1D *mHistMult;
    TH1F *mHist_q;
  };

  struct histSubEvent;
  friend struct histSubEvent;
  struct histSubEvent {
    struct histHarmonic histHarmonics[nHars];
  };

  struct histSubEvent histSubEvents[nSels+nSubs]; //!

  ClassDef(StFlowTagMaker, 1)                     // macro for rootcint
};

inline FlowTag_st* StFlowTagMaker::TagPointer() const {return pFlowTag;}

#endif









