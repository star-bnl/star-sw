///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker.hh
// $Id: StFlowTagMaker.hh,v 1.9 2000/02/23 22:14:11 posk Exp $
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: 
//  Maker to fill Tag database for flow analysis
//
//
//////////////////////////////////////////////////////////////////////
//
// History:
// $Log: StFlowTagMaker.hh,v $
// Revision 1.9  2000/02/23 22:14:11  posk
// Renamed histograms to contain "Flow".
//
// Revision 1.8  2000/02/18 22:47:36  posk
// Minor updates.
//
// Revision 1.7  2000/01/14 05:44:35  snelling
// Added St_FlowTag Table to .data
//
// Revision 1.6  2000/01/14 01:36:03  snelling
// changed include path ../FlowMaker/ to FlowMaker/
//
// Revision 1.5  2000/01/13 21:49:16  posk
// Updates and corrections.
//
// Revision 1.4  1999/12/21 21:28:34  posk
// Updated the README file.
//
// Revision 1.3  1999/12/15 21:56:23  posk
// Increased number of harmonics from 4 to 6.
//
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

#ifndef StFlowTagMaker_HH
#define StFlowTagMaker_HH
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "tables/St_FlowTag_Table.h"
#include "StFlowMaker/StFlowConstants.hh"
class StFlowEvent;
class TH1F;
class TH1D;
class TProfile;

class StFlowTagMaker : public StMaker 
{

public:

               StFlowTagMaker(const Char_t* name = "FlowTag");
  virtual      ~StFlowTagMaker();

  Int_t        Init();
  Int_t        Make();
  void         PrintInfo();
  Int_t        Finish();
  FlowTag_st*  TagPointer() const;         // returns pointer to the tag table

private:

  void         FillFlowTag();
  Int_t        FillHistograms();
  void         PrintTag(ostream& = cout);  // output Tag info to screen

  St_FlowTag*  pSt_FlowTag; //! the StFlowTag table header
  FlowTag_st*  pFlowTag;    //! the StFlowTag table structure to fill
  StFlowEvent* pFlowEvent;  //! the event to fill from

  struct histHarmonic {
    TH1F *mHistPsi;
    TH1F *mHistMeanPt;
    TH1D *mHistMult;
    TH1F *mHist_q;
  };

  struct histSubEvent;
  friend struct histSubEvent;
  struct histSubEvent {
    struct histHarmonic histHarmonics[Flow::nHars];
  };

  struct histSubEvent histSubEvents[Flow::nSels+Flow::nSubs]; //!

  ClassDef(StFlowTagMaker, 1)                     // macro for rootcint
};

inline FlowTag_st* StFlowTagMaker::TagPointer() const { return pFlowTag; }

#endif
