///////////////////////////////////////////////////////////////////////////////
//
// StFlowTagMaker.hh
// $Id: StFlowTagMaker.h,v 1.12 2000/05/20 00:57:03 posk Exp $
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
// $Log: StFlowTagMaker.h,v $
// Revision 1.12  2000/05/20 00:57:03  posk
// Minor update.
//
// Revision 1.11  2000/05/12 22:39:28  snelling
// Fixed warning
//
// Revision 1.10  2000/03/28 23:23:26  posk
// Allow multiple instances of the AnalysisMaker.
//
// Revision 1.9  2000/03/21 00:23:00  posk
// Added GetCVS.
//
// Revision 1.8  2000/03/15 23:30:56  posk
// Added StFlowSelection.
//
// Revision 1.7  2000/03/02 23:00:09  posk
// Changed header file extensions from .hh to .h .
//
// Revision 1.9  2000/02/23 22:14:11  posk
// Renamed histograms to contain "Flow".
//
// Revision 1.7  2000/01/14 05:44:35  snelling
// Added St_FlowTag Table to .data
//
// Revision 1.6  2000/01/14 01:36:03  snelling
// changed include path ../FlowMaker/ to FlowMaker/
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

#ifndef StFlowTagMaker_H
#define StFlowTagMaker_H
#include <iostream.h>
#include "StMaker.h"
#include "tables/St_FlowTag_Table.h"
#include "StFlowMaker/StFlowConstants.h"
class StFlowEvent;
class StFlowSelection;
class TH1F;
class TH1D;
class TProfile;

class StFlowTagMaker : public StMaker 
{

public:

               StFlowTagMaker(const Char_t* name="FlowTag");
  virtual      ~StFlowTagMaker();

  Int_t        Init();
  Int_t        Make();
  void         PrintInfo();
  Int_t        Finish();
  FlowTag_st*  TagPointer() const;         // returns pointer to the tag table
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StFlowTagMaker.h,v 1.12 2000/05/20 00:57:03 posk Exp $ built "__DATE__" "__TIME__ ;
    return cvs;}

private:

  void         FillFlowTag();
  Int_t        FillHistograms();
  void         PrintTag(ostream& = cout);  // output Tag info to screen

  St_FlowTag*      pSt_FlowTag; //! the StFlowTag table header
  FlowTag_st*      pFlowTag;    //! the StFlowTag table structure to fill
  StFlowEvent*     pFlowEvent;  //! the event to fill from
  StFlowSelection* pFlowSelect; //! the selection object

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
