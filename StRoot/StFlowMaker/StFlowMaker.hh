#ifndef StFlowMaker_HH
#define StFlowMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowMaker.hh
//  $Id: StFlowMaker.hh,v 1.4 1999/11/30 18:52:54 snelling Exp $
//
// Description: 
//  Interface to StEvent for StFlowEvent and base class for
//    StFlowTagMaker and StFlowAnalysisMaker
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
// History:
//  $Log: StFlowMaker.hh,v $
//  Revision 1.4  1999/11/30 18:52:54  snelling
//  First modification for the new StEvent
//
//  Revision 1.3  1999/11/24 18:17:15  posk
//  Put the methods which act on the data in with the data in StFlowEvent.
//
//  Revision 1.2  1999/11/11 23:08:58  posk
//  Rearrangement of files.
//
//  Revision 1.1  1999/11/04 19:02:14  snelling
//  First check in of StFlowMaker. It contains the common code from
//  StFlowTagMaker and StFlowAnalysisMaker.
//
//
///////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "TString.h"
class StFlowEvent;
class StEvent;

class StFlowMaker : public StMaker {

public:

          StFlowMaker(const Char_t *name="Flow");
  virtual ~StFlowMaker();

  Int_t   Init();
  void    PrintInfo();
  Int_t   Make();
  Int_t   Finish();

  StFlowEvent* FlowEventPointer() const;  // returns pointer to the StFlowEvent

protected:

  // C++ way to define constants in the header
  enum {nHars = 4, nSels=2, nSubs = 2};  /// remove
  enum {nPhiBins = 60};                     /// remove
  /// StFlowEvent::PhiWgt_t mPhiWgt;        /// instead of next line
  Double_t mPhiWgt[nSels][nHars][nPhiBins]; // To make event plane isotropic

  Int_t        readPhiWgtFile();

  StEvent*     pEvent;                 //! pointer to DST data
  StFlowEvent* pFlowEvent;             //! pointer to micro-DST data
  TString      MakerName;

private:

  StFlowEvent*  fillFlowEvent();

  ClassDef(StFlowMaker, 1)           // macro for rootcint

};

inline StFlowEvent* StFlowMaker::FlowEventPointer() const {return pFlowEvent;}

#endif
