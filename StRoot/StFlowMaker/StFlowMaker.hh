#ifndef StFlowMaker_HH
#define StFlowMaker_HH

///////////////////////////////////////////////////////////////////////////////
//
// StFlowMaker.h
//  $Id: StFlowMaker.hh,v 1.1 1999/11/04 19:02:14 snelling Exp $
//
// Description: 
//  Interface to StEvent for FlowEvent
//
// Environment:
//  Software developed for the STAR Detector at LBNL
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
// History:
//  $Log: StFlowMaker.hh,v $
//  Revision 1.1  1999/11/04 19:02:14  snelling
//  First check in of StFlowMaker. It contains the common code from
//  StFlowTagMaker and StFlowAnalysisMaker.
//
//
///////////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "FlowTag.h"
#include "FlowEvent.hh"
#include "StEvent.h"
#include "StGlobalTrack.h"
#include "StChain.h"

class StFlowMaker : public StMaker {

public:

  StFlowMaker(const Char_t *name="FlowMaker");
  virtual ~StFlowMaker();

  Int_t Init();
  void  PrintInfo();
  Int_t Make();
  Int_t Finish();

  FlowTag_st* tag(); // returns pointer to the tag table

private:

  FlowTag_st*   flowTag;   //! the tag table to fill
  StEvent*      mEvent;    //! pointer to DST data
  TString       MakerName;

protected:

  FlowEvent*  fillFlowEvent();

  ClassDef(StFlowMaker, 1)  // macro for rootcint
};

#endif
