///////////////////////////////////////////////////////////////////////////////
//
//  $Id: StFlowMaker.hh,v 1.9 2000/02/18 22:49:57 posk Exp $
//
// Author List: 
//  Raimond Snellings and Art Poskanzer, LBNL, 6/99
//
///////////////////////////////////////////////////////////////////////////////
//
// Description: 
//  Maker to fill StFlowEvent from StEvent
//
///////////////////////////////////////////////////////////////////////////////
//
//  $Log: StFlowMaker.hh,v $
//  Revision 1.9  2000/02/18 22:49:57  posk
//  Added PID and centrality.
//
//  Revision 1.8  2000/01/13 22:19:20  posk
//  Updates and corrections.
//
//  Revision 1.7  1999/12/21 01:11:01  posk
//  Added more quantities to StFlowEvent.
//
//  Revision 1.6  1999/12/16 18:05:24  posk
//  Fixed Linux compatability again.
//
//  Revision 1.5  1999/12/15 22:01:28  posk
//  Added StFlowConstants.hh
//
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

#ifndef StFlowMaker_HH
#define StFlowMaker_HH
#include <iostream.h>
#include <stdlib.h>
#include "StMaker.h"
#include "TString.h"
#include "StFlowConstants.hh"
class StEvent;
class StPrimaryTrack;
class StParticleDefinition;
class StFlowEvent;

class StFlowMaker : public StMaker {

public:

                  StFlowMaker(const Char_t *name="Flow");
  virtual         ~StFlowMaker();

  Int_t           Init();
  void            PrintInfo();
  Int_t           Make();
  Int_t           Finish();
  StFlowEvent*    FlowEventPointer() const;  // returns pointer to the StFlowEvent

protected:

  Flow::PhiWgt_t  mPhiWgt;                   // To make event plane isotropic

private:

  Int_t           readPhiWgtFile();
  void            fillFlowEvent();
  Float_t         PidSigmas(const StPrimaryTrack*, 
			    const StParticleDefinition* particle);
  StEvent*        pEvent;                    //! pointer to DST data
  StFlowEvent*    pFlowEvent;                //! pointer to micro-DST data

  ClassDef(StFlowMaker, 1)                   // macro for rootcint

};

inline StFlowEvent* StFlowMaker::FlowEventPointer() const { return pFlowEvent; }

#endif
