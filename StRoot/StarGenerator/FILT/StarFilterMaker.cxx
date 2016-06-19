#include "StarFilterMaker.h"
ClassImp(StarFilterMaker);

#include "g2t/St_g2t_particle_Module.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_gepart_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_event_Table.h"

#include "StarCallf77.h"
#include <iostream>
#include "St_geant_Maker/St_geant_Maker.h"
#include "TGiant3.h"
#include <map>
#include "TString.h"
#include "TSystem.h"

#include "StBFChain.h"
#include "TFile.h"
#include "TTree.h"
#include "TClass.h"


#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "StarGenerator/BASE/StarPrimaryMaker.h"
#include "StarGenerator/BASE/AgStarReader.h"
#include "StarGenerator/UTIL/StarRandom.h"

#include "TMCProcess.h"

using namespace std;

// --------------------------------------------------------------------------------------------------------------
StarFilterMaker::StarFilterMaker( const Char_t *name )  :
  StMaker(name),
  NumberofEvents(0),
  AcceptedEvents(0),
  RejectedEvents(0),
  RejectedSinceLast(0),
  mEvent(0),
  bFlag(false)
{
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarFilterMaker::Init()
{

  return kStOK;

}
// --------------------------------------------------------------------------------------------------------------
void StarFilterMaker::Clear( const Option_t *opts )
{

  StMaker::Clear(opts);
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarFilterMaker::Finish()
{ 
  // No need to save the tree because the event generator will take care of that, but we should dump statistics here
  // Dump stats to file and screen

  LOG_INFO << Form("Number of events seen:     %i", NumberofEvents ) << endm;
  LOG_INFO << Form("Number of events accepted: %i", AcceptedEvents ) << endm;
  LOG_INFO << Form("Number of rejected events: %i", RejectedEvents ) << endm;
  LOG_INFO << Form("Rejection power:           %8.3g", double(RejectedEvents)/double(NumberofEvents) ) << endm;
  LOG_INFO << Form("Acceptance:                %8.3g", double(1.0)-double(RejectedEvents)/double(NumberofEvents) ) << endm;

  return StMaker::Finish();
}
// --------------------------------------------------------------------------------------------------------------
Int_t StarFilterMaker::Make()
{

  // Increment total number of events seen
  NumberofEvents++; 

  // Obtain the result from the filter
  UInt_t result = (UInt_t)Filter( mEvent );

  // Set the result on the event
  mEvent -> SetFilterResult( result );

  // If the event was rejected, increment the rejected event counters
  if( result & StarGenEvent::kReject )
  {
    RejectedEvents++;
    RejectedSinceLast++;  
  }
  // Otherwise, inrement accepted events and reset the rejected since last counter
  else if ( result & StarGenEvent::kAccept )
  {
    AcceptedEvents++;
    RejectedSinceLast = 0;
  }
  else
    { // huh?
      assert(0);
    }

  return kStOk;
  
}
