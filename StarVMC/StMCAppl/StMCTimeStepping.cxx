// $Id: StMCTimeStepping.cxx,v 1.1 2005/03/09 18:35:34 perev Exp $
//
//
// Class StMCTimeStepping
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "StMCTimeStepping.h"
#include "StTGeant3.h"
#include "StiTimer.h"
static  TVirtualMC *myMC=0;

ClassImp(StMCTimeStepping)
//_____________________________________________________________________________
StMCTimeStepping::StMCTimeStepping(const char *name,const char *tit)
  : GCall(name,tit)
{
StiTimer::Init("VMCTimer",StiTimer::fgFindTimer, StiTimer::fgFindTally); 
myMC = TVirtualMC::GetMC();

}   
//_____________________________________________________________________________
void StMCTimeStepping::Print(const Option_t*) const
{
}		
//_____________________________________________________________________________
int StMCTimeStepping::Fun()
{
  if(myMC->TrackLength() == 0      ) StiTimer::fgFindTimer->Start(0);
  if(myMC->IsTrackEntering 	 ()) StiTimer::fgFindTally++;
  if(myMC->IsTrackOut            ()) StiTimer::fgFindTimer->Stop();
//  if(myMC->IsTrackStop           ()) StiTimer::fgFindTimer->Stop();
  return 0;
}		
//_____________________________________________________________________________
void StMCTimeStepping::Finish(const Option_t*)
{
  StiTimer::Print();
}	
	
	
	
	
	
	
	
	
	
	
	
