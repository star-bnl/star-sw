// $Id: StVMCApplication.cxx,v 1.2 2009/06/07 02:28:36 perev Exp $
// Class StVMCApplication
// ----------------------- 
// Implementation of the TVirtualMCApplication
#include <assert.h>
#include "StVMCApplication.h"
#include "GCall.h"
#include "TGeoManager.h"
#include "TROOT.h"
#include "TPDGCode.h"
#include "TGeant3TGeo.h"
#include "StMCStack.h"
ClassImp(StVMCApplication);

//_____________________________________________________________________________
StVMCApplication::StVMCApplication(const char *name, const char *title)
{
  SetName(name); SetTitle(title);
  memset(mBeg,0,mEnd-mBeg+1);

}
//_____________________________________________________________________________
StVMCApplication::~StVMCApplication() 
{  // Destructor  
}
//_____________________________________________________________________________
int StVMCApplication::Trig(int nTrig) 
{
   return gMC->ProcessRun(nTrig);
}
//_____________________________________________________________________________
int StVMCApplication::Init() 
{   
  if (mInit) (*mInit)();
  SetDebug(mDebug);
  return 0;
}
//_____________________________________________________________________________
void StVMCApplication::Stepping() 
{   
  (*mStepping)();
}
//_____________________________________________________________________________
void StVMCApplication::GeneratePrimaries() 
{   
  (*mPrimaryGenerator)();
}
//_____________________________________________________________________________
void StVMCApplication::ConstructGeometry() 
{    // Initialize geometry
  if (mConstructGeometry) (*mConstructGeometry)();
}
//_____________________________________________________________________________
void StVMCApplication::InitGeometry() 
{    
  if (mInitGeometry) (*mInitGeometry)();
}
//_____________________________________________________________________________
void StVMCApplication::BeginEvent() 
{    
// User actions at beginning of event
  if (mBeginEvent) (*mBeginEvent)();
}
//_____________________________________________________________________________
void StVMCApplication::BeginPrimary() 
{    
// User actions at beginning of a primary track
  if (mBeginPrimary) (*mBeginPrimary)();
}
//_____________________________________________________________________________
void StVMCApplication::PreTrack() 
{    
// User actions at beginning of each track
  if (mPreTrack) (*mPreTrack)();
}
//_____________________________________________________________________________
void StVMCApplication::PostTrack() 
{    
// User actions after finishing of each track
  if (mPostTrack) (*mPostTrack)();
}
//_____________________________________________________________________________
void StVMCApplication::FinishPrimary() 
{    
// User actions after finishing of a primary track
  if (mFinishPrimary) (*mFinishPrimary)();
}
//_____________________________________________________________________________
void StVMCApplication::FinishEvent() 
{    
// User actions after finishing of an event
  if (mFinishEvent) (*mFinishEvent)();
} 
//_____________________________________________________________________________
void StVMCApplication::Field(const double* x, double* b) const 
{
  if (mField) 	{ (*mField)(x,b);} 
  else      	{b[0]=0;b[1]=0;b[2]=5;}
}
//________________________________________________________________________________
void StVMCApplication::SetDebug(int l) 
{
  mDebug = l;
  TGeant3TGeo *g3g = (TGeant3TGeo*)gMC;
  if (!g3g) return; 

  g3g->Gcflag()->idebug = mDebug ;
  g3g->Gcflag()->itest = 100 ;
  if (mDebug > 1) {
    g3g->SetDEBU(1,1,100);
    g3g->SetSWIT(1,2);
    g3g->SetSWIT(2,2);
  } else {
    g3g->SetSWIT(4,0);
  }
}
