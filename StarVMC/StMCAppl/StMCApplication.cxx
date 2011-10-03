// $Id: StMCApplication.cxx,v 1.1.1.1 2004/07/17 20:02:55 perev Exp $
//

#include "Riostream.h"

#include "StMCApplication.h"
#include "StMCStack.h"
#include "GCall.h"
#include "StMCField.h"

#include <TROOT.h>
#include <TInterpreter.h>
#include <TVirtualMC.h>
#include <TPDGCode.h>

ClassImp(StMCApplication)

//_____________________________________________________________________________
StMCApplication::StMCApplication(const char *name, const char *title) 
  : TVirtualMCApplication(name,title),
    fStack(0), fileBased(0)
{
// Standard constructor
// ---
   int n = fUserEnd-fUser;
   memset(&fUser,0,n*sizeof(void*));

}

//_____________________________________________________________________________
StMCApplication::StMCApplication()
  : TVirtualMCApplication(),
    fStack(0), fileBased(0)

{    
   int n = fUserEnd-fUser;
   memset(fUser,0,n*sizeof(void*));
// Default constructor
// ---
}

//_____________________________________________________________________________
StMCApplication::~StMCApplication() 
{
// Destructor  
   int n = fUserEnd-fUser;
   for (int i=1;i<n;i++) {delete fUser[i];}
// ---
  delete fStack;
  delete gMC;
  gMC = 0;
}
//_____________________________________________________________________________
void StMCApplication::InitUser() 
{
   int n = fUserEnd-fUser;
   for (int i=1;i<n;i++) { 
     GCall *us = fUser[i];
     if (!us) continue;
     us->SetInterfaces(this,gMC,fStack);
     us->Init();
   }

}

//
// private methods
//
//_____________________________________________________________________________
void StMCApplication::setFileBased() {
  fileBased=1;
}


//
// public methods
//

//_____________________________________________________________________________
void StMCApplication::InitMC(const char* setup)
{    
// Initialize MC.
// ---

  gROOT->LoadMacro(setup);
  gInterpreter->ProcessLine("Config()");
 
  if (!fStack) fStack = new StMCStack(100);
  gMC->SetStack(fStack);
  gMC->Init();
  gMC->BuildPhysics(); 
}

//_____________________________________________________________________________
void StMCApplication::RunMC(Int_t nofEvents)
{    
// MC run.
// ---
  InitUser();
  gMC->ProcessRun(nofEvents);
  FinishRun();
}

//_____________________________________________________________________________
void StMCApplication::FinishRun()
{    
// Finish MC run.
// ---
  if (fFinishRun) (*fFinishRun)();
//  fRootManager.Write();
}

//_____________________________________________________________________________
void StMCApplication::ConstructGeometry()
{    
// Construct geometry using detector contruction class
// ---

  if (fConstructGeometry) (*fConstructGeometry)();
}

//_____________________________________________________________________________
void StMCApplication::InitGeometry()
{    
// Initialize geometry
// ---
    if (fStepping)  fStepping->Init(); 

  cout<<"StMCApplication::InitGeometry()"<<endl;

}

//_____________________________________________________________________________
void StMCApplication::GeneratePrimaries()
{    
  // Fill the user stack (derived from TVirtualMCStack) with primary particles.
  // ---
  (*fGeneratePrimaries)();
}

//_____________________________________________________________________________
void StMCApplication::BeginEvent()
{    
  // User actions at beginning of event
  // ---
  if (fBeginEvent) (*fBeginEvent)();
}

//_____________________________________________________________________________
void StMCApplication::BeginPrimary()
{    
  // User actions at beginning of a primary track
  // ---
  if (fBeginPrimary) (*fBeginPrimary)();
}

//_____________________________________________________________________________
void StMCApplication::PreTrack()
{    
  // User actions at beginning of each track
  // ---
  if (fPreTrack) (*fPreTrack)();
}

//_____________________________________________________________________________
void StMCApplication::Stepping()
{    
  // User actions at each step
  // ---
  if (fStepping) (*fStepping)();
}

//_____________________________________________________________________________
void StMCApplication::PostTrack()
{    
  // User actions after finishing of each track
  // ---
  if(fPostTrack) (*fPostTrack)();
}

//_____________________________________________________________________________
void StMCApplication::FinishPrimary()
{    
  // User actions after finishing of a primary track
  // ---
  if(fFinishPrimary) (*fFinishPrimary)();
}

//_____________________________________________________________________________
void StMCApplication::FinishEvent()
{    
  // User actions after finishing of an event
  // ---
  if(fFinishEvent) (*fFinishEvent)();
   
  fStack->Print();  
  fStack->Clear();
} 

//_____________________________________________________________________________
void StMCApplication::Field(const Double_t* x, Double_t* b) const
{
   if (fField) {fField->Field(x,b); return;}
   b[0]=0;b[1]=0;b[2]=0;
}

  
