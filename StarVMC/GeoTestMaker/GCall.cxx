// $Id: GCall.cxx,v 1.2 2009/06/07 02:28:36 perev Exp $
//
//
// Class GCall
// ------------------
// Base class for Magnetic field calculation

#include <stdio.h>
#include <string.h>
#include "TError.h"
#include "GCall.h"

ClassImp(GCall)
//_____________________________________________________________________________
GCall::GCall(const char *name,const char *tit)
  : TNamed(name,tit),fMC(0),fMCA(0)
{
  fDebug=0;
}   
//_____________________________________________________________________________
 void GCall::SetInterfaces(TVirtualMCApplication *mca,TVirtualMC *mc,TVirtualMCStack *stk)
{
   fMC = mc; fMCA=mca;fStack = stk;
}
//_____________________________________________________________________________
int GCall::FunDD(const Double_t*, Double_t* b) 
{ 
  Error("FunDD","Empty method is called");
  assert(0); return 0;

  }
//_____________________________________________________________________________
void GCall::Field(const Double_t*, Double_t* b) 
{ 
  Error("Field","Empty method is called");
  assert(0);
}
//_____________________________________________________________________________
int GCall::Fun()
{ 
  Error("Fun","Empty method is called");
  assert(0);return 0;
}
//_____________________________________________________________________________
void GCall::Print(const Option_t*) const
{
  Error("Print","Empty method is called");
}		
		
		
