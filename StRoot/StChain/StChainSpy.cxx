// $Id: StChainSpy.cxx,v 1.1 1998/10/07 18:44:00 perev Exp $

#include "StChainSpy.h"
StFarmSpy gSpy;
ClassImp(StChainSpy)

//_____________________________________________________________________________
Int_t StChainSpy::Init()
{
  const char *inputfile=0,*ouputfile=0;
  const St_XDFFile *inp,*out;  
  int inperr=0,outerr=0;

//StFarmSpy::StartJob();

 int iInit = StChain::Init();

   if ((inp=GetXDF_in())) 	{// get info about input
     inputfile = inp->GetName();
     inperr    = inp->GetErrorCode();}
   if ((out = GetXDF_out()))	{// get info about output
     ouputfile = out->GetName();
     outerr    = out->GetErrorCode();}
   if (!inputfile) inputfile = "Unknown";
   if (!ouputfile) ouputfile = "Unknown";
   iInit =+ 100 * ( inperr + 100*outerr);  
   StFarmSpy::StartJob(GetName(),inputfile,ouputfile,iInit,"");
   return iInit;
}

///_____________________________________________________________________________
Int_t StChainSpy::Finish()
{
int iFinish = StChain::Finish();
StFarmSpy::EndJob(iFinish);
return iFinish;
}

//_____________________________________________________________________________
Int_t StChainSpy::Make(Int_t i)
{
   StFarmSpy::NewEvent(Run(),Event());
   Int_t iMake = StChain::Make(i);
   StFarmSpy::EndEvent(Run(),Event(),iMake,"");
   return iMake;   
}

void StChainSpy::StartMaker(StMaker *mk)
{
  char buf[100];
  sprintf(buf,"%s Started",mk->GetName()); 
  StFarmSpy::Comment(buf);
  StChain::StartMaker(mk);
}
void StChainSpy::EndMaker(StMaker *mk,Int_t ierr)
{
  char buf[100];
  StChain::EndMaker(mk,ierr);
  sprintf(buf,"%s Ended",mk->GetName()); 
  StFarmSpy::Comment(buf);
}
void StChainSpy::Fatal(int Ierr,const char *Com)
{
  StFarmSpy::EndJob(Ierr,Com);
  StChain::Fatal(Ierr,Com);
}




  
