#include "StHeader.h"
#include <stdio.h>
 
ClassImp(StHeader)

StHeader::StHeader()
{
  fRun=0;	
  fNvertex=0;
  fNprimary=0;
  fNtrack=0;
  fEvent=0;
}

StHeader::StHeader(Int_t run, Int_t event)
{
  fRun=run;	
  fNvertex=0;
  fNprimary=0;
  fNtrack=0;
  fEvent=event;
}

void StHeader::Reset(Int_t run, Int_t event)
{
  fRun=run;	
  fNvertex=0;
  fNprimary=0;
  fNtrack=0;
  fEvent=event;
}

void StHeader::Dump()
{
  printf(
"\n=========== Header for run %d Event %d = beginning ======================================\n",
  fRun,fEvent);
  printf("              Number of Vertex %d\n",fNvertex);
  printf("              Number of Primary %d\n",fNprimary);
  printf("              Number of Tracks %d\n",fNtrack);
  printf(
  "=========== Header for run %d Event %d = end ============================================\n\n",
  fRun,fEvent);
}
