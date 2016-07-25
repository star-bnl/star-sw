#include "TFile.h"
#include "TObjArray.h"
#include "TStopwatch.h"

void er()
{
  int MHITS = 500000;      
  int MHITZ = 100;
  int MTRKS = MHITS/MHITZ;

   TFile ff("e.root");
   printf("*** Read started\n");
   TStopwatch www;
   TObjArray *evt = (TObjArray*)ff.Get("evt");  
   www.Print();
   printf("*** Read finished\n");


  char cbuf[100];

  TObjArray *hits = (TObjArray *)evt->At(0);
  if (strcmp(hits->GetName(),"Hits")!=0) 
  {
     printf ("***Error %s != Hits\n",hits->GetName());
     return;
  }
  if (hits->GetEntries() != MHITS) 
  {
     printf ("***Error hits %d != %d\n",hits->GetEntries(),MHITS);
     return;
  }


  TObjArray *trks = (TObjArray *)evt->At(1);
  if (strcmp(trks->GetName(),"Trks")!=0) 
  {
     printf ("***Error %s != Trks\n",trks->GetName());
     return;
  }
  if (trks->GetEntries() != MTRKS) 
  {
     printf ("***Error hits %d != %d\n",trks->GetEntries(),MTRKS);
     return;
  }


  TObjArray *vtx = (TObjArray *)evt->At(2);
  if (strcmp(vtx->GetName(),"Vtx")!=0) 
  {
     printf ("***Error %s != Vtx\n",vtx->GetName());
     return;
  }
  if (vtx->GetEntries() != MTRKS) 
  {
     printf ("***Error hits %d != %d\n",vtx->GetEntries(),MTRKS);
     return;
  }



  int ih;
  printf("*** Hits started\n");
  for (ih=0;ih<MHITS;ih++) {
    sprintf(cbuf,"hit%d",ih);
    TNamed *hit = (TNamed *)hits->At(ih);
    if (strcmp(hit->GetName(),cbuf)!=0) 
    {
     printf ("***Error %s != %s\n",hit->GetName(),cbuf);
     return;
    }
    
  }
  printf("*** Hits finished\n");
  
  int it;
  for (it=0;it<MTRKS;it++) {
    TObjArray *trk = (TObjArray *)trks->At(it);
    TObjArray *trg = (TObjArray *)vtx->At(it);
    if (trk != trg)
    {
     printf ("***Error  trk !=trg\n");
     return;
    }

    for (ih = 0; ih <MHITZ; ih++) {

      if (trk->At(ih) == hits->At(it*MHITZ+ih)) continue;
      printf ("***Error  trk.hit !=hit\n");
      return;
    }
  }
  printf("*** Traks finished\n");
}   
   
