//*-- Author : Jan Balewski
//  
// $Id: histoLPeval.cxx,v 1.1.1.1 2001/04/21 00:43:14 fisyak Exp $
// $Log: histoLPeval.cxx,v $
// Revision 1.1.1.1  2001/04/21 00:43:14  fisyak
// *** empty log message ***
//
// Revision 1.1  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                    
//   histograms initialization for this class
//                                                                    
//////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include "StppLPevalMaker.h"

#include "TH2.h"

//_______________________________________________________________
//_______________________________________________________________
//_______________________________________________________________
void StppLPevalMaker::init_histo()
{
  float pt0=0., pt1=15.;
  int npt=(int)pt1;

  printf("init histo\n");
  hv[0]=new TH1F("R 00v","Zvertex/cm Geant",50, -30., 30.);
  hv[1]=new TH1F("R 01v","#Delta Zvertex/cm Geant-LMV",100, -5., 5.);
  hv[2]=new TH1F("R 02v","#Delta Zvertex/cm Geant-LMV",200, -200., 200.);

  hv[3]=new TH1F("drh","Dr for hit2hit matching, (cm)",100,0.,2.);
  hv[4]=new TH1F("gpt0","all Pythia events vs. gPt (GeV/c)",npt,pt0,pt1);
  hv[5]=new TH1F("gpt1","all recon events with any rLP vs. gPt (GeV/c)",npt,pt0,pt1);
  hv[6]=new TH1F("gpt2","accepted recon events with any rLP vs. gPt (GeV/c)",npt,pt0,pt1);

  char *tt0[]={"1st", "2nd","no","all"};

  printf("init histo 2\n");
  int ilp;
  
  for(ilp=0;ilp<1;ilp++)
   {
     struct RecStep *s=&(lpaS.Rec);
     for(int i=0;i<sizeof(tt1)/sizeof(char*);i++,s++)
       {
	 s->n=0;
	 TString Tt1=tt1[i];
	 TString Tt0=tt0[ilp];
	 s->h=new TH1D((tt0[ilp]+Tt1).Data(),(Tt0+"LeadPart, "+Tt1+" accepted vs. rec. LP pT (GeV/c)").Data(),npt, pt0, pt1); 
	 s->h->SetXTitle("Recon Leading Particle pT (GeV/c)") ;
	 //printf(" %d %d histo=%s set\n",ilp,i,(tt0[ilp]+Tt1).Data());
       }
   }


  printf("init histo .. done\n");

}

//_______________________________________________________________
//_______________________________________________________________
//_______________________________________________________________
void StppLPevalMaker::printStat()
{
  printf("\nSummary of %s after %d input events\n",GetName(),nEVtot);
  
  printf("                        1st Lead Part                  2nd  Lead Part \n");
  printf("  STEP                 Neve   eff  +- err             Neve   eff  +- err\n");
  struct RecStep *s1=&(lpaS.Rec);  
  float in=s1->n;
  float last1=0;  
  for(int i=0;i<sizeof(tt1)/sizeof(char*);i++,s1++) {
    float ef1=-1,er1=-1;
    if(in>0.) {
      ef1=s1->n/in;
      er1=sqrt(s1->n*(in-s1->n)/in)/in;
    }
    if(i==0) {last1=s1->n;}
    int lost1=(int)((1.-s1->n/last1)*100.+.5);
    printf("%1d %4s (F=%2d%c) T= %5d  %6.3f  %6.3f \n",i+1,tt1[i],lost1,'%',s1->n,ef1,er1
);
    last1=s1->n;
  }
  printf("\n");
}

