//===================================================================
// 09.19.00 Changes in Histogram names, GetName information.  
//                      George
// 
//*-- Author : George  
//  modified by JB, 9/19/00
// 
// $Id: StppTrigMaker.cxx,v 1.1.1.1 2001/01/31 14:00:07 balewski Exp $
// $Log: StppTrigMaker.cxx,v $
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
// StppTrigMaker class for Makers                                      
//
// It will be pt trigger  for p+p
//                                                                     
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>
#include <stdlib.h> 

#include "StppTrigMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "TH1.h"
#include "TH2.h"

// for MiniDst
#include "StEventTypes.h" 
#include "StppMiniDst.h" 

//for ppTag
#include "tables/St_ppSpinTag_Table.h"


//JB+GG

#include "tables/St_g2t_ctf_hit_Table.h"
#include "tables/St_mwc_raw_Table.h"
ClassImp(StppTrigMaker)

//_____________________________________________________________________________
//_____________________________________________________________________________
StppTrigMaker::StppTrigMaker(const char *name):StMaker(name){
 //  const char *name -  the name of this constructor
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
  Setup(80., 1.5, 10, 1); 
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
StppTrigMaker::~StppTrigMaker(){  
printf("\"%s\" setup was: CTB_time_max=%4.1f/ns Thres=%f/MeV sumADC>%d MWCMULT=%d\n", 
GetName(),set.ctb_time_max_ns, set.ctb_ener_thr_mev,
 set.ctb_sumADC_min, set.mwc_sect_mult_min); 
//  printf(" DDDDDDDDDDDDDD Destructor executed\n");
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppTrigMaker::Init(){
  //  Init - empty

  //  printf(" ppTR add=%d\n",(int)&decision);

printf("InInInInInInInInInIn    Initialization start \"%s\",  m_Mode=%d... \n", GetName(),m_Mode);
printf(" SETUP IS CTB_time_max%4.1f/ns Thres=%f/MeV sumADC>%d MWCMULT=%d\n", set.ctb_time_max_ns, set.ctb_ener_thr_mev,
 set.ctb_sumADC_min, set.mwc_sect_mult_min); 

h1 = new TH1F("trg_out","Trigger decision",10, -1.5, 8.5);
h1->SetXTitle(" trigger ") ;
h1->SetYTitle(" Number of events ") ;

h2 = new TH1F("ctb_tof","Tof for CTB(all)",100, 0.,  500.);
h2->SetXTitle("time, ns ") ;
h2->SetYTitle(" Number of counts ") ;

h3 = new TH1F("ctb_ede","Acum Energy in CTB slat(with Tof cut)",100, 0.,  20.);
h3->SetXTitle("Energy, MeV ");
h3->SetYTitle(" Number of Counts  ") ;

h4 = new TH1F("ctb_mul","CTB slats above Energy thr" ,50, -.5, 49.5);
h4->SetXTitle("Number of slats") ;
h4->SetYTitle("Population") ;

h5 = new TH1F("mwc_fir", "MWC  sectors (all) ", 20, -.5, 19.5);
h5->SetXTitle(" Number of wires") ;
h5->SetYTitle("Counts   ") ;

h6 = new TH1F("mwc_bac"," MWC sectors (-Z)",50, -.5, 49.5);
h6->SetXTitle(" Number of fired sectors") ;
h6->SetYTitle(" Counts ") ;

h7 = new TH1F("mwc_for"," MWC sectors (+Z) ",50, -.5, 49.5);
h7->SetXTitle(" Number of fired sectors") ;
h7->SetYTitle(" Counts ") ;

h25 = new TH2F("mwc_all","MWC sectors ",50, -.5, 49.5, 50, -.5, 49.5);
h25->SetXTitle("Number of +Z sectors ") ;
h25->SetYTitle(" Number of -Z sectors ") ;

// JB histos
hj[0] =new TH1F("j0","CTB  ADC/slat",156,-0.5,155.5);
hj[1] =new TH1F("j1","CTB  sumADC/eve",100,0.,1000.);
hj[2] =(TH1F*)new TH2F("j2","CTB nSalt vs.  sumADC ",50,0.,500.,50,0,50);


h25->SetXTitle("Number of +Z sectors ") ;
h25->SetYTitle(" Number of -Z sectors ") ;

  printf("InInInInInInInInInIn Initialization \"%s\"end\n",GetName());

   return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StppTrigMaker::Make(){
 
   cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;
 
  printf("add StppSpinTag ..\n");
  // Create a data set and add the table to it.
  St_ppSpinTag *tagtab= new St_ppSpinTag("ppSpinTag",1); 
  m_DataSet->Add(tagtab);
  
  ppSpinTag_st tagrow;
  //fill default values for the ppSPin Tags
  tagrow.chargeLeadingParticlePt=-55.;
  tagtab->AddAt(&tagrow,0);
  // end of tag initialization
  
  decision=0;  // clear trigger to FALSE
  Int_t i =0;
  Int_t iw =0;
  Int_t id = -1;
  Int_t iback =0;
  Int_t iforw =0;
  Int_t nSlat =0;
  Int_t ntrr = 0;

  // CTB digitalization
  float Escin2ADC=2300; // MIP= ch. #5 = 2.2 MeV
  int ADCrange=255;
  int CtbThresADC=2;// only signals above are accumulated
  int sumADC=0; // value for this event

  const int mxne = 1200;
  float en[mxne]; // for CTB slats

  memset(en, 0, sizeof(en));

  h1->Fill(-1.);  

  // Read in MWC Tables
  St_DataSet *ds=NULL; 
  St_mwc_raw  *tmwr=NULL;
  mwc_raw_st *tr=NULL;
 
  ds=GetDataSet(".make/mwc");
  if(ds == 0) {  Print("No .make/mwc data set \n"); goto mwc_end; }

  tmwr= (St_mwc_raw   *) ds->Find(".data/raw");
  if(tmwr ==NULL) { Print("No mwc/raw table \n"); goto mwc_end; } 
    
  tr= tmwr->GetTable();
  assert(tr); 
  ntrr=tmwr->GetNRows(); 
  
  for (i = 0; i < ntrr ; i++,tr++){
    h5->Fill(float(tr->count));  
    if (tr->sector < 49) {
      //printf(" back sec=%d, count=%d\n",tr->sector,tr->count);
      if(tr->count>0)iback+=tr->count;
    } 
    else  {
      //printf(" forw sec=%d, count=%d\n",tr->sector,tr->count);
      if(tr->count>0) iforw+=tr->count;
    } 
  }
  
  if (iforw >= set.mwc_sect_mult_min  && iback >=set.mwc_sect_mult_min )
   decision += 1;

  mwc_end:

  //END of MWC part.
 
  //CTB part from GEANT

  St_DataSetIter geant(GetDataSet("geant")); 
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) geant("g2t_ctb_hit");
  g2t_ctf_hit_st *t = NULL;

  if (g2t_ctb_hit == 0) { Print("No geant/ctb data set \n"); goto ctb_end; }
  t = g2t_ctb_hit->GetTable();
  assert(t); 

  for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,t++){
    h2->Fill((t->tof)*1.e9);  
    
    if(t->tof > set.ctb_time_max_ns*1.e-9) continue;  
    //    if(t->volume_id < 1300) id = t->volume_id -1100;
    //else id =   t->volume_id -1900;
    id = t->volume_id -1100;
    if(id < 0 || id >=mxne) {
      printf("id out of margin %d\n ", id);
          assert(0);
    }          
    en[id]+=t->de; 
  }  
  
  // count slats with energy above threshold
  for(iw =0; iw < mxne; ++iw) {
    if(en[iw] <= 0.) continue;
    h3->Fill(en[iw]*1000.);
    if(en[iw]*1000. <set.ctb_ener_thr_mev) continue;
    nSlat++;             
  }
  
  //  do ADC sum
  for(iw =0; iw < mxne; ++iw) 
    {
      if(en[iw] <= 0.) continue;
      int iADC=(int)(en[iw]*Escin2ADC);// digitalization
      if(iADC>ADCrange)iADC=ADCrange;
      hj[0]->Fill(iADC);
      //       printf("%d %f %d\n",iw,en[iw],iADC);
      if(iADC<=CtbThresADC) continue;
      sumADC+=iADC;
    }
  hj[1]->Fill(sumADC);
  ((TH2*) hj[2])->Fill(sumADC,nSlat);
  
 ctb_end:  
  
  {
    printf("StppMiniDst STORE..\n");
    StppMiniDst *my=new StppMiniDst;
    my->CtbAdcSumChan=sumADC;
    StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
    stEvent->content().push_back(my);   
  }
  // test only

  StppMiniDst *my2=StppMiniDst::GetppMiniDst(this);  assert(my2);
  printf("add=%d, polDir=%d\n",(int)my2, my2->polDir);
  
  h4->Fill(float(nSlat)) ;
  h6->Fill(float(iback));
  h7->Fill(float(iforw));
  h25->Fill(float(iforw),float(iback),1.) ;
  
  if (sumADC >= set.ctb_sumADC_min) decision +=2;
  
  h1->Fill(float( decision));
  
  // end of tag initialization
  
  printf("\n \"%s\": MWCnSlat=%d, n-ZDC=%d, n+ZDC=%d sumADC=%d  Decision--->%d\n\n",GetName(),nSlat,iback,iforw,sumADC,decision);
  
  if(m_Mode==1 &&  decision!=1) return kStErr;
  if(m_Mode==2 &&  decision<2) return kStErr;
  
  return kStOK;
}

//------------------------------------------------------------------------
  Int_t StppTrigMaker::Finish()
{

  Float_t *p=h1->GetArray();

printf("\n \"%s\" summary: Total number of events  %d\n # of events of type: A0=%d, A1=%d, A2=%d, A3=%d\n\n",GetName(), (int)p[1], (int)p[2], (int)p[3], (int)p[4], (int)p[5]);

 return kStOK;

}
