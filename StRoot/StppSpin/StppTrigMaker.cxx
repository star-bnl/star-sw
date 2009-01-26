//  modified by JB 2/2/01: trigOnCtb() isolated and upgraded
// 
//*-- Author : George , Jan Balewski 
// $Id: StppTrigMaker.cxx,v 1.12 2009/01/26 15:18:34 fisyak Exp $
// $Log: StppTrigMaker.cxx,v $
// Revision 1.12  2009/01/26 15:18:34  fisyak
// Remove mwc
//
// Revision 1.11  2007/04/28 17:56:43  perev
// Redundant StChain.h removed
//
// Revision 1.10  2003/09/02 18:00:19  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.9  2002/04/19 22:24:23  perev
// fixes for ROOT/3.02.07
//
// Revision 1.8  2001/06/22 19:41:46  balewski
// *** empty log message ***
//
// Revision 1.7  2001/04/23 19:44:26  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/23 15:02:11  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/13 18:04:35  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
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
//  Emulates trigger response for the M-C data
// 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>
#include <stdlib.h> 

#include "StppTrigMaker.h"
#include "St_DataSetIter.h"

#include "TH1.h"
#include "TH2.h"

// for gen LP
#include "tables/St_g2t_track_Table.h"

//JB+GG
#include "tables/St_g2t_ctf_hit_Table.h"
#if 0
#include "tables/St_mwc_raw_Table.h"
#endif
void cts_get_ctb_indexes ( long volume, long &i_phi, long &i_eta ) ;

ClassImp(StppTrigMaker)

//_____________________________________________________________________________
//_____________________________________________________________________________
StppTrigMaker::StppTrigMaker(const char *name):StMaker(name){
 //  const char *name -  the name of this constructor
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
  Setup(70., 1., 3, 777); // trig= (MWC(E&&W) || 2+ CTB slats) ignore DiSlats
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
StppTrigMaker::~StppTrigMaker(){  
  printf("\"%s\" CTB-trig setup:\n  TOF<%f ns, dEn>%f MeV, nSlat>=%d, nDiPatch>=%d\n",GetName(),set.CtbTofMax_ns,set.CtbDEnThres_mev,set.CtbnSlatMin,set.CtbnDiPatchMin);
//  printf(" DDDDDDDDDDDDDD Destructor executed\n");
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppTrigMaker::Init(){
  //  Init - empty

  //  printf(" ppTR add=%d\n",(int)&decision);

  printf("InInInInInInInInInIn    Initialization start \"%s\",  m_Mode=%d... \n", GetName(),m_Mode);
  printf("\"%s\" CTB-trig setup:\n  TOF<%f ns, dEn>%f MeV, nSlat>=%d, nDiPatch>=%d\n",GetName(),set.CtbTofMax_ns,set.CtbDEnThres_mev,set.CtbnSlatMin,set.CtbnDiPatchMin);

  h1 = new TH1F("trg_out","Trigger decision",10, -1.5, 8.5);
  h1->SetXTitle(" trigger ") ;
  h1->SetYTitle(" Number of events ") ;

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
    
  h25->SetXTitle("Number of +Z sectors ") ;
  h25->SetYTitle(" Number of -Z sectors ") ;
  
  // CTB histos
  hctb[0] = new TH1F("ctb_tof","Tof for CTB(all)",100, 0.,  500.);
  hctb[0]->SetXTitle("time, ns ") ;
  hctb[0]->SetYTitle(" Number of counts ") ;
  
  hctb[1] =NULL;
  hctb[2] =NULL;
  hctb[3] =NULL;

  hctb[4]= new TH1F("ctb_ede","Acum Energy in CTB slat(with Tof cut)",100, 0.,  20.);
  hctb[4]->SetXTitle("Energy, MeV ");
  hctb[4]->SetYTitle(" Number of Counts  ") ;
  
  hctb[5]= new TH1F("ctb_mul","CTB slats above Energy thr" ,50, -.5, 49.5);
  hctb[5]->SetXTitle("Number of slats") ;
  hctb[5]->SetYTitle("Population") ;
  
  hctb[6] =(TH1F*)new TH2F("d1","CTB nDiPatch vs. Mult, 0<LP pT<1",20,0.,50.,20,0,40);
  hctb[7] =(TH1F*)new TH2F("d2","CTB nDiPatch vs. Mult, 3<LP pT<4",20,0.,50.,20,0,40);
  hctb[8] =(TH1F*)new TH2F("d3","CTB nDiPatch vs. Mult, 6<LP pT<7",20,0.,50.,20,0,40);
  hctb[9] =(TH1F*)new TH2F("d4","CTB nDiPatch vs. Mult, 9<LP pT<10",20,0.,50.,20,0,40);

  hctb[10] =(TH1F*)new TH2F("EvT","CTB dE/track/MeV vs. TOF/ns", 20,0.,100.,20,0.,20.);

  hctb[11] =(TH1F*)new TH2F("o1","CTB nDiPatch vs. n1Patch, 0<LP pT<1",20,0.,30.,20,0,40);
  hctb[12] =(TH1F*)new TH2F("o2","CTB nDiPatch vs. n1Patch, 3<LP pT<4",20,0.,30.,20,0,40);
  hctb[13] =(TH1F*)new TH2F("o3","CTB nDiPatch vs. n1Patch, 6<LP pT<7",20,0.,30.,20,0,40);
  hctb[14] =(TH1F*)new TH2F("o4","CTB nDiPatch vs. n1Patch, 9<LP pT<10",20,0.,30.,20,0,40);


  float pt0=0., pt1=15.;
  int npt=(int)pt1;

  hge[0] =new TH1F("ge0","PYTHIA: accepted LP-pT/GeV/c",npt,pt0,pt1);
  hge[1] =(TH1F*)new TH2F("ge1","PYTHIA: nCharge in CTB vs. LP-pT/GeV/c",npt,pt0,pt1,11,-2.5,30.5);

  hge[2] =(TH1F*)new TH2F("ge2","PYTHIA: n #pi^{+} +#pi^{-} in CTB  vs. LP-pT/GeV/c",npt,pt0,pt1,11,-1.5,20.5);

  hge[3] =(TH1F*)new TH2F("ge3","PYTHIA: CTB nSlat  vs. LP-pT/GeV/c",npt,pt0,pt1,20,0.,60.);

  hge[4] =(TH1F*)new TH2F("ge4","PYTHIA: CTB nDiPatch vs. LP-pT/GeV/c",npt,pt0,pt1,15,0.,30.);

  hge[5] =(TH1F*)new TH2F("ge5","PYTHIA: CTB n1Patch vs. LP-pT/GeV/c",npt,pt0,pt1,11,0.,22.);

  printf("InInInInInInInInInIn Initialization \"%s\"end\n",GetName());
  
  return StMaker::Init();
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppTrigMaker::Make(){
 
  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;
   
  float gpT=-1;
  int nTr1=-2, nTr2=-3;

  h1->Fill(-1.);  

  decision=0;  // clear trigger to FALSE

  int ibackMWC=-3, iforwMWC=-4;
  trigOnMwc( iforwMWC , ibackMWC);
  if (iforwMWC >=1  && ibackMWC >=1 )   decision+=1;

  // start processing the C T B  information

  int nSlat=-1, nDiPatch=-2, n1Patch=-3;
  trigOnCtb(nSlat, nDiPatch, n1Patch);

  if(nSlat>=set.CtbnSlatMin)   decision+=2;
  if(nDiPatch>=set.CtbnDiPatchMin)   decision+=4;

  //     H I S T O G R A M M I N G

  if(getGeneratedLP(gpT,nTr1, nTr2) <0) gpT=0.2;; // no valid pT was found
  printf("Generated pT=%f, nTr1=%d, nTr2=%d\n",gpT,nTr1,nTr2);
  

  h1->Fill(float( decision));

  hge[0]->Fill(gpT);
  ((TH2*) hge[1])->Fill(gpT,nTr1);
  ((TH2*) hge[2])->Fill(gpT,nTr2);
  ((TH2*) hge[3])->Fill(gpT,nSlat);
  ((TH2*) hge[4])->Fill(gpT,nDiPatch);
  ((TH2*) hge[5])->Fill(gpT,n1Patch);
  
  
  if(gpT<1) {
    ((TH2*) hctb[6])->Fill(nSlat,nDiPatch);
    ((TH2*) hctb[11])->Fill(n1Patch,nDiPatch);
  }
  else if(gpT>3 && gpT<4) {
    ((TH2*) hctb[7])->Fill(nSlat,nDiPatch);
    ((TH2*) hctb[12])->Fill(n1Patch,nDiPatch);
  } 
  else if(gpT>6 && gpT<7) {
    ((TH2*) hctb[8])->Fill(nSlat,nDiPatch);
    ((TH2*) hctb[13])->Fill(n1Patch,nDiPatch);
  } 
  else if(gpT>9 && gpT<10) {
    ((TH2*) hctb[9])->Fill(nSlat,nDiPatch);
    ((TH2*) hctb[14])->Fill(n1Patch,nDiPatch);
  } 
  
  printf("\n \"%s\":  n-MWC=%d, n+MWC=%d CTBnSlat=%d  CTBnDiPatch=%d decision=%d\n",GetName(),ibackMWC,iforwMWC,nSlat,nDiPatch,decision);  

  if(m_Mode==1 && decision==0) return kStErr;
  return kStOK;
}

//------------------------------------------------------------------------
  Int_t StppTrigMaker::Finish()
{

  Float_t *p=h1->GetArray();

printf("\n \"%s\" summary: Total number of events  %d\n # of events of type: A0=%d, A1=%d, A2=%d, A3=%d\n\n",GetName(), (int)p[1], (int)p[2], (int)p[3], (int)p[4], (int)p[5]);

 return kStOK;

}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
void StppTrigMaker::trigOnCtb(  int &nSlat, int &nDiPatch, int &n1Patch )
{

  int i,j;

  const int MXphi=60, MXeta=4, MXpatch=12; // counts CTB-slats
  float en[MXphi][MXeta]; // energy deposit (MeV) for CTB slats after TOF-cut
  int patchADC[MXpatch]; // nSlats >Ethres for 1*1.5 patches

  // clear
  nSlat=nDiPatch=n1Patch=0;

  for(i=0;i<MXphi;i++) 
    for(j=0;j<MXeta;j++)
      en[i][j]=0.;

  for(i=0;i<MXpatch;i++) 
    patchADC[i]=0;


  //access the CTB data  from GEANT
  St_DataSetIter geant(GetDataSet("geant")); 
  St_g2t_ctf_hit *g2t_ctb_hit = (St_g2t_ctf_hit *) geant("g2t_ctb_hit");
  g2t_ctf_hit_st *t = NULL;

  if (g2t_ctb_hit == NULL) { printf("No geant/ctb data set \n"); return ; }
  t = g2t_ctb_hit->GetTable();  assert(t); 
  if (g2t_ctb_hit->GetNRows() == 0) 
    { printf("Empty geant/ctb data set \n"); return ; }

  // accumulate energy per slat , within time gate
  for (i = 0; i < g2t_ctb_hit->GetNRows(); i++,t++){
    float tof_ns=t->tof*1.e9;
    float de_mev=t->de*1000.;
    hctb[0]->Fill(tof_ns);  
    ((TH2*) hctb[10])->Fill(tof_ns,de_mev);
    if(tof_ns > set.CtbTofMax_ns) continue;  

    long iPhi,iEta;
    cts_get_ctb_indexes(t->volume_id,iPhi,iEta);
    iPhi--; iEta--; // change range to [0,N-1]
    if(iPhi < 0 || iPhi>=MXphi || iEta<0 || iEta>=MXphi) {
      printf("ctb_indexes Problem, hit=%d, vol_id=%d, iPhi=%d, iEta=%d, de/MeV=%f\n",i,(int)t->volume_id,(int)iPhi,(int)iEta,t->de*1000);
      assert(0);
    }          
    
    en[iPhi][iEta]+=de_mev;
    // SLAT counting
    if(de_mev < set.CtbDEnThres_mev ) continue;

    nSlat++;
    // add sums in CTBpatch= 1*1.5=eta*phi 
    int kphi=iPhi/15; // makes four patches in phi
    assert(kphi>=0 && kphi<4); // internal error
    if(iEta<2) patchADC[kphi]++; // eta=[-1,0]
    if(iEta>0 && iEta<3) patchADC[kphi+4]++; // eta=[-0.5,0.5]
    if(iEta>1) patchADC[kphi+8]++; // eta=[0,1]  
    hctb[4]->Fill(de_mev);
	
  }  
  
  
  // search for the PAIR of patches opposite in phi (any eta)
  int k;
  for(k=0;k<2;k++) { // up+down or left+right
    int k1=k;
    int k2=k+2;
    int i1,i2;
    for(i1=k1; i1<MXpatch; i1+=4) // over 3 eta-patches
      for(i2=k2; i2<MXpatch; i2+=4) { // over opposite 3 eta-patches
	int sum=patchADC[i1]+patchADC[i2];
	if( nDiPatch< sum) nDiPatch=sum;
	//printf("k=%d, i1=%d, i2=%d, sum=%d, max=%d\n",k,i1,i2,sum,nDiPatch);
      }
  }

  // search for the largest single patche

  for(k=0;k<MXpatch;k++)  
    if( n1Patch< patchADC[k])n1Patch=patchADC[k];
  
  hctb[5]->Fill(float(nSlat)) ;
  printf("trigOnCtb()  nSlat=%d, nDiPatch=%d, n1Patch=%d\n",nSlat, nDiPatch,n1Patch);

  // dump patches content
  for(i=0;i<4;i++) 
    printf("phi=%d-patch vs. eta sum=\t%d \t%d \t%d\n",i,patchADC[i],patchADC[i+4],patchADC[i+8]);  

  return;
}

//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
void StppTrigMaker::trigOnMwc(  int &iforw , int &iback)
{
#if 0
  int i=0, ntrr=0;

  // Read in MWC Tables
  St_DataSet *ds=NULL; 
  St_mwc_raw  *tmwr=NULL;
  mwc_raw_st *tr=NULL;

  ds=GetDataSet(".make/mwc");
  if(ds == NULL) {  printf("No .make/mwc data set \n"); return; }

  tmwr= (St_mwc_raw   *) ds->Find(".data/raw");
  if(tmwr ==NULL) { printf("No mwc/raw table \n"); return; } 
    
  tr= tmwr->GetTable();
  assert(tr); 
  ntrr=tmwr->GetNRows(); 
  if(ntrr ==0) { printf("Empty mwc/raw table \n"); return; } 

  iforw=0;
  iback=0;
  printf("MWC n hit=%d\n",ntrr);
  for (i = 0; i < ntrr ; i++,tr++){
    h5->Fill(float(tr->count));  
    if (tr->sector < 49) {
      //printf("MWC back sec=%d, count=%d\n",tr->sector,tr->count);
      if(tr->count>0)iback+=tr->count;
    } 
    else  {
      //printf("MWC forw sec=%d, count=%d\n",tr->sector,tr->count);
      if(tr->count>0) iforw+=tr->count;
    } 
  }


  h6->Fill(float(iback));
  h7->Fill(float(iforw));
  h25->Fill(float(iforw),float(iback),1.) ;
  return;
#endif
}


//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
int   StppTrigMaker::getGeneratedLP(float &pT, int &nTr1, int &nTr2)
{

 //    G E A N T
  St_DataSet *gds=GetDataSet("geant"); assert(gds);

 St_g2t_track  *gtra=(St_g2t_track  *) gds->Find("g2t_track");
 if(gtra==NULL)printf(" NULL pointer to St_g2t_track table\n");
 assert(gtra);
 
 // select only tracks of interests
 g2t_track_st *GTRA=gtra->GetTable(); assert(GTRA);
 int it;
 pT=0;
 nTr1=0; // all charged prim particle with eta<1.4
 nTr2=0; // MIPS: pi+- with eta<1.4 & pt>0.5 GeV/c
 g2t_track_st *track=NULL;

 // search for the largest two pT values 
 for( it=0; it<gtra->GetNRows();it++,GTRA++) 
   { 
     if(GTRA->eg_label<=0 ) break; //abort on first secondary/pileup  particle
     
     if( fabs(GTRA->charge) <0.5 ) continue; // kill charg=0.
     if( fabs(GTRA->eta) >1.4 ) continue; // kill too large eta
     if( fabs(GTRA->eta) <1. ) { // COUNT PARTICLES in CTB
       nTr1++;
       if((GTRA->ge_pid==8 ||GTRA->ge_pid==9) && GTRA->pt>0.4) nTr2++;
     }
     if(GTRA->pt<pT) continue;
     pT=GTRA->pt;
     track=GTRA;
   } // all track were examined

 if(track==NULL) {printf(" No gen LP found\n"); return -1;}

 printf("getGeneratedLP_inTRIG(), Ntable=%d, found largest pT=%f GeV/c \n",(int)gtra->GetNRows(),pT);
 
  return 0;
}
