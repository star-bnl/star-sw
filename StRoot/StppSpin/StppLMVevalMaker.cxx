// *-- Author : Jan Balewski 
// $Id: StppLMVevalMaker.cxx,v 1.5 2001/04/24 21:58:26 balewski Exp $
// $Log: StppLMVevalMaker.cxx,v $
// Revision 1.5  2001/04/24 21:58:26  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/24 15:54:49  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/23 21:55:52  balewski
// *** empty log message ***
//
// Revision 1.2  2001/04/23 21:54:24  balewski
// *** empty log message ***
//
// Revision 1.1  2001/04/23 21:47:17  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/23 15:02:11  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                     
//  evaluates ppLMV
// 
//                                                                     
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include <math.h>
#include <strings.h>
#include <stdio.h>
#include <stdlib.h> 

#include "StppLMVevalMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"


#include "TH1.h"
#include "TH2.h"

// for gen LP
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"

int aa;

ClassImp(StppLMVevalMaker)

//_____________________________________________________________________________
//_____________________________________________________________________________
StppLMVevalMaker::StppLMVevalMaker(const char *name):StMaker(name){
 //  const char *name -  the name of this constructor
  printf("CCCCCCCCCCCCCCC Constructor of class=%s= executed\n", name);
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
StppLMVevalMaker::~StppLMVevalMaker(){  
//  printf(" DDDDDDDDDDDDDD Destructor executed\n");
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppLMVevalMaker::Init(){
  //  Init - empty

  //  printf(" ppTR add=%d\n",(int)&decision);

  printf("InInInInInInInInInIn    Initialization start \"%s\",  m_Mode=%d... \n", GetName(),m_Mode);


  he[0]=new TH1F("ge0","generated (all) NchG",50,-0.5,49.5);
  he[1]=new TH1F("ge1","any vertex vs. NchG",50,-0.5,49.5);
  he[2]=new TH1F("ge2","good vertex vs. NchG",50,-0.5,49.5);

  he[3]=new TH1F("dz","error of any vertex",100,-5.,5.);

  he[4] =(TH1F*)new TH2F("2dz","dz vs. NchG",50,-0.5,49.5,50,-3.,3.);
  he[5] =(TH1F*)new TH2F("2dr","dRxy vs. NchG",50,-0.5,49.5,50,.0,6.);

  he[6] =(TH1F*)new TH2F("2dz","dz vs. zG",50,-0.5,49.5,50,-25.,35.);

  // h1 = new TH1F("trg_out","Trigger decision",10, -1.5, 8.5);
  //h1->SetXTitle(" trigger ") ;
  //h1->SetYTitle(" Number of events ") ;

  //
  printf("InInInInInInInInInIn Initialization \"%s\"end\n",GetName());
  
  return StMaker::Init();
}

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________
Int_t StppLMVevalMaker::Make(){ 
  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;

  int NchG=-1;

  //    G E A N T
  St_DataSet *gds=GetDataSet("geant"); assert(gds);
  St_g2t_vertex  *gver=(St_g2t_vertex  *) gds->Find("g2t_vertex");
  if(gver==NULL){printf(" NULL pointer to St_g2t_vertext table\n");assert(gver);}
  if(gver->GetNRows()<=0){printf(" empty St_g2t_vertext table\n"); assert(0);}
  g2t_vertex_st *GVER=gver->GetTable(); assert(GVER);
  float x_true=GVER->ge_x[0];
  float y_true=GVER->ge_x[1];
  float z_true=GVER->ge_x[2];
  //float rxy_true=sqrt(GVER->ge_x[0]*GVER->ge_x[0] + GVER->ge_x[1]*GVER->ge_x[1]);
  
 
  getGenerated(NchG);

  StEvent *stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  printf(" eveID=%d Geant vertex =%f %f %f\n",(int)stEvent->id(),GVER->ge_x[0],GVER->ge_x[1],GVER->ge_x[2]);

  StPrimaryVertex* primV=NULL;
  primV=stEvent->primaryVertex();
   if(!primV)
    printf("primaryVertex()=NULL\n");
  else {
    printf("primaryVertex()= %f, %f %f, nTracks=%d\n",primV->position().x(),primV->position().y(),primV->position().z(),primV->numberOfDaughters());
  }

  //     H I S T O G R A M M I N G

  he[0]->Fill(NchG);
  if(primV) {
    he[1]->Fill(NchG);
    float dz=z_true-primV->position().z();
    float dx=x_true-primV->position().x();
    float dy=y_true-primV->position().y();
    float drxy=sqrt(dx*dx+dy*dy);
    if(fabs(dz)<1.5 && drxy<1.5) he[2]->Fill(NchG);
    he[3]->Fill(dz);
    ((TH2*) he[4])->Fill(NchG,dz);
    ((TH2*) he[5])->Fill(NchG,drxy);
    ((TH2*) he[6])->Fill(z_true,dz);

  }
    


 
  return kStOK;
}

//------------------------------------------------------------------------
  Int_t StppLMVevalMaker::Finish()
{

  /*  
 Float_t *p=h1->GetArray();

printf("\n \"%s\" summary: Total number of events  %d\n # of events of type: A0=%d, A1=%d, A2=%d, A3=%d\n\n",GetName(), (int)p[1], (int)p[2], (int)p[3], (int)p[4], (int)p[5]);
  */
 return kStOK;

}
//------------------------------------------------------------------------
//------------------------------------------------------------------------
//------------------------------------------------------------------------
void   StppLMVevalMaker::getGenerated( int &Nch)
{

 //    G E A N T
  St_DataSet *gds=GetDataSet("geant"); assert(gds);

 St_g2t_track  *gtra=(St_g2t_track  *) gds->Find("g2t_track");
 if(gtra==NULL)printf(" NULL pointer to St_g2t_track table\n");
 assert(gtra);
 
 // select only tracks of interests
 g2t_track_st *GTRA=gtra->GetTable(); assert(GTRA);
 int it;
 int nTr2=0; // MIPS: pi+- & K+- with eta<1.4 & pt>0.2 GeV/c
 //g2t_track_st *track=NULL;

 // search for the largest two pT values 
 for( it=0; it<gtra->GetNRows();it++,GTRA++) 
   { 
     if(GTRA->eg_label<=0 ) break; //abort on first secondary/pileup  particle
     
     if( fabs(GTRA->charge) <0.5 ) continue; // kill charg=0.
     if( fabs(GTRA->eta) >1.4 ) continue; // kill too large eta
     if( GTRA->pt<0.2) continue; // kill too low pT
     if(GTRA->ge_pid==8 || GTRA->ge_pid==9|| GTRA->ge_pid==11|| GTRA->ge_pid==12) nTr2++;
   } // all track were examined
 
 Nch=nTr2;
 printf("getGenerated(), Ntable=%d, found Nch=%d \n",(int)gtra->GetNRows(),Nch);
 
  return ;
}



