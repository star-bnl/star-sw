/***************************************************************
 *
 * $Id: StPmdSimulatorMaker.cxx,v 1.1 2002/08/27 12:03:26 subhasis Exp $
 * Author: Subhasis Chattopadhyay
 ***************************************************************
 *
 * Description: StPmdSimulatorMaker is class for Pmd Simulation
 *
 ****************************************************************
 * $Log: StPmdSimulatorMaker.cxx,v $
 * Revision 1.1  2002/08/27 12:03:26  subhasis
 * First Version
 *
 ****************************************************************/
#include <iostream.h>
#include <assert.h>
#include <math.h>
#include "TROOT.h"
#include <TRandom.h>
#include <TBrowser.h>
#include <StMessMgr.h>
#include "StPmdSimulatorMaker.h"         
#include "StPmdUtil/StPmdCollection.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdUtil/StPmdHit.h"
#include "StPmdUtil/StPmdModule.h"
#include "StBFChain.h"
#include "tables/St_g2t_pmd_hit_Table.h" //!for PMD information
#include "tables/St_g2t_track_Table.h"   //!for track information 
#include <TTableSorter.h>

ClassImp(StPmdSimulatorMaker)

TDataSet  *geaIn;


St_g2t_pmd_hit *g2t_pmd_hit;
StPmdGeom *mpmdgeom;
//St_g2t_track *track;

StPmdSimulatorMaker::StPmdSimulatorMaker(const char *name):StMaker(name)
{
 //! Default value of parameters for this maker
   mpmdgeom=new StPmdGeom();
   mPmdCollection=NULL;
   gMessMgr->SetLimit("StPmdSimulator",100);
}

StPmdSimulatorMaker::~StPmdSimulatorMaker() 
{
	delete mpmdgeom;
}

Int_t StPmdSimulatorMaker::Init()
{
  bookHistograms();
  return StMaker::Init();
}
void StPmdSimulatorMaker::bookHistograms()
{
 m_pmdEdep2D = new TH2F("PMDEdep2D" ,"PMD Edep (2D)",150,-150.,150.,150,-150.,150.);
 m_cpvEdep2D = new TH2F("CPVEdep2D" ,"CPV Edep (2D)",150,-150.,150.,150,-150.,150.);

 mEdepPmd = new TH1F("PmdEdep" ,"Edep for PMD",1000,0.,0.004);
 mHitPmd = new TH1F("PMDHit" ,"Hits for PMD",100, 0.,5000.);
 mEdepCpv = new TH1F("CpvEdep" ,"Edep for CPV",500,0.,0.00004);
 mHitCpv = new TH1F("CpvHit" ,"Hits for CPV",100, 0.,5000);
 m_pmdsuper = new TH1F("PmdSuper" ,"Super for PMD",20,0.5,20.5);
 m_pmdrow = new TH2F("PMD_SupervsRow" ,"Pmd super vs row",20,0.5,20.5,100,0.5,100.5);
 m_pmdcol = new TH2F("PMD_SupervsCol" ,"Pmd super vs col",20,0.5,20.5,100,0.5,100.5);
 m_cpvsuper = new TH1F("CpvSuper" ,"Super for CPV",20,0.5,20.5);
 m_cpvrow = new TH2F("CPV_SupervsRow" ,"Cpv super vs row",20,0.5,20.5,100,0.5,100.5);
 m_cpvcol = new TH2F("CPV_SupervsCol" ,"Cpv super vs col",20,0.5,20.5,100,0.5,100.5);

}


Int_t StPmdSimulatorMaker::Make()
{
  //!  Find  Geant  Tables
  geaIn = GetDataSet("geant");      //! Input from fzin file
  if (geaIn == 0) {
    geaIn = GetDataSet("event/geant/Event"); //! Input from xdf file
    if (geaIn == 0) {
      gMessMgr->Error()<<"Geant Data didn't find in event/geant/Event and geant directories"<<endm;
      return kStWarn;
    }
  }

  Int_t retpmd;
  retpmd = GetPmd();   //! Get Pmd Hits from Geant and fill hits.
  return retpmd;
}

Int_t StPmdSimulatorMaker::GetPmd()
{
  g2t_pmd_hit = (St_g2t_pmd_hit *) geaIn->Find("g2t_pmd_hit");   //! Get g2t table 
  //  track = (St_g2t_track *) geaIn->Find("g2t_track");

  if ( g2t_pmd_hit && g2t_pmd_hit->GetTableSize()>0){ // O"k

   Int_t phit=makePmdHits();
  if(phit!=kStOK){cout<<"problem in Hit formation"<<endl;return kStWarn;}
  }
  else gMessMgr->Warning()<<
  "StPmdSimulatorMaker::makePmd=>table g2t_PMD_hit isn't found in dataset or size is 0 "<< 
       geaIn->GetName()<<endm;
  return kStOK;
}

////////////////////////////////////////////////
Int_t StPmdSimulatorMaker::makePmdHits()
{
  /*! Create PmdCollection, and add that to the dataset, so that it is avialable to
   *  other makers */

    mPmdCollection = new StPmdCollection("PmdCollection");
    m_DataSet->Add(mPmdCollection);
   StPmdDetector* det0 = mPmdCollection->detector(0);
   StPmdDetector* det1 = mPmdCollection->detector(1);

Float_t ecpv=0;
Float_t epmd=0.;
Float_t hitcpv=0;
Float_t hitpmd=0;

  g2t_pmd_hit_st *hit = g2t_pmd_hit->GetTable();
  Int_t nhits         = g2t_pmd_hit->GetNRows();

  //  g2t_track_st *tra = track->GetTable();

 if(nhits<=0)
 {
   cout<<" No hits fro PMD, go out"<<endl;
	 return kStWarn;
 }
for(Int_t ihit=0; ihit<nhits; ihit++,hit++) { 
  Int_t sector=0,super=0,subdet=0,row=0,col=0;
  Int_t gsuper;
  //! Decoding volume_id to sector,super,subdet,row,col

  Int_t decode=Decode_VolId(hit->volume_id,sector,super,subdet,row,col);

  //! converts sector,super to global supermodule number 17
  mpmdgeom->NModule(Int_t(sector),Int_t(super),gsuper);

  //! converts super 17 to super 12 (i.e SM no in 12 convention)

  mpmdgeom->Sim2Detmap(gsuper,row,col);

  //!Create PmdHits and Fill it in StPmdHit

  if(decode==kStOK){
  StPmdHit *phit = new StPmdHit();
  phit->setGsuper(Int_t(gsuper));     //! filling supermodule no
  phit->setSubDetector(Int_t(subdet)); //! filling subdetector
  phit->setRow(Int_t(row));            //! filling row
  phit->setColumn(Int_t(col));         //! filling col
  phit->setEdep(hit->de);              //! filling energy   

  if(subdet==1){   //! for PMD

    if(det0->module_hit(Int_t(gsuper))==0){
      det0->addHit(phit);}
     else
    {
//if phit already exist
    StPmdHit *hitmatched=Exist(phit,det0,Int_t(gsuper));
    if(hitmatched){
      Float_t newEdep=hitmatched->Edep()+hit->de;
         hitmatched->setEdep(newEdep);
         delete phit;
      }
     else {det0->addHit(phit);}
   }
   epmd+=hit->de;
   hitpmd++;


//! Fill 2D hist after converting SM,row,col into x,y
  Float_t xPMD,yPMD,etaPMD,phiPMD;
  mpmdgeom->DetCell_xy(gsuper,Int_t(row),Int_t(col),xPMD,yPMD,etaPMD,phiPMD);
  m_pmdEdep2D->Fill(xPMD,yPMD,hit->de);
//! Filling basic hists for Hits
  m_pmdsuper->Fill(Float_t(gsuper));
  m_pmdrow->Fill(Float_t(gsuper),Float_t(row));
  m_pmdcol->Fill(Float_t(gsuper),Float_t(col));
  }

  if(subdet==2){      //! for CPV
    if(det1->module_hit(Int_t(gsuper))==0){
      det1->addHit(phit);}
     else
    {
//if phit already exist
    StPmdHit *hitmatched=Exist(phit,det1,Int_t(gsuper));
    if(hitmatched){
      Float_t newEdep=hitmatched->Edep()+hit->de;
         hitmatched->setEdep(newEdep);
         delete phit;
      }
     else {det1->addHit(phit);}
   }
   ecpv+=hit->de;
   hitcpv++;


//! Fill 2D hist after converting SM,row,col into x,y
  Float_t xCPV,yCPV,etaCPV,phiCPV;
  mpmdgeom->DetCell_xy(gsuper,Int_t(row),Int_t(col),xCPV,yCPV,etaCPV,phiCPV);
  m_cpvEdep2D->Fill(xCPV,yCPV,hit->de);
//! Filling basic hists for Hits
  m_cpvsuper->Fill(Float_t(gsuper));
  m_cpvrow->Fill(Float_t(gsuper),Float_t(row));
  m_cpvcol->Fill(Float_t(gsuper),Float_t(col));

  }
  }
}
 Int_t n0=0,n1=0;
 for(Int_t ii=1;ii<13;ii++){
   n0 += det0->module_hit(ii);
   n1 += det1->module_hit(ii);
}
cout<<"n0  "<<n0<<"n1  "<<n1<<endl;
//! Filling the hist for total no of hits
cout<<"epmd,ecpv **"<<epmd<<" "<<ecpv<<endl;

if(hitpmd>0){
	mHitPmd->Fill(hitpmd);
	mEdepPmd->Fill(epmd);
}
if(hitcpv>0){
	mHitCpv->Fill(hitcpv);
	mEdepCpv->Fill(ecpv);
}
  
return kStOK;
}

void StPmdSimulatorMaker::Browse(TBrowser* b)
{
//  if(mPmdCollection) b->Add((TDataSet*)mPmdCollection);
  TDataSet::Browse(b);
}

//! decoding volume_id

Int_t StPmdSimulatorMaker::Decode_VolId(Int_t& vol, Int_t& sector, Int_t& super,Int_t& subdet, Int_t& row, Int_t& col)
 {

     Int_t temp0=vol%1000000;
     Int_t temp1=temp0%100000;
     Int_t temp2=temp1%10000;
     Int_t temp3=temp2%100;
     
      col=temp3;
      row=temp2/100;  
      subdet=temp1/10000;
      super=temp0/100000;
      sector=vol/1000000;

 return kStOK;

}

StPmdHit* StPmdSimulatorMaker::Exist(StPmdHit* phit,StPmdDetector* pdet,Int_t id)
  {
       Int_t xpad,ypad,super;
        Int_t nmh=pdet->module_hit(id);
        StPmdModule * pmod=pdet->module(id);
        TIter next(pmod->Hits());
        StPmdHit *spmcl;
// Loop over hits for each SM
        for(Int_t im=0; im<nmh; im++)
          {
            spmcl = (StPmdHit*)next();
            if(spmcl){
              ypad=spmcl->Row();
              xpad=spmcl->Column();
              super = spmcl->Gsuper();
   if(phit->Row()==ypad && phit->Column()==xpad && phit->Gsuper() == super)   return spmcl;
          }
       }
    return NULL;
}





