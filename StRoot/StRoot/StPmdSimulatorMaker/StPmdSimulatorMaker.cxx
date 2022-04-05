/***************************************************************
 *
 * $Id: StPmdSimulatorMaker.cxx,v 1.14 2017/04/26 19:57:00 perev Exp $
 * Author: Subhasis Chattopadhyay
 ***************************************************************
 *
 * Description: StPmdSimulatorMaker is class for Pmd Simulation
 *
 ****************************************************************
 * changed detector(1) for PMD and detector(0) for CPV while 
 * filling StPmdCollection    : Dipak
 *
 * $Log: StPmdSimulatorMaker.cxx,v $
 * Revision 1.14  2017/04/26 19:57:00  perev
 * Hide m_DataSet
 *
 * Revision 1.13  2007/04/26 04:14:57  perev
 * Remove StBFChain dependency
 *
 * Revision 1.12  2004/09/22 19:24:56  perev
 * Leak fixed + mess with i,j indexes
 *
 * Revision 1.11  2004/06/24 13:52:52  subhasis
 * numbering of det0 and det1 interchanged
 *
 * Revision 1.10  2004/01/26 23:01:49  perev
 * WarnOff
 *
 * Revision 1.9  2003/11/27 12:33:41  subhasis
 * calib constant values updated
 *
 * Revision 1.8  2003/10/23 04:25:29  perev
 * Stiostream again
 *
 * Revision 1.7  2003/10/15 10:40:18  subhasis
 * Changes by Dipak (eg GeV to keV
 *
 * Revision 1.5  2003/08/14 10:37:35 
 * Edep changed from GeV to Kev
 * Proper binning of the histograms added.
 **
 * Revision 1.4  2003/05/12 11:37:35  subhasis
 * Stevent added
 *
 * Revision 1.3  2002/09/09 11:39:08  subhasis
 * warning removed
 *
 * Revision 1.2  2002/09/05 06:21:05  subhasis
 * Calibration and readout resolution added
 *
 ****************************************************************
 ****************************************************************/
#include "Stiostream.h"
#include <assert.h>
#include <math.h>
#include "TROOT.h"
#include <TRandom.h>
#include <TBrowser.h>
#include <StMessMgr.h>
#include "StPmdSimulatorMaker.h"    
//! Library files for StEvent
#include "StPhmdCollection.h"
#include "StPhmdClusterCollection.h"
#include "StPhmdDetector.h"
#include "StPhmdHit.h"
#include "StPhmdModule.h"
     
#include "StPmdUtil/StPmdCollection.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdUtil/StPmdHit.h"
#include "StPmdUtil/StPmdModule.h"
#include "StPmdUtil/StPmdMapUtil.h"
#include "tables/St_g2t_pmd_hit_Table.h" //!for PMD information
#include "tables/St_g2t_track_Table.h"   //!for track information 
#include <TTableSorter.h>
// added for StEvent
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"


ClassImp(StPmdSimulatorMaker)
  
  TDataSet  *geaIn;

St_g2t_pmd_hit *g2t_pmd_hit;   //! hit table for PMD
StPmdGeom *mpmdgeom;   //!StPmdGeom included
StPmdMapUtil *mPmdMapUtil; // Mapping from SM,row,col to Chain Number

//St_g2t_track *track;

StPmdSimulatorMaker::StPmdSimulatorMaker(const char *name):StMaker(name)
{
  //! Default value of parameters for this maker
  adcconstants();
  mpmdgeom=new StPmdGeom();
  mPmdMapUtil= new StPmdMapUtil();
  mPmdCollection=NULL;
  gMessMgr->SetLimit("StPmdSimulator",100);
}

StPmdSimulatorMaker::~StPmdSimulatorMaker() 
{
  delete mpmdgeom;
  delete mPmdMapUtil;
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
  
  mEdepPmd = new TH1F("PmdEdep" ,"Edep for PMD",1000,0.,500.);
  mEdepPmd_part = new TH1F("PmdEdep_part" ,"Edep Part for PMD",1000,0.,1000.);
  mPmdAdc = new TH1F("pmdadc" ,"ADC for PMD",1000, 0.,1000.);
  mHitPmd = new TH1F("PMDHit" ,"Hits for PMD",9000, -0.5,8999.5);
  
  mEdepCpv = new TH1F("CpvEdep" ,"Edep for CPV",400,0.,200.);
  mEdepCpv_part = new TH1F("CpvEdep_part" ,"Edep Part for CPV",200,0.,200.);
  mCpvAdc = new TH1F("cpvadc" ,"ADC for CPV",1000, 0.,1000.);
  mHitCpv = new TH1F("CpvHit" ,"Hits for CPV",5000, -0.5,4999.5);
  
  m_pmdsuper = new TH1F("PmdSuper" ,"Super for PMD",12,0.5,12.5);
  m_pmdrow = new TH2F("PMD_SupervsRow" ,"Pmd super vs row",12,0.5,12.5,100,0.5,100.5);
  m_pmdcol = new TH2F("PMD_SupervsCol" ,"Pmd super vs col",12,0.5,12.5,100,0.5,100.5);

  m_cpvsuper = new TH1F("CpvSuper" ,"Super for CPV",12,0.5,12.5);
  m_cpvrow = new TH2F("CPV_SupervsRow" ,"Cpv super vs row",12,0.5,12.5,100,0.5,100.5);
  m_cpvcol = new TH2F("CPV_SupervsCol" ,"Cpv super vs col",12,0.5,12.5,100,0.5,100.5);

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
  
  if ( g2t_pmd_hit && g2t_pmd_hit->GetTableSize()>0)
    { // O"k
    
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
  AddData(mPmdCollection);
  StPmdDetector* det0 = mPmdCollection->detector(1); //! Filling detector(1) for 'PMD' Collection
  StPmdDetector* det1 = mPmdCollection->detector(0); //! Filling detector(0) for 'CPV' Collection
  
  g2t_pmd_hit_st *hit = g2t_pmd_hit->GetTable();
  Int_t nhits         = g2t_pmd_hit->GetNRows();
  
  //  g2t_track_st *tra = track->GetTable();
  
  if(nhits<=0)
    {
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
    
    if(decode==kStOK)
      {
	StPmdHit *phit = new StPmdHit();
	phit->setGsuper(Int_t(gsuper));       //! filling supermodule no 1-12
	phit->setSubDetector(Int_t(subdet));  //! filling subdetector PMD=1 & CPV =2
	phit->setRow(Int_t(row));             //! filling row 1-72
	phit->setColumn(Int_t(col));          //! filling col 1-96
	phit->setEdep((hit->de)*1000000.);    //! filling energy in keV
	
//VP	Int_t ChainNo;
	Float_t xPMD,yPMD;
	if(subdet==1){   //! Filling Hit Collection for PMD
	  
	  /**** Switch OFF the chains which were not working in RUN-4 *****/
	  /*
	    mPmdMapUtil->ChainNumber(gsuper+12,row,col,ChainNo);
	    Int_t BeamEnergy;
	    BeamEnergy = 0; //Default setting
	    switch(BeamEnergy)
	    {
	    case 1:    //For 62 GeV data
	    if(ChainNo == 25 || ChainNo == 27 || ChainNo == 38 || ChainNo == 39 || ChainNo == 40 ||
	    ChainNo ==45 || ChainNo == 47 || ChainNo == 48)
	    {
	    hit->de =0.;
	    phit->setEdep(0.);     //Putting the cell Edep as Zero
	    break;
	    }
	    }
	  */
	  
	  if(det0->module_hit(Int_t(gsuper))==0)
	    {
	      det0->addHit(phit);
	    }
	  else
	    {
	      //if phit already exist
	      StPmdHit *hitmatched=Exist(phit,det0,Int_t(gsuper));
	      if(hitmatched)
		{
		  Float_t newEdep=hitmatched->Edep()+(hit->de)*1000000.;
		  hitmatched->setEdep(newEdep);     //!   energy in keV
		  delete phit;
		}
	      else 
		{
		  det0->addHit(phit);
		}
	    }
	  
	  m_pmdEdep2D->Fill(xPMD,yPMD,hit->de);
	  //! Filling basic hists for Hits
	  m_pmdsuper->Fill(Float_t(gsuper));
	  m_pmdrow->Fill(Float_t(gsuper),Float_t(row));
	  m_pmdcol->Fill(Float_t(gsuper),Float_t(col));
	}
	
	if(subdet==2){      //! Filling Hit Collection for CPV
	  if(det1->module_hit(Int_t(gsuper))==0)
	    {
	      det1->addHit(phit);}
	  else
	    {
	      //if phit already exist
	      StPmdHit *hitmatched=Exist(phit,det1,Int_t(gsuper));
	      if(hitmatched)
		{
		  Float_t newEdep=hitmatched->Edep()+(hit->de)*1000000.;
		  hitmatched->setEdep(newEdep);   //!  Edep in keV
		  delete phit;
		}
	      else 
		{
		  det1->addHit(phit);
		}
	    }
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
  
  
  if(det0)
    {
      for(Int_t ii=1;ii<13;ii++)
	{
	  FinalEdep(det0,ii);
	}
    }
  if(det1)
    {
      for(Int_t ii=1;ii<13;ii++)
	{
	  FinalEdep(det1,ii); 
	}
    }
  
  for(Int_t ii=1;ii<13;ii++)
    {
      FillHistograms(det0,det1,ii);
    }
  //!Filling StEvent for PMD and CPV
  fillStEvent(det0,det1);
  
  return kStOK;
} //! end of makePmdHit

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
      if(spmcl)
	{
	  ypad=spmcl->Row();
	  xpad=spmcl->Column();
	  super = spmcl->Gsuper();
	  if(phit->Row()==ypad && phit->Column()==xpad && phit->Gsuper() == super)   return spmcl;
	}
    }
  return NULL;
}


void StPmdSimulatorMaker::FillHistograms(StPmdDetector* pmd_det,StPmdDetector* cpv_det,Int_t id)
{
  StPmdModule* pmd_mod=pmd_det->module(id);
  Int_t nmh1=pmd_det->module_hit(id);
  Float_t edep_part = 0.;
  if(nmh1>0){
    TIter next(pmd_mod->Hits());
    StPmdHit *spmcl1;   //! pointer for hits
    for(Int_t im=0; im<nmh1; im++)
      {
	spmcl1 = (StPmdHit*)next();
	if(spmcl1)
	  {
	    Int_t gsuper = spmcl1->Gsuper();     //! supermodule 1-12
	    Int_t row=spmcl1->Row();             //! row of the hit 1-72
	    Int_t col=spmcl1->Column();          //! column of the hit 1-96
	    Float_t edep=spmcl1->Edep();         //! edep in keV
	    Int_t adc=spmcl1->Adc();             //! Adc
	    
	    if(edep>0.)mEdepPmd->Fill(edep);
	    if(edep>0.) edep_part = edep_part+edep;
	    mPmdAdc->Fill(Float_t(adc));
	    Float_t xPMD,yPMD,etaPMD,phiPMD;
	    mpmdgeom->DetCell_xy(gsuper,Int_t(row),Int_t(col),xPMD,yPMD,etaPMD,phiPMD);
	    m_pmdEdep2D->Fill(xPMD,yPMD,edep);
	    m_pmdsuper->Fill(Float_t(gsuper));
	    m_pmdrow->Fill(Float_t(gsuper),Float_t(row));
	    m_pmdcol->Fill(Float_t(gsuper),Float_t(col));
	  }
      }
  }
  if(nmh1>0)mHitPmd->Fill(nmh1);
  
  if(edep_part>0) mEdepPmd_part->Fill(edep_part);
  
  
  StPmdModule* cpv_mod=cpv_det->module(id);
  edep_part = 0.;
  Int_t nmh2=cpv_det->module_hit(id);
  if(nmh2>0){
    TIter next(cpv_mod->Hits());
    StPmdHit *spmcl2;   //! pointer for hits
    
    for(Int_t im=0; im<nmh2; im++)
      {
	
	spmcl2 = (StPmdHit*)next();
	if(spmcl2){
	  Int_t gsuper = spmcl2->Gsuper();   //! supermodule 1-12
	  Int_t row=spmcl2->Row();           //! row of the hit 1-72
	  Int_t col=spmcl2->Column();        //! column of the hit 1-96
	  Float_t edep=spmcl2->Edep();       //! edep in keV
	  
	  Float_t xCPV,yCPV,etaCPV,phiCPV;
	  mpmdgeom->DetCell_xy(gsuper,Int_t(row),Int_t(col),xCPV,yCPV,etaCPV,phiCPV);		
	
	  mEdepCpv->Fill(edep);
          if(edep>0.4) edep_part = edep_part+edep;
	  
	  mpmdgeom->DetCell_xy(gsuper,Int_t(row),Int_t(col),xCPV,yCPV,etaCPV,phiCPV);

	  m_cpvEdep2D->Fill(xCPV,yCPV,edep);
	  m_cpvsuper->Fill(Float_t(gsuper));
	  m_cpvrow->Fill(Float_t(gsuper),Float_t(row));
	  m_cpvcol->Fill(Float_t(gsuper),Float_t(col));
	}
      }
  }
  if(nmh2>0)mHitCpv->Fill(nmh2);
  
  if(edep_part>0) mEdepCpv_part->Fill(edep_part);
}



Int_t StPmdSimulatorMaker::fillStEvent(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
{
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  mevtPmdCollection = new StPhmdCollection();
  currevent->setPhmdCollection(mevtPmdCollection);
  
  StPhmdDetector* evtdet0 = mevtPmdCollection->detector(StDetectorId(kPhmdId)); //!DetectorId = 26 for PMD in StEvent
  StPhmdDetector* evtdet1 = mevtPmdCollection->detector(StDetectorId(kPhmdCpvId)); //! DetectorId = 25 for CPV in StEvent
  
  
  for(Int_t id=1;id<13;id++)
    {
      
      //    Int_t hitpmd=0, hitcpv=0;
      Int_t subdet=1;
      //Filling StEvent for PMD
      
      StPmdModule * pmd_mod=pmd_det->module(id);  
      Int_t nmh1=pmd_det->module_hit(id);   
      if(nmh1>0)
	{
	  TIter next(pmd_mod->Hits());
	  StPmdHit *spmcl1;  
	  for(Int_t im=0; im<nmh1; im++)
	    {
	      spmcl1 = (StPmdHit*)next();
	      if(spmcl1)
		{
		  Int_t gsuper=spmcl1->Gsuper();
		  Int_t col=spmcl1->Column();
		  Int_t row=spmcl1->Row();
		  Float_t edep=spmcl1->Edep();         
		  Int_t adc=spmcl1->Adc();
		  //! Filling PmdHit for StEvent 
		  StPhmdHit *phit = new StPhmdHit();
		  phit->setSuperModule(Int_t(gsuper-1));     // filling supermodule no (range 0-11)
		  phit->setSubDetector(Int_t(subdet)); // filling subdetector PMD = 1 and CPV = 2
		  phit->setRow(Int_t(row));            // filling row 1-72
		  phit->setColumn(Int_t(col));         // filling col 1-96
		  phit->setEnergy(edep);              // filling energy in KeV  
		  phit->setAdc(adc);              // filling energy(ADC)   
		  evtdet0->addHit(phit);
		}
	    }
	}
      
      // Filling StEvent for  CPV 
      StPmdModule * cpv_mod=cpv_det->module(id);
      Int_t nmh2=cpv_det->module_hit(id);     
      if(nmh2>0)
	{
	  subdet=2;
	  TIter next(cpv_mod->Hits());
	  StPmdHit *spmcl2;  
	  for(Int_t im=0; im<nmh2; im++)
	    {
	      spmcl2 = (StPmdHit*)next();
	      if(spmcl2)
		{
		  Int_t gsuper=spmcl2->Gsuper();
		  Int_t col=spmcl2->Column();
		  Int_t row=spmcl2->Row();
		  Float_t edep=spmcl2->Edep();         
		  Int_t adc=spmcl2->Adc();
		  //! Filling PmdHit  for CPV in StEvent 
		  StPhmdHit *phit = new StPhmdHit();
		  phit->setSuperModule(Int_t(gsuper-1)); // filling supermodule no (0-11)
		  phit->setSubDetector(Int_t(subdet)); // filling subdetector PMD=1 and CPV=2
		  phit->setRow(Int_t(row));            // filling row 1-72
		  phit->setColumn(Int_t(col));         // filling col 1-96
		  phit->setEnergy(edep);              // filling energy in KeV  
		  phit->setAdc(adc);              // filling energy(ADC)   
		  evtdet1->addHit(phit);
		}
	    }
	}  
    }
  
  return kStOK;
}

void StPmdSimulatorMaker::FinalEdep(StPmdDetector* pdet,Int_t id)
{
  
  StPmdModule * mod=pdet->module(id);  
  Int_t nmh=pdet->module_hit(id);   
  if(nmh>0)
    {
      TIter next(mod->Hits());
      StPmdHit *spmcl;  
      for(Int_t im=0; im<nmh; im++)
	{
	  Float_t rawadc=0.;
	  //Float_t Edep=0.;
	  Int_t ADC=0;
	  spmcl = (StPmdHit*)next();
	  if(spmcl)
	    {
	      Float_t rawedep=spmcl->Edep();         
	      Float_t keVedep=rawedep;
	      keV_ADC(keVedep,rawadc);
	      if(mResFlag) ADC_Readout(rawadc,ADC);
	      else ADC = (int)rawadc;
	      //commented because of saturation problem in Edep

	      //	  mpmdgeom->ADC2Edep(ADC,Edep); //Again changeing back to Edep
	      //	  spmcl->setEdep(Edep);
	      spmcl->setAdc(ADC);
	    }
	}
    }
}

Float_t StPmdSimulatorMaker::keV_ADC(Float_t edep, Float_t& adc)
{
  adc=mlcon0 + mlcon1*edep + mlcon2*pow(edep,2);
  return kStOK;
}

Float_t StPmdSimulatorMaker::ADC_Readout(Float_t adc,Int_t& ADC)
{
  Float_t reso_percent=0., reso=0.;
  reso_percent=mpcon0 + mpcon1*adc + mpcon2*pow(adc,2);
  reso=(reso_percent*100.)/adc;
  
  Float_t adcprime=gRandom->Gaus(adc,reso);
  if(adcprime<0)adcprime=0;
  ADC=Int_t(adcprime);
  return kStOK;
}







