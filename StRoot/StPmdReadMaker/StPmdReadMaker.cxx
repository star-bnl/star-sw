/***************************************************************************
 *$Id: StPmdReadMaker.cxx,v 1.19 2007/07/12 19:52:05 fisyak Exp $
 *
 * StPmdReadMaker
 *
 * Author:  Supriya Das and Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: Reading PMD data and filling hits for StEvent
 **************************************************************************
 *$Log: StPmdReadMaker.cxx,v $
 *Revision 1.19  2007/07/12 19:52:05  fisyak
 *Add includes for ROOT 5.16
 *
 *Revision 1.18  2007/04/28 17:56:37  perev
 *Redundant StChain.h removed
 *
 *Revision 1.17  2007/04/26 04:14:27  perev
 *Remove StBFChain dependency
 *
 *Revision 1.16  2006/02/16 21:36:08  perev
 *No delete pmdCollection. It is deleted in Clear()
 *
 *Revision 1.15  2005/12/07 19:46:39  perev
 *EndCrashFix
 *
 *Revision 1.14  2005/01/27 13:08:51  subhasis
 *chaged to read 2005 data
 *
 *Revision 1.13  2004/09/22 19:24:56  perev
 *Leak fixed + mess with i,j indexes
 *
 *Revision 1.12  2004/07/12 14:45:08  subhasis
 *QA hist added
 *
 *Revision 1.11  2004/07/09 12:23:03  subhasis
 *numbering scheme adopted on CPV
 *
 *Revision 1.10  2004/07/09 10:40:09  subhasis
 *channel 0 ADC made to 0, numbering scheme implemented in fillStEvent, earlier it was done on StPmdHit, in stead of StPhmdHit
 *
 *Revision 1.9  2004/07/09 09:01:13  subhasis
 *numbering convention starts from 0 everywhere for filling StEvent
 *
 *Revision 1.8  2004/06/25 10:38:00  subhasis
 *vmecond bug fixed for 200 geV
 *
 *Revision 1.7  2004/04/14 15:40:41  subhasis
 *chainno 45,46 interchanged (in hardware) issue fixed
 *
 *Revision 1.6  2004/03/23 08:52:22  subhasis
 *several changes (Board Detail by hand etc) for first production
 *
 *Revision 1.5  2004/03/12 06:36:57  subhasis
 *fillStEvent argument orders done properly
 *
 *Revision 1.4  2004/03/11 11:29:46  subhasis
 *Changes made for PMD run config
 *
 *Revision 1.3  2003/12/03 11:50:29  subhasis
 *Comment header changed by Supriya
 *
 ***************************************************************************
 */

//#include <Stiostream.h>
#include "Stsstream.h"

#include "StPmdReadMaker.h"
#include "St_DataSetIter.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDbConfigNode.hh"

#include "StGlobals.hh"
#include "StPmdUtil/StPmdCollection.h"
#include "StPmdUtil/StPmdDetector.h"
#include "StPmdUtil/StPmdHit.h"
#include "StPmdUtil/StPmdModule.h"
#include "StPmdUtil/StPmdGeom.h"
#include "StPmdUtil/StPmdDBUtil.h"
#include<StMessMgr.h>
//
// Interfaces
//
// DAQ Libraries
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/PMD/PMD_Reader.hh"
#include "StDAQMaker/StDAQReader.h"

// added for StEvent
#include "StEvent/StEvent.h"
#include "StEvent/StEventTypes.h"
//
ClassImp(StPmdReadMaker) // macro
//-----------------------------------------------------------------
  
StPmdReadMaker::StPmdReadMaker(const char *name)
    : StMaker(name)
{
  mThePmdReader=NULL;
  mEvtPmdCollection=NULL;
  mPmdCollection=NULL;
  mTheDataReader=NULL;//!
  mThePmdData=NULL;//!
  mPmdEvent=NULL;  
  mCpvEvent=NULL;  
  mDb=NULL;
  m_PmdCalibConst=NULL;
  mPmdGeom = new StPmdGeom();
  mPmdDBUtil = new StPmdDBUtil();
  mChainTh=0;
  mCalibFlag=kFALSE;
  mPmdPrint=kFALSE;
  
}

//-----------------------------------------------------------------

StPmdReadMaker::~StPmdReadMaker() {
  delete mPmdGeom;	mPmdGeom	=0;
  delete mPmdDBUtil;	mPmdDBUtil	=0;
  mThePmdReader	=0;
  mTheDataReader=0;
  mThePmdData	=0;
  mPmdEvent	=0;
  mCpvEvent	=0;
  mPmdCollection=0;
  mDb=0;
  m_PmdCalibConst=0;
}

//-----------------------------------------------------------------

Int_t StPmdReadMaker::Init() {
  if(mPmdPrint)gMessMgr->Info("StPmdReadMaker::Init()");
  bookHist();

  return StMaker::Init();
}
//-----------------------------------------------------------------
Int_t StPmdReadMaker::InitRun(Int_t runnr) {
  if(mPmdPrint)gMessMgr->Info("StPmdReaderMaker::InitRun()");
  
  mRunNumber = runnr;

  if(mRunNumber < 5034042) mVmeCond = 1;
  else if(mRunNumber >= 5034042 && mRunNumber < 5049020) mVmeCond = 2;
  else if(mRunNumber >= 5049020 && mRunNumber < 6000000) mVmeCond = 3;
  else mVmeCond = 4;

 if(mPmdPrint) cout<<"Run Number, VME Condition : "<<mRunNumber<<" "<<mVmeCond<<endl;
					 
  ReadCalibrationsConst();
 
  return StMaker::InitRun(runnr);
}

//------------------------------------------------------------------
Int_t StPmdReadMaker::Make() {
  if(mPmdPrint)gMessMgr->Info("StPmdReadMaker::Make()");

  mThePmdData   = GetDataSet("StDAQReader");
  
  if(!mThePmdData) {
    gMessMgr->Info("StPmdReadMaker::Make()");
    gMessMgr->Info("DataSet: StDAQReader not there()");
    gMessMgr->Info("Skip this event");
    return kStWarn;
  }
  else{gMessMgr->Info("GOT DaqReader dataset");}
  
  mTheDataReader = (StDAQReader*)(mThePmdData->GetObject());
  if(!mTheDataReader) {
    gMessMgr->Info("StPmdReadMaker::Make()");
    gMessMgr->Info("DataSet: PMDData not there()");
    gMessMgr->Info("Skip this event");
    return kStWarn;
  }
  else{gMessMgr->Info("GOT DaqReader object, look for PMD**");}
 
  if ( !(mTheDataReader->PMDPresent())) {
    cout << "StPmdreaderMaker::Maker()\n";
    cout << "\tPMD  not in datastream\n";
    cout << "\tSkip this event\n" << endl;
    return kStWarn;
  }
  else{cout<<"PMD present seen from datareader"<<endl;}
  
  mThePmdReader = mTheDataReader->getPMDReader();
  
  if(mThePmdReader) {
    //printf("**********NPMDHITS==%d\n", mThePmdReader->NPMDHits() );
  }
  else {
    gMessMgr->Info("StPmdReaderMaker::Make()");
    gMessMgr->Info("Could not get PMD Reader");
    gMessMgr->Info("Skip this event");
    return kStWarn;
  }
  
  int adc[2*PMD_CRAMS_MAX*2*(PMD_CRAMS_CH_MAX)];
 
  
  int ret=mThePmdReader->getAllPmdCpvData(&adc[0]);
  if(ret){/*nothing*/}
  Int_t result=ApplyMapping(&adc[0]);
  if(result!=kStOK){gMessMgr->Info("Problem in getting PMD data:ApplyMap");
  return kStWarn;
  } 
  
  
  //	if(mThePmdReader) {
  //	    delete mThePmdReader;
  //	}
  //	mThePmdReader = 0;
  
  
  return kStOK;
}


Int_t StPmdReadMaker:: ApplyMapping(int *adc)
{
// Get Year of run
  char runfile[20];
  sprintf(runfile,"%d",mRunNumber);
  // Fetch from the run # the day
  char iRun[8];
  char iyear[8];
  for (Int_t ik=0; ik<3; ik++)
    {
      iRun[ik] = runfile[ik+1];
    }
  iyear[0] = runfile[0];
  Int_t year =0;
  year=atoi(iyear);
////////////////////////////////////
  mPmdGeom->readBoardDetail(mRunNumber); //!Read status of the FEE boards to apply proper mapping 
  
  mPmdCollection = new StPmdCollection("PmdCollection");
  m_DataSet->Add(mPmdCollection);
  StPmdDetector* det0 = mPmdCollection->detector(0); //! Collection for CPV
  StPmdDetector* det1 = mPmdCollection->detector(1); //! Collection for PMD
  
  Int_t Chain_No,supmod,row,col,SubDet,chtemp;
  int AddCh_Count=0;
   for(int SEC=0; SEC < PMD_SECTOR; SEC++){
    for(int CRAM=0; CRAM < PMD_CRAMS_MAX; CRAM++){
      for(int BLOCK=0; BLOCK < PMD_CRAMS_BLOCK; BLOCK++){
	for(int CHANNEL=0; CHANNEL < PMD_CRAMS_CH_MAX; CHANNEL++){
	  Int_t channel=CHANNEL;  // Input to apply mapping should be 0-1727.
	  
	  //Added for diffrent VME Crate conditions ////////////

	  switch (mVmeCond){
	    
	  case 1: 
	    {
	      if(SEC==0){
		if(BLOCK==0)Chain_No=25+CRAM;
		if(BLOCK==1)Chain_No=37+CRAM;
	      }	
              else break;	      
	    }
	    break; 
	  case 2: 
	    {
	      Chain_No=(CRAM+1)+(SEC*PMD_CRAMS_MAX)+(BLOCK*2*PMD_CRAMS_MAX);
	    }
	    break;
	  case 3:
	    {
	      if(SEC==0 && BLOCK==0 && CRAM==9) Chain_No = 39;
	      else if(SEC==0 && BLOCK==0 && CRAM==10) Chain_No = 40;
	      else Chain_No=(CRAM+1)+(SEC*PMD_CRAMS_MAX)+(BLOCK*2*PMD_CRAMS_MAX); 
	    }
	    break;
	  case 4:   // 2005 data
	    {
	      if(SEC==0 && BLOCK==0 && CRAM==5) Chain_No = 36;
	      else Chain_No=(CRAM+1)+(SEC*PMD_CRAMS_MAX)+(BLOCK*2*PMD_CRAMS_MAX); 
	    }
	    break;
	  }
// On 18th access it was found that I/P to chain 45 and 46 are interchanged,
//	  (see log book for details).
          Int_t Chain_original=Chain_No;
	  if(Chain_original==45)Chain_No=46;
	  if(Chain_original==46)Chain_No=45;
	  //Chain_No goes from 1 to 48
	  //   VME Condition Ends ///////////////////////////////////
	  
	  // Setting the SubDetector No.
	  
	  if (Chain_No >= 1 && Chain_No < 25) SubDet = 2; //! Chains from CPV 	  
	  if (Chain_No >= 25 && Chain_No <= 48) SubDet = 1; //! Chains from PMD 

	  
          // Apply Mapping to get the sm, row and col here
	  // 	 initialise the refs.  
          Int_t mapp=0;
           supmod=0;
	   row=0;
	   col=0;
	   chtemp=-1;

          if(mRunNumber < 6000000)mapp=mPmdGeom->ChainMapping(Chain_No,channel,supmod,col,row,chtemp);  // 2004 data
          if(mRunNumber >= 6000000)mapp=mPmdGeom->ChainMapping(Chain_No,channel,supmod,col,row,chtemp,year);  // 2005 data
	  Int_t DaqADC=adc[AddCh_Count];
	  // zeroing zeroeth channel
	  if(channel==0)DaqADC=0;

	  AddCh_Count++;
	  
	  if(DaqADC>0 && mapp==kStOK){
	 //Fill chain-channel QA
		  m_chain_channel->Fill(channel,Chain_No-1);

	  // Apply uniformity calibration here
	  //
	    if(mCalibFlag){
	      Float_t calib = 0;

	      if(supmod<=(2*PMD_CRAMS_MAX) && row <=PMD_ROW_MAX && col <=PMD_COL_MAX){
		
		Int_t stCalib = GetCalib(supmod,row,col,calib); 
		if(stCalib != kStOK)gMessMgr->Info("Problem in getting Calibration Constant");
		if(calib!=0)DaqADC=(Int_t)(DaqADC*calib);
  	      } //Check on overflow of supmod, row col
	    } // Calibration flag     
	    
	    // Converting ADC to Edep
	    Float_t edep=0;
	    mPmdGeom-> ADC2Edep(DaqADC, edep);
	    if(edep<0) edep=0;
	    
	    //Fill StPmdHit
	    StPmdHit *pmdhit = new StPmdHit();
	    if(supmod>PMD_CRAMS_MAX)supmod-=PMD_CRAMS_MAX;
	    pmdhit->setGsuper(Int_t(supmod));      //! filling supermodule no (1-12)
	    pmdhit->setSubDetector(Int_t(SubDet)); //! filling subdetector (pmd=1,cpv=2)
	    pmdhit->setRow(Int_t(row));            //! filling row
	    pmdhit->setColumn(Int_t(col));         //! filling col
	    pmdhit->setAdc(Int_t(DaqADC));         //! filling ADC   
	    pmdhit->setEdep(Float_t(edep));        //! filling energy   

	    if(SubDet==2)det0->addHit(pmdhit);
	    if(SubDet==1)det1->addHit(pmdhit);
	  } //Check on non zero DaqADC
	  
	} //CHANNEL
      } //BLOCK
    } //C-RAM
   } //SEC
  
  if(mPmdPrint)gMessMgr->Info("StEvent to be called **");
  Int_t testevt=fillStEvent(det0,det1);  //called as (cpv_det,pmd_det)
  if(testevt!=kStOK)gMessMgr->Info("Problem in fillStEvent");
  
  return kStOK;
}
//-------------------------------------------------------------------------

Int_t StPmdReadMaker::fillStEvent(StPmdDetector* cpv_det, StPmdDetector* pmd_det)
{
  // Look for StEvent
  StEvent *currevent = (StEvent*)GetInputDS("StEvent");
  if(!currevent){
    gMessMgr->Info("NO STEVENT**");
    //	   StEvent *currevent=new StEvent();
    
    return kStWarn;
  }
  // Create PhmdCollection, first Maker in Chain, so here it is created
  mEvtPmdCollection = new StPhmdCollection();
  currevent->setPhmdCollection(mEvtPmdCollection);
 
  //Set the Detectors to PhmdCollection
  if(mEvtPmdCollection){
    mPmdEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdId)); 
    mCpvEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdCpvId));
  }
    Int_t tothit_pmd=0;
    Int_t tothit_cpv=0;
  
  StEventInfo* eventInfo = currevent->info();
  Int_t Nevent=eventInfo->id();
  
//  if(!mEvtPmdCollection){
  //  cout<<"No PMDCOLLECTION **, Creating one"<<endl;
  //  mEvtPmdCollection = new StPhmdCollection();
  //  currevent->setPhmdCollection(mEvtPmdCollection);
  //  mPmdEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdId)); 
  //  mCpvEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdCpvId));
  //}

  for(Int_t id=1;id<(PMD_CRAMS_MAX+1);id++){

    //Fill StEvent info for PMD and CPV, first PMD(subdet=1)
   //does the id goes from 1 to 12 for PmdHit (for PhmdHit it goes 0-11).
	  
    StPmdModule * pmd_mod=pmd_det->module(id);
    Int_t nmh1=pmd_det->module_hit(id);

    if(nmh1>0){
      TIter next(pmd_mod->Hits());
      StPmdHit *spmcl1;
      for(Int_t im=0; im<nmh1; im++)
	{
	  spmcl1 = (StPmdHit*)next();
	  if(spmcl1){
            Int_t subdet=spmcl1->SubDetector();
      	    Int_t gsuper=spmcl1->Gsuper();
	    Int_t col=spmcl1->Column();
	    Int_t row=spmcl1->Row();
	    Float_t edep=spmcl1->Edep();
	    Int_t adc=spmcl1->Adc();
	    
	    //! Filling PmdHit for StEvent
	    
	    StPhmdHit *phit = new StPhmdHit();
	    /*
	    phit->setSuperModule(Int_t(gsuper-1)); // filling supermodule no (range 0-11)
	    phit->setSubDetector(Int_t(subdet));   // filling subdetector
	    phit->setRow(Int_t(row));              // filling row
	    phit->setColumn(Int_t(col));           // filling col
	    phit->setEnergy(edep);                 // filling energy
	    phit->setAdc(adc);                     // filling ADC
	   */
	    // changed to accommodate numbering scheme (i.e start from 0)
	    phit->setSuperModule(Int_t(gsuper-1)); // filling supermodule no (range 0-11)
	    phit->setSubDetector(Int_t(subdet-1));   // filling subdetector (pmd=0,cpv=1)
	    phit->setRow(Int_t(row-1));              // filling row (starts from 0)
	    phit->setColumn(Int_t(col-1));           // filling col (starts from 0)
	    phit->setEnergy(edep);                 // filling energy
	    phit->setAdc(adc);                     // filling ADC
	tothit_pmd++;
	    if(mPmdEvent)mPmdEvent->addHit(phit);
	  }
	}
    }

    //Now CPV (subdet=2)    //////
    
    Int_t nmh2=cpv_det->module_hit(id);
    StPmdModule * cpv_mod=cpv_det->module(id);
    
    if(nmh2>0){
      TIter next(cpv_mod->Hits());
      StPmdHit *spmcl2;
      for(Int_t im=0; im<nmh2; im++)
	{
	  spmcl2 = (StPmdHit*)next();
	  if(spmcl2){
            Int_t subdet=spmcl2->SubDetector();
	    Int_t gsuper=spmcl2->Gsuper();
	    Int_t col=spmcl2->Column();
	    Int_t row=spmcl2->Row();
	    Float_t edep=spmcl2->Edep();
	    Int_t adc=spmcl2->Adc();
	    
	    StPhmdHit *phit = new StPhmdHit();
	    /*
	    phit->setSuperModule(Int_t(gsuper-1));
	    phit->setSubDetector(Int_t(subdet));
	    phit->setRow(Int_t(row));
	    phit->setColumn(Int_t(col));
	    phit->setEnergy(edep);
	    phit->setAdc(adc);              
	    */

	    // changed to accommodate numbering scheme (i.e start from 0)
	    phit->setSuperModule(Int_t(gsuper-1)); // filling supermodule no (range 0-11)
	    phit->setSubDetector(Int_t(subdet-1));   // filling subdetector (pmd=0,cpv=1)
	    phit->setRow(Int_t(row-1));              // filling row (starts from 0)
	    phit->setColumn(Int_t(col-1));           // filling col (starts from 0)
	    phit->setEnergy(edep);                 // filling energy
	    phit->setAdc(adc);                     // filling ADC
	    
	tothit_cpv++;
	    if(mCpvEvent)mCpvEvent->addHit(phit);
	  }
	}
    }
  }
  m_event_tothit_pmd->Fill(Nevent,tothit_pmd);
  m_event_tothit_cpv->Fill(Nevent,tothit_cpv);


  return kStOK;
}
//------------------------------------------------------------------

Int_t StPmdReadMaker::Finish() {
  if(mPmdPrint)gMessMgr->Info("StPmdReadMaker::Finish()");
  
  return StMaker::Finish();
}
//------------------------------------------------------------------

//! This method obtains the calibration constants for applying uniformity
Bool_t StPmdReadMaker::ReadCalibrationsConst()
{
  mDb=NULL;
  TString DbName = "Calibrations/pmd/";
  mDb=GetInputDB(DbName.Data());
  if(!mDb) return kFALSE;

  //getting tables ////////////////////////////////////////////////////
	
  pmdCalSummary_st* pmdcalsum = NULL;
  St_pmdCalSummary* summ = (St_pmdCalSummary*) mDb->Find("pmdCalSummary");
  if(summ) pmdcalsum = summ->GetTable();

  pmdBrdMipCalib_st* pmdcalibst = NULL;
  TString TableName;
  St_pmdBrdMipCalib* a = (St_pmdBrdMipCalib*) mDb->Find("pmdBrdMipCalib");
  if(a) pmdcalibst = a->GetTable();
  if(!pmdcalibst) return kFALSE;
  m_PmdCalibConst=pmdcalibst;		
  
  return kTRUE;
}
//------------------------------------------------------------------
Int_t StPmdReadMaker::GetCalib(int supmod,int row,int col,float& calib){

    Int_t brdno=0;
    mPmdDBUtil->BoardNumber(supmod-1,row-1,col-1,brdno);
    
    Int_t brdch=0;
    mPmdDBUtil->ChannelInBoard(supmod-1,row-1,col-1,brdch);

    if(brdno>0 && brdch>0) calib=m_PmdCalibConst[brdno-1].MipPeakPosition[brdch];
   
return kStOK;    
}
void StPmdReadMaker::bookHist(){
	m_event_tothit_pmd = new TH1F("pmd_tothit","tothit vs eventNo (PMD)",10000,0,100000);
	m_event_tothit_cpv = new TH1F("cpv_tothit","tothit vs eventNo (CPV)",10000,0,100000);

	m_chain_channel = new TH2F("chain_chan","channel vs chain No ",1728,-0.5,1727.5,48,-0.5,47.5);
}

