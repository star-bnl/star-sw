/***************************************************************************
 *
 * StPmdReadMaker
 *
 * Author:  Supriya Das and Subhasis Chattopadhyay
 **************************

 *************************************************
 *
 * Description: Reading PMD data and filling hits for StEvent
 ****************************
 ***********************************************
 */

//#include <Stiostream.h>
#include <sstream>

#include "StPmdReadMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

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
#include "StBFChain.h"
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

}

//-----------------------------------------------------------------

StPmdReadMaker::~StPmdReadMaker() {
if(mPmdGeom){mPmdGeom=0; delete mPmdGeom;}
if(mPmdDBUtil){mPmdDBUtil=0; delete mPmdDBUtil;}
if(mThePmdReader)delete mThePmdReader;
if(mTheDataReader)delete mTheDataReader;
if(mThePmdData)delete mThePmdData;
if(mPmdEvent)delete mPmdEvent;
if(mCpvEvent)delete mCpvEvent;
if(mPmdCollection)delete mPmdCollection;
if(mDb)delete mDb;
if(m_PmdCalibConst)delete m_PmdCalibConst;
}

//-----------------------------------------------------------------

Int_t StPmdReadMaker::Init() {
if(mPmdPrint)gMessMgr->Info("StPmdReaderMaker::Init()");
  //moved to Make(), Later on will be kept in InitRun, if needed
  //    ReadCalibrationsConst();

    
  return StMaker::Init();
}
//-----------------------------------------------------------------
Int_t StPmdReadMaker::InitRun(Int_t runnr) {
  ReadCalibrationsConst();
  return StMaker::InitRun(runnr);

}
//------------------------------------------------------------------
Int_t StPmdReadMaker::Make() {

if(mPmdPrint)gMessMgr->Info("StPmdReaderMaker::Make()");
  //cout<<"calib const to be read "<<endl;
  mThePmdData   = GetDataSet("StDAQReader");

  if(!mThePmdData) {
gMessMgr->Info("StPmdReaderMaker::Make()");
gMessMgr->Info("DataSet: StDAQReader not there()");
gMessMgr->Info("Skip this event");
return kStWarn;
  }
  else{gMessMgr->Info("GOT DaqReader dataset");}

  mTheDataReader = (StDAQReader*)(mThePmdData->GetObject());
  if(!mTheDataReader) {
gMessMgr->Info("StPmdReaderMaker::Make()");
gMessMgr->Info("DataSet: PMDData not there()");
gMessMgr->Info("Skip this event");
return kStWarn;
  }
  else{gMessMgr->Info("GOT DaqReader object, look for PMD**");}
  // Will be ON once Victor fixes PMD present in StDAQMaker
	    
  /*	    if ( !(mTheDataReader->PMDPresent())) {
	    cout << "StPmdreaderMaker::Maker()\n";
	    cout << "\tPMD  not in datastream\n";
	    cout << "\tSkip this event\n" << endl;
	    return kStWarn;
	    }
	    else{cout<<"PMD prsent seen from datareader"<<endl;}
  */
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
  //if(ret!=kStOK){gMessMgr->Info("Problem in getting PMD data:GetData");return kStWarn;} 
  cout<<"No of cells = "<<ret<<endl;

  Int_t result=ApplyMapping(&adc[0]);
  if(result!=kStOK){gMessMgr->Info("Problem in getting PMD data:ApplyMap");return kStWarn;} 


  //	if(mThePmdReader) {
  //	    delete mThePmdReader;
  //	}
  //	mThePmdReader = 0;


  return kStOK;
}


Int_t StPmdReadMaker:: ApplyMapping(int *adc)
{

  mPmdCollection = new StPmdCollection("PmdCollection");
  m_DataSet->Add(mPmdCollection);
  StPmdDetector* det0 = mPmdCollection->detector(0);
  StPmdDetector* det1 = mPmdCollection->detector(1);

  Int_t supmod,row,col;
  int AddCh_Count=0;
  Int_t TotalHit=0;	
  for(int SEC=0; SEC < PMD_SECTOR; SEC++){
    for(int CRAM=0; CRAM < PMD_CRAMS_MAX; CRAM++){
      for(int BLOCK=0; BLOCK < PMD_CRAMS_BLOCK; BLOCK++){
	for(int CHANNEL=0; CHANNEL < PMD_CRAMS_CH_MAX; CHANNEL++){
	  Int_t Chain_No=(CRAM+1)+(SEC*PMD_CRAMS_MAX)+(BLOCK*2*PMD_CRAMS_MAX);
	  Int_t channel=CHANNEL+1;
	  mPmdGeom->ChainMapping(Chain_No,channel,supmod,col,row);
	  Int_t DaqADC=adc[AddCh_Count];
	  AddCh_Count++;
	  // apply calib const here
	  //
	  if(DaqADC>0){
	    TotalHit++;
	    Float_t calib=1.;
	    if(supmod<=(2*PMD_CRAMS_MAX) && row <=PMD_ROW_MAX && col <=PMD_COL_MAX){
	    	Int_t brdno=0;
		mPmdDBUtil->BoardNumber(supmod-1,row-1,col-1,brdno);
		Int_t brdch=0;
		mPmdDBUtil->ChannelInBoard(supmod-1,row-1,col-1,brdch);

	    	//if(brdno>0 && brdch>0)calib=m_MipPeak[brdno][brdch];
	    	if(brdno>0 && brdch>0)calib=m_PmdCalibConst[brdno-1].MipPeakPosition[brdch];
	         }
	    if(calib!=0)DaqADC=(Int_t)(DaqADC*calib);

	    // Converting ADC to Edep

	    Float_t edep=0;
	 
	    mPmdGeom-> ADC2Edep(DaqADC, edep);

	    if(edep<0) edep=0;

	    //Fill StPmdHit
	    StPmdHit *pmdhit = new StPmdHit();
	    if(supmod>PMD_CRAMS_MAX)supmod-=PMD_CRAMS_MAX;
	    pmdhit->setGsuper(Int_t(supmod));     //! filling supermodule no
	    pmdhit->setSubDetector(Int_t(BLOCK+1)); //! filling subdetector
	    pmdhit->setRow(Int_t(row));            //! filling row
	    pmdhit->setColumn(Int_t(col));         //! filling col
	    pmdhit->setAdc(Int_t(DaqADC));       //! filling ADC   
	    pmdhit->setEdep(Float_t(edep));        //! filling energy   
	    if(BLOCK==0)det0->addHit(pmdhit);
	    if(BLOCK==1)det1->addHit(pmdhit);
	    if(mPmdPrint)cout<<"Applymap:Chain "<<Chain_No<<"channel "<<channel<<"supmod "<<supmod<<"col  "<<col<<" row "<<row<<"ADC "<<DaqADC<<"BLOCK "<<BLOCK<<endl;	
	  }

	}
      }
    }
  }

  if(mPmdPrint)cout<<"StEvent to be called **"<<endl;
  Int_t testevt=fillStEvent(det0,det1);
  if(testevt!=kStOK)cout<<"problem in fillStEvent"<<endl;

  return kStOK;
}


Int_t StPmdReadMaker::fillStEvent(StPmdDetector* pmd_det, StPmdDetector* cpv_det)
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
   
  //  if(!mEvtPmdCollection){
  //  cout<<"No PMDCOLLECTION **, Creating one"<<endl;
  //  mEvtPmdCollection = new StPhmdCollection();
  //  currevent->setPhmdCollection(mEvtPmdCollection);
  //  mPmdEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdId)); 
  //  mCpvEvent = mEvtPmdCollection->detector(StDetectorId(kPhmdCpvId));
  //}

  for(Int_t id=1;id<(PMD_CRAMS_MAX+1);id++){

    //Fill StEvent info for PMD and CPV, first PMD(subdet=1)
    Int_t subdet=1;
    StPmdModule * pmd_mod=pmd_det->module(id);
    Int_t nmh1=pmd_det->module_hit(id);

    if(nmh1>0){
      TIter next(pmd_mod->Hits());
      StPmdHit *spmcl1;
      for(Int_t im=0; im<nmh1; im++)
	{
	  spmcl1 = (StPmdHit*)next();
	  if(spmcl1){
	    Int_t gsuper=spmcl1->Gsuper();
	    Int_t col=spmcl1->Column();
	    Int_t row=spmcl1->Row();
	    Float_t edep=spmcl1->Edep();
	    Int_t adc=spmcl1->Adc();
	    //! Filling PmdHit for StEvent
	    StPhmdHit *phit = new StPhmdHit();
	    phit->setSuperModule(Int_t(gsuper-1));     // filling supermodule no (range 0-11)
	    phit->setSubDetector(Int_t(subdet)); // filling subdetector
	    phit->setRow(Int_t(row));            // filling row
	    phit->setColumn(Int_t(col));         // filling col
	    phit->setEnergy(edep);              // filling energy
	    phit->setAdc(adc);              // filling ADC
	    if(mPmdEvent)mPmdEvent->addHit(phit);
	  }
	}
    }

    //CPVV    //////
    Int_t nmh2=cpv_det->module_hit(id);
    StPmdModule * cpv_mod=cpv_det->module(id);
    if(nmh2>0){
      subdet=2;
      TIter next(cpv_mod->Hits());
      StPmdHit *spmcl2;
      for(Int_t im=0; im<nmh2; im++)
	{
	  spmcl2 = (StPmdHit*)next();
	  if(spmcl2){
	    Int_t gsuper=spmcl2->Gsuper();
	    Int_t col=spmcl2->Column();
	    Int_t row=spmcl2->Row();
	    Float_t edep=spmcl2->Edep();
	    Int_t adc=spmcl2->Adc();
	    StPhmdHit *phit = new StPhmdHit();

	    phit->setSuperModule(Int_t(gsuper-1));
	    phit->setSubDetector(Int_t(subdet));
	    phit->setRow(Int_t(row));
	    phit->setColumn(Int_t(col));
	    phit->setEnergy(edep);
	    phit->setAdc(adc);              
	    if(mCpvEvent)mCpvEvent->addHit(phit);
	  }
	}
    }
  }
  return kStOK;
}


Int_t StPmdReadMaker::Finish() {


  return StMaker::Finish();
}



/*!
  This method obtains the calibration constants for applying uniformity
*/
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
  //loop over all boards and all channels //
  //Int_t MAX_BRD= 1296;
  /*
    for(Int_t id=1;id<=MAX_BRD;id++)
    {
      for(Int_t ich=0;ich<PMD_BOARD_CH_MAX;ich++){
	Float_t c = pmdcalibst[id-1].MipPeakPosition[ich];
	cout<<"id-1, ich "<<id-1<<" "<<ich<<"calib "<<c<<endl;

//	m_MipPeak[id-1][ich]=c;
      }

    }
 //
  */

  return kTRUE;
}

