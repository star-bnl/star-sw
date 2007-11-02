/***************************************************************************
 *$Id: StPmdReadMaker.cxx,v 1.26 2007/11/02 11:03:00 rashmi Exp $
 *
 * StPmdReadMaker
 *
 * Author:  Supriya Das and Subhasis Chattopadhyay
 ***************************************************************************
 *
 * Description: Reading PMD data and filling hits for StEvent
 **************************************************************************
 *$Log: StPmdReadMaker.cxx,v $
 *Revision 1.26  2007/11/02 11:03:00  rashmi
 *Storing gains with hits, not applying gain calibration
 *
 *Revision 1.25  2007/10/26 18:13:17  rashmi
 *fixed some warnings
 *
 *Revision 1.24  2007/09/06 06:35:30  genevb
 *Small refinements to the BadChain fix
 *
 *Revision 1.23  2007/09/06 05:30:52  subhasis
 *Subhasis: BadChain fix to avoid cucu crash
 *
 *Revision 1.22  2007/09/05 03:41:42  genevb
 *Attribute check was in the wrong place
 *
 *Revision 1.21  2007/09/05 03:19:45  genevb
 *Use attribute pmdRaw
 *
 *Revision 1.20  2007/08/31 10:50:12  rashmi
 *Added routines to read badchains,HotCells,Cell_GNF,SMChain_GNF,Modified VMEcondition&ApplyMapping(subhasis)
 *
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

//added for cleanup 
#include "StPmdCleanConstants.h"
Int_t BadChainZero[25];
Int_t * BadChain;
Float_t SM_chain_factor[24][48];
//ofstream fout("calib_out.dat");

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
  //  mCalibFlag=kFALSE;

  mCalibFlag=kTRUE;
  mPmdPrint=kFALSE;
  mHotCells=NULL;                                                          
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
  mHotCells=0;
}

//-----------------------------------------------------------------

Int_t StPmdReadMaker::Init() {
  if(mPmdPrint)gMessMgr->Info("StPmdReadMaker::Init()");
//  mCalibFlag = !(IAttr("pmdRaw"));
  mCalibFlag = (IAttr("pmdRaw"));
  bookHist();

  return StMaker::Init();
}
//-----------------------------------------------------------------
Int_t StPmdReadMaker::InitRun(Int_t runnr) {
  if(mPmdPrint)gMessMgr->Info("StPmdReaderMaker::InitRun()");
  
  mRunNumber = runnr;
  
  mPmdGeom->readBoardDetail(runnr);
  if(Debug())cout<<"PMDINITRUN*****runnr "<<runnr<<"   "<<mRunNumber<<endl;
  
  if(mRunNumber < 5034042) mVmeCond = 1;
  else if(mRunNumber >= 5034042 && mRunNumber < 5049020) mVmeCond = 2;
  else if(mRunNumber >= 5049020 && mRunNumber < 6000000) mVmeCond = 3;
  else if(mRunNumber >= 6000000 && mRunNumber < 7000000) mVmeCond = 4; // run 7 data
  else if(mRunNumber >= 8000000 && mRunNumber < 9000000) mVmeCond = 5; // run 7 data
  //  else mVmeCond = 4;
  // // Subhasis: 7000000 to 8000000, this was pp , PMD was absent..
  // subhasis (25th aug 2007:) These conditions need to be checked very carefully
  
  if(mPmdPrint) cout<<"Run Number, VME Condition : "<<mRunNumber<<" "<<mVmeCond<<endl;
// subhasis // to fix crash in BadChain
    for(Int_t i=0;i<25;i++) BadChainZero[i]=0;
  
  ReadBadChains(runnr);
  ReadCalibrationsConst();

  /*
 for(int ic=1;ic<=48;ic++){
   for(int sm = 1;sm<=24;sm++){
     if(SM_chain_factor[sm-1][ic-1]>0)cout<<sm<<","<<ic<<","<<SM_chain_factor[sm-1][ic-1]<<endl;
   }
 }
                                                             
 for(int ic=1;ic<=48;ic++){
   for(int icl=0;icl<1728;icl++){
     if(IsHot(ic,icl))fout<<"HOT "<<ic<<" "<<icl<<endl;
   }
 }
  */
  
  return StMaker::InitRun(runnr);
}
//----------------------------------------------------
void StPmdReadMaker::ReadBadChains(Int_t runNo){
                                                             
  Int_t rn,year;
  mPmdGeom->GetRunYear(runNo,rn,year);
  if(Debug())cout<<"runNo="<<runNo<<" year="<<year<<endl;
                                                             
  if(year==8){
    if(runNo>8143000){
      BadChain = PmdClean::BadChain_y8d143;
    }else if(runNo>8135000){
      BadChain = PmdClean::BadChain_y8d135;
    }else if(runNo>8131000){
      BadChain = PmdClean::BadChain_y8d131;
    }else if(runNo>8122000){
      BadChain = PmdClean::BadChain_y8d122; // for year2007 data for day122-1\28
    }else if(runNo>8116000){
      BadChain = PmdClean::BadChain_y8d116; // for year2007 data for day116-1\21
    }else if(runNo>8108000){
      BadChain = PmdClean::BadChain_y8d108; // for year2007 data for day108-1\15
    }else if(runNo>8102000){
      BadChain = PmdClean::BadChain_y8d102; // for year2007 data for day102-1\07
    }else if(runNo > 8095000){
      BadChain = PmdClean::BadChain_y8d95; // for year2007 data for day95-100
    }else if(runNo>8089001){
      BadChain = PmdClean::BadChain_y8d89; // for year2007 data for day89-92
    }else{
      BadChain = PmdClean::BadChain_y8d0;
    }
  } else {
    BadChain = BadChainZero;
  }

  if (BadChain[0]>0 && Debug()){
    //if (BadChain[0]>0){
    cout<<"BadChains are ";
    for(Int_t i=0;i<25;i++){
      if (BadChain[i]>0) cout<<BadChain[i]<<" ";
    }
    cout<<endl;
  }
                                                             
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
  if(Debug())cout<<"In StPmdReadMaker::Make"<<endl;
  //  fout<<"In StPmdReadMaker::Make"<<endl;
  
  int adc[2*PMD_CRAMS_MAX*2*(PMD_CRAMS_CH_MAX)];
 
  
  int ret=mThePmdReader->getAllPmdCpvData(&adc[0]);
  if(ret){/*nothing*/}
  Int_t result=ApplyMapping(&adc[0]);
  if(result!=kStOK){gMessMgr->Info("Problem in getting PMD data:ApplyMap");
  return kStWarn;
  } 

  /*
  for(int is=1;is<=2*PMD_CRAMS_MAX;is++){
    for(int ir=1;ir<=PMD_ROW_MAX;ir++){
      for(int ic=1;ic<=PMD_COL_MAX;ic++){
        float calib=0;
        Int_t stCalib = GetCalib(is,ir,ic,calib);
        if(calib>0) fout<<"is,ir,ic "<<is<<" "<<ir<<" "<<ic<<" "<<calib<<endl;
      }
    }
  }
  */
  
  
  //	if(mThePmdReader) {
  //	    delete mThePmdReader;
  //	}
  //	mThePmdReader = 0;
  
  
  return kStOK;
}


Int_t StPmdReadMaker:: ApplyMapping(int *adc)
{
// Get Year of run
/*
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
*/
  Int_t rn=0; Int_t year=0;
  mPmdGeom->GetRunYear(mRunNumber,rn,year);
  if(Debug())cout<<"runNo="<<mRunNumber<<" year="<<year<<endl;
  //  fout<<"runNo="<<mRunNumber<<" year="<<year<<" day="<<rn<<endl;
////////////////////////////////////
  mPmdGeom->readBoardDetail(mRunNumber); //!Read status of the FEE boards to apply proper mapping 
  
  mPmdCollection = new StPmdCollection("PmdCollection");
  m_DataSet->Add(mPmdCollection);
  StPmdDetector* det0 = mPmdCollection->detector(0); //! Collection for CPV
  StPmdDetector* det1 = mPmdCollection->detector(1); //! Collection for PMD
  
  Int_t Chain_No,supmod,row,col,SubDet=0,chtemp;
  int AddCh_Count=0;
  Int_t orig_nhits = 0;
  Int_t nhits=0;
  Int_t nhits_sm[24]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
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
          case 5:   // 2006-7 data
            {
              // !!! this needs to be checked
              if(SEC==0 && BLOCK==0 && CRAM==5) Chain_No = 36;
              else Chain_No=(CRAM+1)+(SEC*PMD_CRAMS_MAX)+(BLOCK*2*PMD_CRAMS_MAX);
            }
            break;
	  }
	  if(mVmeCond==4){
	    // On 18th access it was found that I/P to chain 45 and 46 are interchanged,
	    //	  (see log book for details).
	    Int_t Chain_original=Chain_No;
	    if(Chain_original==45)Chain_No=46;
	    if(Chain_original==46)Chain_No=45;
	    //Chain_No goes from 1 to 48
	  }
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
          if(mRunNumber >= 6000000)mapp=mPmdGeom->ChainMapping(Chain_No,channel,supmod,col,row,chtemp,year);  // 2005 onwards data
	  Int_t DaqADC=adc[AddCh_Count];
	  // zeroing zeroeth channel
	  if(channel==0)DaqADC=0;

	  AddCh_Count++;
	  
	  if(DaqADC>0 && mapp==kStOK){
	    orig_nhits++;
	 //Fill chain-channel QA
		  m_chain_channel->Fill(channel,Chain_No-1);

	  // Apply uniformity calibration here
	  //
	      Float_t cellgain = 1;
	      Float_t smchaingain = 1;
	      Float_t cellstatus = 1;
//fout<<" "<<mRunNumber<<" "<<supmod<<" "<<row<<" "<<col<<" "<<DaqADC<<endl;
	    if(mCalibFlag){
	      Float_t calib = 1;
	      Float_t final_factor = 0;

	      if(supmod<=(2*PMD_CRAMS_MAX) && row <=PMD_ROW_MAX && col <=PMD_COL_MAX){
		
		Int_t stCalib = GetCalib(supmod,row,col,calib); 
		if(stCalib != kStOK)gMessMgr->Info("Problem in getting Calibration Constant");

                if(mRunNumber < 8000000){
//                  final_factor=calib;
                  final_factor=1.;
                }
                if(mRunNumber >= 8000000){
//storing gain factors for StPmdHit to be used in ClusterMaker
	          cellgain=calib;
	          smchaingain=SM_chain_factor[supmod-1][Chain_No-1];
// application of finalfactor
                  final_factor=calib*SM_chain_factor[supmod-1][Chain_No-1];
		  //		  fout<<"ADC, chain, row, col, calib, sm_chain "<<DaqADC<<" "<<Chain_No<<" "<<row<<" "<<col<<" "<<calib<<" "<<SM_chain_factor[supmod-1][Chain_No-1]<<" final_factor="<<final_factor<<endl;
                  if(!Accept(Chain_No,channel)){
			final_factor=0.0;
			cellstatus=0;
		    }
//	      if(SubDet==1) fout<<supmod-1<<" "<<col-1<<" "<<row-1<<" "<<calib<<" "<<smchaingain<<" "<<cellstatus<<endl;
                }
		//if(calib!=0)DaqADC=(Int_t)(DaqADC*calib);

		//		fout<<supmod<<","<<row<<","<<col<<","<<Chain_No<<","<<calib<<","<<SM_chain_factor[supmod-1][Chain_No-1]<<","<<final_factor<<","<<DaqADC;
                if(final_factor>0){
//subhasi: stop applying gain factor here, apply in clusterMaker
//                  DaqADC=(Int_t)(1.0*DaqADC/final_factor);
                }else{
//                  DaqADC=0;
                }

                                                             
  	      } //Check on overflow of supmod, row col
	    } // Calibration flag     
	    
	    // Converting ADC to Edep
	    Float_t edep=0;
	    mPmdGeom-> ADC2Edep(DaqADC, edep);
	    if(edep<0) edep=0;
	    
	    if(DaqADC>0){
	      //Fill StPmdHit
	      StPmdHit *pmdhit = new StPmdHit();
	      nhits_sm[supmod-1]++;
	      if(supmod>PMD_CRAMS_MAX)supmod-=PMD_CRAMS_MAX;
	      pmdhit->setGsuper(Int_t(supmod));      //! filling supermodule no (1-12)
	      pmdhit->setSubDetector(Int_t(SubDet)); //! filling subdetector (pmd=1,cpv=2)
	      pmdhit->setRow(Int_t(row));            //! filling row
	      pmdhit->setColumn(Int_t(col));         //! filling col
	      pmdhit->setAdc(Int_t(DaqADC));         //! filling ADC   
	      pmdhit->setEdep(Float_t(edep));        //! filling energy   
	      // gain related
	      pmdhit->setGainCell(Float_t(cellgain));        //! filling cellgain   
	      pmdhit->setGainSmChain(Float_t(smchaingain));        //! smchaingain   
	      pmdhit->setCellStatus(Float_t(cellstatus));        //! filling cellstatus   
	      
	    if(SubDet==2)det0->addHit(pmdhit);
	    if(SubDet==1)det1->addHit(pmdhit);
	    nhits++;
	    } // Check on non zero DaqADC after calibration and Accept()

	  } //Check on non zero DaqADC
	  
	} //CHANNEL
      } //BLOCK
    } //C-RAM
   } //SEC
   
   if(Debug()){
     cout<<" NUmber of channels read ="<<AddCh_Count<<endl;
     cout<<" NUmber of original hits ="<<orig_nhits<<endl;
     cout<<" number of hits="<<nhits<<endl;
     for(Int_t ism = 0;ism<24;ism++){
       if(nhits_sm[ism]>0) cout<<"number if hits in module "<<ism+1<<" is ="<<nhits_sm[ism]<<endl;
     }
   }
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
void StPmdReadMaker::bookHist(){
  m_event_tothit_pmd = new TH1F("pmd_tothit","tothit vs eventNo (PMD)",10000,0,100000);
  m_event_tothit_cpv = new TH1F("cpv_tothit","tothit vs eventNo (CPV)",10000,0,100000);
                                                             
  m_chain_channel = new TH2F("chain_chan","channel vs chain No ",1728,-0.5,1727.5,48,-0.5,47.5);
}
                                                             
                                                             
//---------------------------------------------------------------------
Bool_t StPmdReadMaker::IsHot(Int_t chain,Int_t channel){
  // Find out if a given chain/channel is "hot"
  // DB stores element IDs 1-48 => table rows 0-47 => chains 1-48
  // DB stores 1728 channels as 54 x 32bit words (1-1728)
  if (!mHotCells) return kFALSE;
                                                                                       
  //   channel--;
  int offset = channel/32;
  int bit = channel%32;
                                                                                       
  pmdHotCells_st* chainCells = mHotCells->GetTable(chain-1);
  unsigned int* bitMask = &(chainCells->m00);
  bitMask += offset;
  Bool_t isHot = ( ((*bitMask) & (((unsigned int) 1) << (31-bit))) != 0 );
  //   static int isHotCount = 0;
  if (isHot) {
    //     isHotCount++;
    //    cout<<" isHot: "<< isHotCount << endl;
    //    mHotTracks++;
    //    if(Debug())cout<<"chain/channel="<<chain<<"  "<<channel<<endl;
    //    cout<<"in IsHOT: chain/channel="<<chain<<"  "<<channel<<endl;
  }
  // temporarily put here by RR
  // to remove chain 11 and chain 13
  // if (chain==11|| chain==13) isHot=kTRUE;
  return isHot;
}
//----------------------------------------------------------
Bool_t StPmdReadMaker::Accept(Int_t chain,Int_t channel){
                                                                                       
  if (channel==0)return kFALSE;
  // This should have access to database of hot channels according to year
                                                                                       
  for(Int_t i=0;i<25;i++){
    // if the chain is not to be included in the analysis for some reason
    if (chain==BadChain[i]){
      //      fout<<"BadChain="<<BadChain[i]<<endl;
      return kFALSE;
    }
  }
  //  Int_t chain_channel = chain*1728+channel;
  //  return !IsHot(chain_channel);
  return !IsHot(chain,channel);
                                                                                       
}
//----------------------------------------------------------------------
Bool_t StPmdReadMaker::ReadCalibrationsConst()
{
  if(Debug()) cout<<" StPmdReadMaker::I AM IN READCALIB "<<endl;
  
  StDbManager* mgr=StDbManager::Instance();
  StDbConfigNode* node=mgr->initConfig("Calibrations_pmd");
  node->setVersion("SMChain");
  
  mDb=NULL;
  m_PmdCalibConst=NULL;
  TString DbName = "Calibrations/pmd";
  mDb=GetInputDB(DbName.Data());
  if(Debug())cout<<"after mDB"<<endl;
  if(!mDb) return kFALSE;
  //  fout<<"after !mDb"<<mDb->GetTimeStamp()<<endl;
  
  for(Int_t ism=0;ism<24;ism++){
    for(Int_t chain=0;chain<48;chain++){
      SM_chain_factor[ism][chain]=0.;
    }
  }
  //getting tables ////////////////////////////////////////////////////
  
  // Cell_GNF Tables
  pmdCalSummary_st* pmdcalsum = NULL;
  St_pmdCalSummary* summ = (St_pmdCalSummary*) mDb->Find("pmdCalSummary");
  if(summ) pmdcalsum = summ->GetTable();
  
  pmdSMCalib_st* pmdcalibst = NULL;
  TString TableName;
  St_pmdSMCalib* a = (St_pmdSMCalib*) mDb->Find("pmdSMCalib");
  if(a) pmdcalibst = a->GetTable();
  if(!pmdcalibst) return kFALSE;
  m_PmdCalibConst=pmdcalibst;
  if(Debug())cout<<"obtained the Cell_GNF Tables"<<endl;
  
  // HOT CELLS
  mHotCells = (St_pmdHotCells*) mDb->Find("pmdHotCells");
  if (!mHotCells) gMessMgr->Warning("pmdHotCells not found!");
  //  if (!mHotCells) fout<<"pmdHotCells not found!"<<endl;
  
  Int_t nhot = 0;
  for (int chain=1; chain <= 48; chain++) {
    pmdHotCells_st* chainCells = mHotCells->GetTable(chain-1);
    if (!chainCells) {
      cout<<"End-of-DbTable. Stopping."<<endl;
      return kFALSE;
    }
    
    for (Int_t offset=0; offset<54; offset++) {
      unsigned int* bitMask = &(chainCells->m00);
      bitMask += offset;
      //      if (*bitMask!=0)cout<<chain<<" "<<offset<<"  "<<*bitMask<<endl;
      for (Int_t bit=31; bit>=0; bit--) {
        if ( ((*bitMask) & (((unsigned int) 1) << bit)) != 0 ) {
	  //	  Int_t channel = offset*32 + (31-bit);
          //            printf("%2d %4d",chain,channel);
	  //	  fout<<bit<<"  "<<chain<<" "<<channel<<endl;
          nhot++;
        }
      }
    }
    
  }
  if(Debug()) cout<<" number of hot cells is "<< nhot<<endl;
  
  // SM_CHAIN_GNF
  
  St_pmdSMChain_GNF * tab = (St_pmdSMChain_GNF*) mDb->Find("pmdSMChain_GNF");
  if (!tab) {
    cout<<"No pmdSMChain_GNF DBTable. Stopping."<<endl;
    return kFALSE;
  }
  
  //  cout<<"Got SmChain tables"<<endl;
  //  int sm,chain;
  //  float meancor,meancor_err,mpvcor,mpvcor_err;
  
  //  fout<<"Reading SMChain_GNF "<<endl;
  
  for (Int_t ismchain=0; ismchain<64; ismchain++) {
    pmdSMChain_GNF_st* smchain = tab->GetTable(ismchain);
    if (!smchain) {
      cout<<"End-of-SmChain DbTable. Stopping."<<endl;
      return kFALSE;
    }
    // Range of SM, Chain: from 0 or 1
    long supmod = smchain->sm;
    long chainNo=smchain->chain;
    float MeanFactor = smchain->mean_factor;
    float MPVFactor = smchain->mpv_factor;
    SM_chain_factor[supmod-1][chainNo-1]=MPVFactor;
    
    //struct pmdSMChain_GNF {
    //   long sm;              /* super module */
    //   long chain;           /* chain */
    //   float mean_factor;    /* mean */
    //   float errmean_factor; /* error on mean */
    //   float mpv_factor;     /* most probable value */
    //   float errmpv_factor;  /* error on mpv */
    //};
    if(Debug()){
      if(supmod>0 && chainNo>0){
	cout<<"SM,Chain,mean_factor,mpv_factor "<<supmod<<" "<<chainNo<<" "<<MeanFactor<<" "<<MPVFactor<<" "<<SM_chain_factor[supmod-1][chainNo-1]<<endl;
      }
    }
  }
  return kTRUE;
  
}


//------------------------------------------------------------------
Int_t StPmdReadMaker::GetCalib(int sm,int row,int col,float& calib){
  if(m_PmdCalibConst)calib=m_PmdCalibConst[sm-1].CellGain[row-1][col-1];
  //   cout<<"sm,row,col,calib"<<sm<<" "<<row<<" "<<col<<" "<<calib<<endl;
  //  if(Debug() && calib>0)fout<<"sm,row,col,calib "<<sm<<" "<<row<<" "<<col<<" "<<calib<<endl;
  return kStOK;
}

//------------------------------------------------------------------------------
