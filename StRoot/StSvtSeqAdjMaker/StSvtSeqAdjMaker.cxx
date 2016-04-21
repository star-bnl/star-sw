 /***************************************************************************
 *
 * $Id: StSvtSeqAdjMaker.cxx,v 1.57 2016/04/21 01:37:04 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Sequence
 **************************************************************************
 *
 * 
 * Revision 1.29  2001/09/26 18:42:48  Takahashi
 * Added new bad anode list and switched ON the bad anode elimination
 *
 * $Log: StSvtSeqAdjMaker.cxx,v $
 * Revision 1.57  2016/04/21 01:37:04  perev
 * Remove dangerous delete
 *
 * Revision 1.56  2007/04/28 17:57:10  perev
 * Redundant StChain.h removed
 *
 * Revision 1.55  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.54  2004/04/03 01:17:25  caines
 * I think this time I should really have stopped valgrind complaing lets see
 *
 * Revision 1.53  2004/03/18 04:05:01  caines
 * Remove from global scope variables used in debug mode as they shouldnt be there and caused erratic behaviour, also initialise some variables that valgrind was complaining about - didnt really need to as they are sent back from function which initialises them properly always but doesnt hurt
 *
 * Revision 1.52  2004/02/11 00:42:17  caines
 * Put common mode back
 *
 * Revision 1.51  2004/01/27 02:44:16  perev
 * LeakOff
 *
 * Revision 1.50  2004/01/22 17:15:21  caines
 * Remove gain calibration files and common mode moise subtraction
 *
 * Revision 1.49  2003/09/18 18:16:44  caines
 * Initialise stuff for new compiler
 *
 * Revision 1.48  2003/07/31 19:01:14  caines
 * Make changes so the slow simulator can run
 *
 * Revision 1.47  2003/07/18 17:15:40  caines
 * Fix Pedoffset to be 20 not 10 change variables to int from floats to avoid casting problems, fix that when pedestal goes negative we dont
 *
 * Revision 1.46  2003/04/14 15:56:59  munhoz
 * changes for gain calibration file
 *
 * Revision 1.45  2003/01/28 20:21:17  munhoz
 * changing from Init() to InitRun()
 *
 * Revision 1.44  2003/01/21 01:27:44  jeromel
 * hfile->write(0 while NULL caused spurious crash.
 * Took the oportunity to add GetCVS()
 * Some maniaco-compulsive //! alignement fixes ...
 *
 * Revision 1.43  2002/09/26 20:46:06  caines
 * Make sure initRun works when no svt data present and correct code to allow for the ordering init and initrun are called in
 *
 * Revision 1.42  2002/09/20 19:35:19  caines
 * Change building of file name
 *
 * Revision 1.41  2002/09/19 16:17:48  caines
 * Add code to do Juns gain calibration
 *
 * Revision 1.40  2002/05/09 16:55:08  munhoz
 * add reading bad anodes from DB
 *
 * Revision 1.39  2002/04/21 20:28:01  caines
 * Put some of the print out only if in debug mode
 *
 * Revision 1.37  2002/01/11 22:49:15  caines
 * Fix sequence merging bugs-hopefully
 *
 * Revision 1.36  2001/12/13 03:08:11  caines
 * Can now subtract common mode noise via black anodes 239 and 2
 *
 * Revision 1.35  2001/10/19 23:31:30  caines
 * Correct problem that if anodes were missing didnt do average common mode noise calc
 *
 * Revision 1.32  2001/10/04 02:56:01  caines
 * Change default pedoffset and black anodes to current values
 *
 * Revision 1.31  2001/10/02 22:55:57  caines
 * Jun was coorect about B2L8D2H2
 *
 * Revision 1.30  2001/09/28 20:47:38  caines
 * Fix typo in bad anode listing
 *
 * Revision 1.29  2001/09/28 15:36:41  caines
 * Add in bad anode elimination
 *
 * Revision 1.28  2001/09/26 18:42:48  caines
 * Fix 2 anode subtraction routines
 *
 * Revision 1.27  2001/09/22 00:35:54  caines
 * Fixes now that AddData() is cleared everyevent
 *
 * Revision 1.26  2001/09/16 22:24:22  caines
 * Fix for when SVT isnt in every event
 *
 * Revision 1.25  2001/08/28 19:02:55  caines
 * Remove hardwired line that makes it always do common mode average (dumb!)
 *
 * Revision 1.24  2001/08/24 21:40:04  caines
 * Adjust rawHybridData not SeqHybridData in Common Mode noise subtraction
 *
 * Revision 1.23  2001/08/24 20:57:45  caines
 * Do common mode noise suppression from first two anodes
 *
 * Revision 1.22  2001/08/22 14:24:49  caines
 * Raise m_thresh_hi to 10
 *
 * Revision 1.21  2001/07/25 14:47:41  caines
 * Fix filling histogram only when debug is on
 *
 * Revision 1.20  2001/07/22 20:31:28  caines
 * Better tuning for real data. Common mode noise calc and sub. Avoid overlapping seq. Fill histograms only in debug
 *
 * Revision 1.19  2001/05/06 22:16:35  caines
 * Fix pointer problem for DAQ Ped files
 *
 * Revision 1.18  2001/04/30 22:27:40  caines
 * Fix adjusting for ZSP data
 *
 * Revision 1.17  2001/04/25 19:00:17  perev
 * HPcorrs
 *
 * Revision 1.16  2001/03/28 21:49:38  caines
 * Correct bug that allowed AdjustSeq1 code to fall off end
 *
 * Revision 1.15  2001/03/22 20:47:10  caines
 * Comment out some of the QA histograms
 *
 * Revision 1.14  2001/02/07 19:15:05  caines
 * Change char[3] to char[4] for full SVT running
 *
 * Revision 1.13  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.12  2000/10/31 16:21:59  caines
 * Set up efaults for paramters in init
 *
 * Revision 1.11  2000/10/02 13:48:10  caines
 * Adjusting donw hybrid by hybrid
 *
 * Revision 1.10  2000/09/14 22:13:56  caines
 * Move histogram creation to better place
 *
 * Revision 1.9  2000/08/28 22:11:38  caines
 * Fixed check that data exists before using it
 *
 * Revision 1.8  2000/08/24 04:23:50  caines
 * Improved histograms
 *
 * Revision 1.7  2000/08/21 12:57:31  caines
 * Now opens and reads in ped using CalibMaker
 *
 * Revision 1.6  2000/08/09 02:05:08  caines
 * Only add pixels in the ASIC-like sequence adjusting
 *
 * Revision 1.5  2000/07/16 22:32:23  caines
 * Now also saves RAW data
 *
 * Revision 1.4  2000/07/13 14:50:13  caines
 * Fixed up problem when pointer went past 128
 *
 * Revision 1.3  2000/07/11 18:36:15  caines
 * Updates to save more of sequence for fitting
 *
 * Revision 1.2  2000/07/03 02:07:56  perev
 * StEvent: vector<TObject*>
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "StSequence.hh"
#include "StMCTruth.h"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtInverseProducts.hh"
#include "StSvtProbValues.hh"
#include "StSvtPedSub.h"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtSeqAdjMaker.h"
#include "StSvtCalibMaker/StSvtPedMaker.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"

#include "St_DataSetIter.h"
#include "TObjectSet.h"



//___________________________________________________________________________________________
StSvtSeqAdjMaker::StSvtSeqAdjMaker(const char *name) : StMaker(name)
{

  mSvtDataSet = NULL;
  mSvtAdjData = NULL;
  mSvtRawData = NULL;
  mSvtBadAnodes = NULL;
  mHybridRawData = NULL;
  mHybridAdjData = NULL;
  mSvtPedSub = NULL;
  mSvtPedColl = NULL;
  anolist = NULL;
  mInvProd = NULL;
  mProbValue = NULL;

  // Set up some defaults

  mPedFile = NULL;
  mPedOffSet = 20;
  m_thresh_lo = 3+mPedOffSet;
  m_thresh_hi = 5+mPedOffSet; 
  m_n_seq_lo  = 2;
  m_n_seq_hi  = 0;
  m_inv_prod_lo = 0;
 
}

//____________________________________________________________________________
StSvtSeqAdjMaker::~StSvtSeqAdjMaker(){
}
//____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Init(){


  hfile = NULL;
  GetSvtRawData();
  
  SetSvtData();
  
  mInvProd = new StSvtInverseProducts();
  mProbValue = new StSvtProbValues();
  
  float sigma;
  // set probability values
  for (int barrel = 1;barrel <= mSvtRawData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtRawData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtRawData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtRawData->getNumberOfHybrids();hybrid++) {
            int index = mSvtRawData->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
	    if (mSvtPedColl && mSvtPedColl->at(index))
	      sigma = ((StSvtHybridPed*)mSvtPedColl->at(index))->getRMS();
	    else
	      sigma = mProbValue->GetSigma();
	    mProbValue->SetProbValue(sigma);    
	    mInvProd->SetProbTable(mProbValue);
	}
      }
    }
  }
  
  return  StMaker::Init();  
}
//____________________________________________________________________________
Int_t StSvtSeqAdjMaker::InitRun( int runnumber)
{

  if(GetSvtRawData() ){
    
    St_DataSet *dataSet = NULL;
    dataSet = GetDataSet("StSvtConfig");
    
    if (!dataSet) return kStWarn;

    mSvtRawData = new StSvtData((StSvtConfig*)(dataSet->GetObject()));
    AddData(new TObjectSet("StSvtRawData",mSvtRawData));
  }

  mTotalNumberOfHybrids = mSvtRawData->getTotalNumberOfHybrids();

  if( hfile){
    hfile->Write();
    hfile->Close();
  }
  
  string ioMakerFileName;
  string FileName;
  string DirName;
  DirName=".";

  StIOMaker* mIOMaker = (StIOMaker*)GetMaker("inputStream");
  cout << "************************" << mIOMaker << endl;
  if (mIOMaker){
    ioMakerFileName = string(mIOMaker->GetFile()); 
    FileName = buildFileName( DirName+"/", baseName(ioMakerFileName),".SvtGainCal.root"); 
    // cout << "Heres my name: " << FileName << endl;
  }else
    {
      FileName=buildFileName( DirName+"/", "SeqAdj-Run"+GetRunNumber(),".SvtGainCal.root"); 
    }
  //hfile  = new TFile(FileName.c_str(),"RECREATE","Demo ROOT file");
  CreateHist(mTotalNumberOfHybrids);

  GetBadAnodes();

  GetSvtPedestals();

  gMessMgr->Info()<< " StSvtSeqAdjMaker-info:"<<endm;
  gMessMgr->Info() << "     PedOffSet = "<<mPedOffSet<<endm;
  gMessMgr->Info() << "    thresh_lo = "<<m_thresh_lo <<endm;
  gMessMgr->Info() << "     thresh_hi = "<<m_thresh_hi <<endm;
  gMessMgr->Info() << "     n_seq_lo  = "<<m_n_seq_lo <<endm;
  gMessMgr->Info() << "     n_seq_hi  = "<< m_n_seq_hi <<endm;
  gMessMgr->Info() << "    inv_prod_lo  = "<<m_inv_prod_lo <<endm;

  return  kStOK;  
}

//____________________________________________________________________________


Int_t StSvtSeqAdjMaker::GetSvtRawData()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtRawData");
  if( !dataSet) {
    gMessMgr->Warning() << " No Svt Raw data set" << endm;
    return kStWarn;
  }

  mSvtRawData = (StSvtData*)(dataSet->GetObject());
  if( !mSvtRawData) {
    gMessMgr->Warning() << " No Svt Raw data " << endm;
    return kStWarn;
  }
  return kStOK;
}

//____________________________________________________________________________


Int_t StSvtSeqAdjMaker::GetSvtPedestals()
{
  if (mPedFile) {
    StSvtPedMaker* sdaq = (StSvtPedMaker*)GetMaker("SvtPed");

    if( sdaq)
      sdaq->ReadFromFile( mPedFile);
  }

  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtPedestal");

  // get pedestals and set pedestal subtraction ONLY if there is a pedestal data set
  if (dataSet) {
    mSvtPedColl = (StSvtHybridCollection*)(dataSet->GetObject());
    mSvtPedSub = new StSvtPedSub(mSvtPedColl);
  
    //Set offset
    mSvtRawData->setPedOffset(mPedOffSet);
  }

  return kStOK;
}

//__________________________________________________________________________


Int_t  StSvtSeqAdjMaker::SetSvtData()
{
  mSvtDataSet = new St_ObjectSet("StSvtData");
  AddData(mSvtDataSet);  

  mSvtAdjData = new StSvtData(*mSvtRawData);
  //cout<<"mSvtAdjData  = "<<mSvtAdjData<<endl;
  mSvtDataSet->SetObject((TObject*)mSvtAdjData); 
  // assert(mSvtAdjData);

  return kStOK;
}

//__________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetMinAdcLevels( int MinAdc1,  int MinAbove1,
				         int MinAdc2,   int MinAbove2, int PedOffset){

  m_thresh_lo = MinAdc1+PedOffset;
  m_thresh_hi = MinAdc2+PedOffset; 
  m_n_seq_lo  = MinAbove1;
  m_n_seq_hi  = MinAbove2;

  //if (PedOffset)
    mPedOffSet = PedOffset;

  return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetPedestalFile(const char* pedFile)
{
  if(Debug()) gMessMgr->Debug() << "opening file called " << pedFile << " " 
                               << GetName() << endm;  

  mPedFile = pedFile;

  return kStOK;
}


//____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetLowInvProd(int LowInvProd)
{
  m_inv_prod_lo = LowInvProd;
  
  return kStOK; 
}

//___________________________________________________________________________

Int_t StSvtSeqAdjMaker::GetBadAnodes()
{

  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtBadAnodes");
  if( !dataSet) {
    gMessMgr->Warning() << " No Svt Bad Anodes data set" << endm;
    return kStWarn;
  }

  mSvtBadAnodes = (StSvtHybridCollection*)(dataSet->GetObject());
  if( !mSvtBadAnodes) {
    gMessMgr->Warning() << " No Svt Bad Anodes data " << endm;
    return kStWarn;
  }
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::CreateHist(Int_t tNuOfHyb)
{
   // Create Histograms

  
   //mInvProdSeqAdj= new TH1D*[tNuOfHyb];
   mRawAdc = new TH1F*[tNuOfHyb];
   mAdcAfter = new TH1F*[tNuOfHyb];
   //mTimeAn = new TH2F*[tNuOfHyb];

   char  RawTitle[25], AdcAfterTitle[25], TimeAnTitle[25];
   char  Index[4];
   char* RawTitle_cut;
   char* AdcAf_cut;
   char* TimeAnCh;
  for (int barrel = 1;barrel <= mSvtRawData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtRawData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtRawData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtRawData->getNumberOfHybrids();hybrid++) {
           
            int index = mSvtRawData->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
            sprintf(RawTitle,"RawAdcIn");
 	    sprintf(AdcAfterTitle,"numOfhitsIn");
	    sprintf(TimeAnTitle,"TimeAn");
            sprintf(Index,"%d", index);
            RawTitle_cut = strcat(RawTitle,Index);
	    AdcAf_cut = strcat(AdcAfterTitle,Index);
	    TimeAnCh = strcat(TimeAnTitle,Index);

	    mRawAdc[index] = new TH1F(RawTitle_cut,"Raw ADC for each anode",241,0,241);
	    mAdcAfter[index] = new TH1F(AdcAf_cut,"Number of hits in each anode",241,0,241);
	    //mTimeAn[index] = new TH2F(TimeAnCh,"Time vs Anode",240,0,240,128,0,128);
	}
      }
    }
  }
  mOcupancyHisto = new TH1F("OcupancyHisto","Anode Occupancy in an event",129,0,128);
  EventOccupancy = new TH1F("EventOccup","Event Occupancy",200,0,100000);

  return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSeqAdjMaker::Make()
{

  int Anode;

  StSvtHybridBadAnodes* BadAnode=NULL;

  //  GetBadAnodes();
  if ( GetSvtRawData() ) return kStWarn; // Return if SVT not there

  SetSvtData();

  // copy event information to adjusted data collection
//VP  (*mSvtAdjData) = (*mSvtRawData);  // already made in SetSvtData()

  gMessMgr->Info() << "Working On Event: " << mSvtAdjData->getEventNumber() << endm;

  
  Evt_counts=0;

  for(int Barrel = 1;Barrel <= mSvtRawData->getNumberOfBarrels();Barrel++){
    for (int Ladder = 1;Ladder <= mSvtRawData->getNumberOfLadders(Barrel);Ladder++){
      for (int Wafer = 1;Wafer <= mSvtRawData->getNumberOfWafers(Barrel);Wafer++){
	for( int Hybrid = 1;Hybrid <=mSvtRawData->getNumberOfHybrids();Hybrid++){

          int index = mSvtRawData->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

	  if( index < 0) continue;

          mHybridRawData = (StSvtHybridData *)mSvtRawData->at(index);

	  // cout << "mHybridRawData = " << mHybridRawData << endl;
	  if( !mHybridRawData) continue;

	  // subtract pedestal from raw data and save pedestal subtracted data
	  //if( mSvtPedSub){
	  //  mSvtPedSub->SubtractPed(mHybridRawData, index, mPedOffSet);
	  //}

	  // retrieve bad anodes
	  if (mSvtBadAnodes)
	    BadAnode = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);

          mHybridAdjData = (StSvtHybridData *)mSvtAdjData->at(index);
	  mSvtAdjData->put_at(0,index);
	  delete mHybridAdjData;
	  mHybridAdjData = new StSvtHybridData(Barrel,Ladder,Wafer,Hybrid); 
	  mHybridAdjData->setTimeZero(mHybridRawData->getTimeZero());
	  mHybridAdjData->setSCAZero(mHybridRawData->getSCAZero());

	  doCommon = 1;
	  mNAnodes = FindBlackAnodes();

	  if( doCommon || !mNAnodes ){
	    if (Debug())   gMessMgr->Debug() << "Doing Common mode average for index " << index << endl;

	    // Zero Common Mode Noise arrays
	    for(int Timebin=0; Timebin<128; Timebin++){
	      mCommonModeNoise[Timebin]=0;
	      mCommonModeNoiseAn[Timebin]=0;	
	    } 
	  }
	  for( int iAnode= 0; iAnode< mHybridRawData->getAnodeList(anolist); iAnode++)
	   {
	     Anode = anolist[iAnode];  

	  // here anode is real anode number (1-240)
	  //if (Debug())MakeHistogramsAdc(mHybridRawData,index,Anode,1);
	     if( doCommon)  CommonModeNoiseCalc(iAnode);
	   }
	 
	  if( doCommon){
	    int TimeLast, TimeAv, TimeSum, TimeAvSav;
	    TimeLast = 0;
	    TimeSum = 0;
	    TimeAv=0;
	    TimeAvSav = 0;
	    for( int TimeBin=0; TimeBin<128; TimeBin++){
	      if(mCommonModeNoiseAn[TimeBin] > 30)
		mCommonModeNoise[TimeBin] /= mCommonModeNoiseAn[TimeBin];
	      else  mCommonModeNoise[TimeBin] =0;
	      
	      if( Debug()){
		if( index < 4 && mCommonModeNoiseAn[TimeBin] > 20){
		  if( TimeLast < TimeBin-3 && TimeSum > 0){
		    
		    TimeAv /= TimeSum;
		    TimeLast = TimeBin;
		    TimeAvSav = TimeAv;
		    TimeAv = 0;
		    TimeSum=0;
		  }
		  else{
		    TimeAv +=TimeBin;
		    TimeSum++;
		    TimeLast = TimeBin;
		  }
		}
	      } // End of if If(Debug)
	    }
	  } // End of If(Common)
	
	  for( int iAnode= 0; iAnode<mHybridRawData->getAnodeList(anolist); iAnode++)
            {
	      Anode = anolist[iAnode];

	      // Skip Bad anodes
	      //
	      if( BadAnode && BadAnode->isBadAnode(Anode)) continue;

	      if( doCommon) CommonModeNoiseSub(iAnode);
	      else SubtractFirstAnode(iAnode);
	      //if (Debug() && !doCommon)MakeHistogramsAdc(mHybridRawData,index,Anode,1);
	      //Perform Asic like zero suppression
	      AdjustSequences1(iAnode, Anode);
	      
	      //Perform E896 type zero-suppresion (look for non-noise like signals
	      if(m_inv_prod_lo){
		mInvProd->FindInvProducts(mHybridAdjData,iAnode,mPedOffSet);
		// if(Debug())MakeHistogramsProb(index,Anode);
		AdjustSequences2(iAnode, Anode);

	      }
	      // Here anode is index to real anode number (1-240)
	      MakeHistogramsAdc(mHybridRawData,index,Anode,2);
	    }
	  

	  mHybridAdjData->setAnodeList();
	  mSvtAdjData->put_at(mHybridAdjData,index);
	}

	mInvProd->ResetBuffer();
      }
    }
  }
  cout << endl;
  cout << " Event Occupancy = " << Evt_counts << endl;

  EventOccupancy->Fill((float)Evt_counts);
  Evt_counts=0;

  return kStOK;

}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences1(int iAnode, int Anode){

  //Perform ASIC like zero suppression. Need > m_n_seq_lo pixels with a count
  // > than m_thresh_lo and  > m_n_seq_hi pixels with a count
  // > than m_thresh_hi  

  int  nSeqOrig, nSeqNow, nSeqTruth, length, count1, count2;
  int startTimeBin,  status;
  StSequence* Sequence;
  StMCTruth*   SeqTruth;
  unsigned char* adc;
  //int ExtraBefore = 1;
  // int ExtraAfter = 3;
  int ExtraBefore = 0;
  int ExtraAfter = 0;
  int firstTimeBin;

  //Anode is the index into the anolist array
   status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);
   status= mHybridRawData->getListTruth     (iAnode,nSeqTruth,SeqTruth );
  //status= mHybridRawData->getSequences(Anode,nSeqOrig,Sequence);

  nSeqNow = 0;
  StSequence tempSeq1[128];
  StMCTruth  tempTru1[128];
    for( int nSeq=0; nSeq< nSeqOrig ; nSeq++){
    
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin;
    
    int j =0;
    while( j<length){
      count1=0;
      count2=0;
      
      while( (int)adc[j] > m_thresh_lo && j<length){
	count1++;

	if( (int)adc[j] > m_thresh_hi){
	  count2++;
	}

	j++;
      }

      if( count2 > m_n_seq_hi && count1 > m_n_seq_lo){

	firstTimeBin = startTimeBin + j - count1 - ExtraBefore;

	tempSeq1[nSeqNow].firstAdc=&adc[j- count1 - ExtraBefore];
	tempSeq1[nSeqNow].startTimeBin = firstTimeBin;
	tempSeq1[nSeqNow].length = 0;
	if((startTimeBin + j - count1 - ExtraBefore)  < 0){
	  tempSeq1[nSeqNow].startTimeBin=0;
	  tempSeq1[nSeqNow].firstAdc=&adc[0];
	  // Make length temp. negative to account for the fact we went
	  //past the start of the sequence get adjusted properly in a bit
	  tempSeq1[nSeqNow].length = startTimeBin + j - count1 - ExtraBefore;
	}

	if((startTimeBin + j - count1 - ExtraBefore)  < startTimeBin){
	  tempSeq1[nSeqNow].startTimeBin=startTimeBin;
	  tempSeq1[nSeqNow].firstAdc=&adc[0];
	  // Make length temp. negative to account for the fact we went
	  //past the start of the sequence get adjusted properly in a bit
	  tempSeq1[nSeqNow].length = startTimeBin + j - count1 - ExtraBefore
	    -startTimeBin;
	}

	// Make length proper size even if it went negative
	tempSeq1[nSeqNow].length += count1+ ExtraAfter+ ExtraBefore;

	// Check dont go past end of sequence if do adjust length correctly
	if( tempSeq1[nSeqNow].length + tempSeq1[nSeqNow].startTimeBin  > 128) 
	  tempSeq1[nSeqNow].length=128-tempSeq1[nSeqNow].startTimeBin +1;
	if ( tempSeq1[nSeqNow].startTimeBin+ tempSeq1[nSeqNow].length >
	     startTimeBin +length){
	  tempSeq1[nSeqNow].length =  (startTimeBin +length) -
	    tempSeq1[nSeqNow].startTimeBin ;
	}
	if (SeqTruth) tempTru1[nSeqNow]=SeqTruth[nSeq];
	nSeqNow++;
      }
      
      
      j++;
    }
  }

  mNumOfSeq=0;
  if( nSeqNow > 0){
  if (SeqTruth) SeqTruth=tempTru1;
  mNumOfSeq = MergeSequences(tempSeq1,nSeqNow,SeqTruth);
  }

  mHybridAdjData->setListSequences(iAnode, Anode,mNumOfSeq, tempSeq1);
  if (SeqTruth)
  mHybridAdjData->setListTruth    (iAnode, Anode,mNumOfSeq, tempTru1);
  
  //if (nSeqNow)
  //  cout << "For Anode=" << Anode << " Number sequnces was=" << nSeqOrig << " Number now=" << nSeqNow << endl;
  
  //  data << endl;
  
  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences2(int iAnode, int Anode){

  //Perform E896 like zero suppression. Find pixels that have consecutive ADC 
  // counts that do not have the shape of noise

  int nSeqBefore, nSeqNow, count1, nSeqTruth;
  int startTimeBin, length, status;
  StSequence* Sequence;
  StMCTruth*  SeqTruth;
  unsigned char* adc;
  //int ExtraBefore=1;
  //int ExtraAfter=3;
 int ExtraBefore=0;
  int ExtraAfter=0;

  int firstTimeBin;
  
  double tempBuffer = 0;
  
  nSeqNow = 0;
  StSequence tempSeq1[128];
  StMCTruth  tempTru1[128];
  status = mHybridAdjData->getListSequences(iAnode,nSeqBefore,Sequence);
  status = mHybridRawData->getListTruth    (iAnode,nSeqTruth ,SeqTruth );
  
  for(int Seq = 0; Seq < nSeqBefore; Seq++) 
    {
      startTimeBin =Sequence[Seq].startTimeBin; 
      length = Sequence[Seq].length;
      adc = Sequence[Seq].firstAdc;
      
      int j=0;
      while( j < length)
	{ 
	  count1 = 0;
	  tempBuffer = mInvProd->GetBuffer(startTimeBin + j);
	  
	  while(tempBuffer > m_inv_prod_lo && j < length)
	    {
	      ++count1;
	      ++j;
	      tempBuffer = mInvProd->GetBuffer(startTimeBin + j);
	    }
	  if(count1 > 0  && (tempBuffer < m_inv_prod_lo || j == length))
	    {
	      firstTimeBin = startTimeBin + j - count1 - ExtraBefore;
	      
	      tempSeq1[nSeqNow].firstAdc=&adc[j- count1 - ExtraBefore];
	      tempSeq1[nSeqNow].startTimeBin = firstTimeBin;
	      tempSeq1[nSeqNow].length = 0;
	      if((startTimeBin + j - count1 - ExtraBefore)  < 0){
		tempSeq1[nSeqNow].startTimeBin=0;
		tempSeq1[nSeqNow].firstAdc=&adc[0];
		// Make length temp. negative to account for the fact we went
		//past the start of the sequence get adjusted properly in a bit
		tempSeq1[nSeqNow].length = startTimeBin + j - count1 - ExtraBefore;
	      }
	      
	      if((startTimeBin + j - count1 - ExtraBefore)  < startTimeBin){
		tempSeq1[nSeqNow].startTimeBin=startTimeBin;
		tempSeq1[nSeqNow].firstAdc=&adc[0];
		// Make length temp. negative to account for the fact we went
		//past the start of the sequence get adjusted properly in a bit
		tempSeq1[nSeqNow].length = startTimeBin + j - 
		  count1 - ExtraBefore -startTimeBin;
	      }
	      
	      // Make length proper size even if it went negative
	      tempSeq1[nSeqNow].length += count1+ ExtraAfter+ ExtraBefore;
	      
	      // Check dont go past end of sequence if do adjust length correctly
	      if( tempSeq1[nSeqNow].length + tempSeq1[nSeqNow].startTimeBin  > 128) 
		tempSeq1[nSeqNow].length=128-tempSeq1[nSeqNow].startTimeBin +1;
	      if ( tempSeq1[nSeqNow].startTimeBin+ tempSeq1[nSeqNow].length >
		   startTimeBin +length){
		tempSeq1[nSeqNow].length =  (startTimeBin +length) -
		  tempSeq1[nSeqNow].startTimeBin ;
	      }
	      if(SeqTruth) tempTru1[nSeqNow] = SeqTruth[Seq];
	      nSeqNow++;
	    
	    }
	  j++;
	}
	  
    } // Sequence loop
  
  
  if( nSeqBefore >0){
    if (SeqTruth) SeqTruth = tempTru1;
    mNumOfSeq = MergeSequences(tempSeq1, nSeqNow,SeqTruth);
    mHybridAdjData->setListSequences(iAnode, Anode,mNumOfSeq, tempSeq1);
    if (SeqTruth)
    mHybridAdjData->setListTruth    (iAnode, Anode,mNumOfSeq, tempTru1);
  }
  //cout << "For Anode=" << Anode << " Number of sequnces was=" << nSeqBefore << " Number now=" << nSeqNow << endl;
  
  return kStOK;

}
//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::MergeSequences( StSequence* seq, int nSeq, StMCTruth* tru){

  // Check and see if the end of one sequence overlaps the start of the next
  // if it does merge sequences

  int nSeqNow = 0;
  int EndTime;
  StMCPivotTruth pivo(1);
  if (tru) pivo.Add(tru[0]);
  for( int i=1; i<nSeq; i++){
    
    if( (seq[nSeqNow].startTimeBin + seq[nSeqNow].length) 
	>= seq[i].startTimeBin){
      EndTime = seq[i].startTimeBin + seq[i].length;
      seq[nSeqNow].length = EndTime - seq[nSeqNow].startTimeBin;
      if (tru) { pivo.Add(tru[i]); tru[nSeqNow] = pivo.Get();}
      
    }
    else{
      nSeqNow++;
      seq[nSeqNow] = seq[i];
      if (tru){ pivo.Reset(); pivo.Add(tru[i]);} 
    }

  }

  return nSeqNow+1;
}
//_____________________________________________________________________________

void StSvtSeqAdjMaker::CommonModeNoiseCalc(int iAnode){

  // Calc common mode noise

  int  nSeqOrig, length;
  int startTimeBin,  status;
  StSequence* Sequence;
  unsigned char* adc;

  //Anode is the index into the anolist array
  
  status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);

  for( int nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin;

    int j =0;
    while( j<length){
      
      mCommonModeNoise[j+startTimeBin] += (int)adc[j] - mPedOffSet;
      mCommonModeNoiseAn[j+startTimeBin]++;
      j++;
 

    }
  }
  return;
}
//_____________________________________________________________________________

void StSvtSeqAdjMaker::CommonModeNoiseSub(int iAnode){

  // Calc common mode noise

  int  nSeqOrig, length;
  int startTimeBin,  status;
  StSequence* Sequence;
  unsigned char* adc;

  //Anode is the index into the anolist array
  
  status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);

  for( int nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin;

    int j =0;
    while( j<length){
      if( (int) adc[j]-mCommonModeNoise[j+startTimeBin] < 0) adc[j]=0;
      else{
	adc[j] -=mCommonModeNoise[j+startTimeBin];
      }
      j++;
    }
  }
  return;
}

//_____________________________________________________________________________

void StSvtSeqAdjMaker::SubtractFirstAnode(int iAnode){

  // Calc common mode noise

  int  nSeq, nSeqOrig, length;
  int startTimeBin,  status, j;
  int adcMean;
  StSequence* Sequence;
  unsigned char* adc;


  status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);

  adcMean = 0;
  for( nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin; 
    j =0;
    while( j<length){

     if( mNAnodes) adcMean = adcCommon[j+startTimeBin]/mNAnodes;

     //cout << "iAnode = " << iAnode << "ADC value is: " << (int) adc[j] << " for time = " << startTimeBin + j << " and ADCMean = "<< adcMean << " mNAnodes = " << mNAnodes <<endl;
      if( (int)adc[j]- adcMean < 0) adc[j]=0;
      else if( adcMean < 0){
	adc[j]	+= (unsigned char)(-1*adcMean);
      }
      else{
	adc[j] -= (unsigned char)adcMean;
      }
      //cout << "Final ADC= " << (int)adc[j] << endl;
      j++;
    }
  }
  return;
}
//____________________________________________________________________________

int StSvtSeqAdjMaker::FindBlackAnodes(){
  // Decide whether to do common mode noise from average timebucket 
  //value or via first two black anodes.Have many options andoes 1&240,
  //1&2 or 2&239. IN some case one of the black anodes is bad so only 
  //subtract one.
  int status, length;
  int i, j, startTimeBin;
  int nSequence = 0;
  StSequence* Seq = NULL;
  unsigned char *adc;
  
  doCommon = 0;
  mNAnodes=2;
  int adcAv=0;
  
  status= mHybridRawData->getSequences(240,nSequence,Seq);
  length = 0;
  for(j=0; j<128; j++) adcCommon[j]=0;
  for( i=0; i<nSequence; i++)  length += Seq[i].length;
  if( length < 126){
    // IF anode 240 isnt black try 239
    status= mHybridRawData->getSequences(239,nSequence,Seq);
    length = 0;
    
    for( i=0; i<nSequence; i++)  length += Seq[i].length;
    if( length < 126){
      //if that one isnt black either try 2
      status= mHybridRawData->getSequences(2,nSequence,Seq);
      length = 0;
      
      for( i=0; i<nSequence; i++)  length += Seq[i].length;
      if( length < 126){
	// If that isnt black too do common mode noise subtraction
	doCommon =1;
      }
    }
  }
  
  // If one of those anodes was black fill adc array
  if( !doCommon){
    
    // Fill in array of adc values of first anode
    for( int nSeq=0; nSeq< nSequence ; nSeq++){
      
      adc=Seq[nSeq].firstAdc;
      length = Seq[nSeq].length;
      startTimeBin=Seq[nSeq].startTimeBin;
      j=0;
      while( j<length){
	adcCommon[startTimeBin+j] = adc[j]-mPedOffSet;
	adcAv += (int) adc[j];
	j++;
      }
    }
    // If this black anode was bad dont count it in subtraction
    if( adcAv <100 || adcAv> 25000) {
      mNAnodes--;
      for(j=0; j<128; j++) adcCommon[j]=0;
    }
  }
  
  // now find second black anode
  
  status= mHybridRawData->getSequences(2,nSequence,Seq);
  length = 0;
  
  for( i=0; i<nSequence; i++)  length += Seq[i].length;
  if( length < 126){
    status= mHybridRawData->getSequences(1,nSequence,Seq);
    length = 0;	  
    
    for( i=0; i<nSequence; i++)  length += Seq[i].length;
    if( length < 126){
      doCommon=1;
    }
    
  }
  if( !doCommon){
    // Fill in array of adc values of second anode
    adcAv=0;
    for( int nSeq=0; nSeq< nSequence ; nSeq++){
      
      adc=Seq[nSeq].firstAdc;
      length = Seq[nSeq].length;   
      startTimeBin=Seq[nSeq].startTimeBin;
      j=0;
      while( j<length){
	adcCommon[startTimeBin+j] += adc[j]-mPedOffSet;
	adcAv += (int)  adc[j];
	j++;
      }
    }

    if( adcAv <100 || adcAv> 25000) {
      mNAnodes--;
      // If black anode is bad subtract back of wrong adc values
      // from average will subtract from all other anodes
      if( mNAnodes){
	for( int nSeq=0; nSeq< nSequence ; nSeq++){
	  
	  adc=Seq[nSeq].firstAdc;
	  length = Seq[nSeq].length;
	  startTimeBin=Seq[nSeq].startTimeBin;
	  j=0;
	  while( j<length){
	    adcCommon[startTimeBin+j] -= adc[j]-mPedOffSet;
	    adcAv += (int)  adc[j];
	    j++;
	  }
	}
      }
    }
  }
  return mNAnodes;
}

//_____________________________________________________________________________
void StSvtSeqAdjMaker::MakeHistogramsAdc(StSvtHybridData* hybridData, int index, int Anode, int Count){
  
  // Count is 1 for before CM-Noise subtraction and 2 for after CMN subtraction.
  int mSequence;
  int len,status;
  unsigned char* adc;

  StSequence* svtSequence;
  mSequence = 0;
  svtSequence = NULL;

  // Anode goes between 1-240
  
  status = hybridData->getSequences(Anode,mSequence,svtSequence);
  
  int numOfPixels=0;
  for(int mSeq = 0; mSeq < mSequence; mSeq++) 
    { 
      adc = svtSequence[mSeq].firstAdc;
      len = svtSequence[mSeq].length;
      if (len>12) continue;
      for(int j = 0 ; j < len; j++){
	mRawAdc[index]->Fill(Anode,(int)adc[j]);
	//mTimeAn[index]->Fill(Anode,j+svtSequence[mSeq].startTimeBin,(int)adc[j]);
	mAdcAfter[index]->Fill(Anode);
	//cout << "Index= " << index << " Anode " << Anode << " " << (int)adc[j] - mPedOffSet << " count= " << Evt_counts <<endl;
	if (adc[j] >0) numOfPixels++;
	if (adc[j] >0) Evt_counts++;
      }
    }
  mOcupancyHisto->Fill(numOfPixels);
}

//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Reset(){

  //delete mInvProd;
  //delete mProbValue;

  mSvtDataSet = NULL;
  mSvtAdjData = NULL;
  mSvtRawData = NULL;
  mHybridRawData = NULL;
  mHybridAdjData = NULL;
  mSvtPedSub = NULL;
  mSvtPedColl = NULL;
  mSvtBadAnodes = NULL;
  mInvProd = NULL;
  mProbValue = NULL;
  
  return kStOK;
}
//_____________________________________________________________________________
string StSvtSeqAdjMaker::baseName(string s){

  string name(s);
  size_t pos;
  pos = name.find_last_of("/");
  if (pos!=string::npos ) name.erase(0, pos );
  pos = name.find_first_of(".");
  if (pos!=string::npos ) name.erase(pos,name.length()-pos );
  return name;
} 
//_____________________________________________________________________________
string StSvtSeqAdjMaker::buildFileName(string dir, string fileName, string extention){

  fileName = dir + fileName + extention;
  while (fileName.find("//")!=string::npos) {
    int pos = fileName.find("//");
    fileName.erase(pos,1);
  }
  return fileName;
}
//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Finish(){
  
  
  if (Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Finish() "
				 << GetName() << endm; 

  if ( hfile ){
    hfile->Write();
    hfile->Close();
  }
  
  return kStOK;
}

//_____________________________________________________________________________
ClassImp(StSvtSeqAdjMaker)
