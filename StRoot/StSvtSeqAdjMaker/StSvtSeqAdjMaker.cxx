 /***************************************************************************
 *
 * $Id: StSvtSeqAdjMaker.cxx,v 1.37 2002/01/11 22:49:15 caines Exp $
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

#include "StChain.h"
#include "TH1.h"
#include "TH2.h"
#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtInverseProducts.hh"
#include "StSvtProbValues.hh"
#include "StSvtPedSub.h"
#include "StSvtBadAnode.hh"
#include "StSvtSeqAdjMaker.h"
#include "StSvtCalibMaker/StSvtPedMaker.h"
#include "StMessMgr.h"

#include "St_DataSetIter.h"
#include "TObjectSet.h"

#include <iostream.h>
#include <fstream.h>

int* anolist; 

//___________________________________________________________________________________________
StSvtSeqAdjMaker::StSvtSeqAdjMaker(const char *name) : StMaker(name)
{

  mSvtDataSet = NULL;
  mSvtAdjData = NULL;
  mSvtRawData = NULL;
  mSvtBadAnodes = NULL;
  mSvtBadAnodeSet = NULL;
  mHybridRawData = NULL;
  mHybridAdjData = NULL;
  mSvtPedSub = NULL;
  anolist = NULL;
  mInvProd = NULL;
  mProbValue = NULL;
  tempSeq1 = new StSequence[128];


  // Set up some defaults

  mPedFile = NULL;
  mPedOffSet = 10;
  m_thresh_lo = 3+mPedOffSet;
  m_thresh_hi = 5+mPedOffSet; 
  m_n_seq_lo  = 2;
  m_n_seq_hi  = 0;
  m_inv_prod_lo = 0;
 
}

//____________________________________________________________________________________________
StSvtSeqAdjMaker::~StSvtSeqAdjMaker(){
           
}

//_____________________________________________________________________________________________
Int_t StSvtSeqAdjMaker::Init()
{

  if(Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Init() ... "
                               << GetName() << endm; 

  GetSvtRawData();
  
  SetSvtData();

  mTotalNumberOfHybrids = mSvtRawData->getTotalNumberOfHybrids();
  if( Debug())CreateHist(mTotalNumberOfHybrids);	    
  
  GetSvtPedestals();

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
	    
	    //gMessMgr->Warning() << "No sigma set for hybrid index " << index << " using defaut sigma = " << sigma << endm;
	    mProbValue->SetProbValue(sigma);    
	    mInvProd->SetProbTable(mProbValue);
	}
      }
    }
  }

  GetBadAnodes();
  
  return  StMaker::Init();  
}

//_____________________________________________________________________________


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

//__________________________________________________________________________________________________


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

//__________________________________________________________________________________________________


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

//____________________________________________________________________________

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

//_______________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetPedestalFile(const char* pedFile)
{
  if(Debug()) gMessMgr->Debug() << "opening file called " << pedFile << " " 
                               << GetName() << endm;  

  mPedFile = pedFile;

  return kStOK;
}


//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetLowInvProd(int LowInvProd)
{
  m_inv_prod_lo = LowInvProd;
  
  return kStOK; 
}

////_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::GetBadAnodes()
{

  fstream BadAnodeFile;
  int index;
  StSvtBadAnode* mHybridBadAnodeData;

  mSvtBadAnodeSet = new St_ObjectSet("SvtBadAnodeSet");
  AddConst(mSvtBadAnodeSet);  

  mSvtBadAnodes = new StSvtHybridCollection(mSvtRawData->getConfiguration());

  mSvtBadAnodeSet->SetObject((TObject*)mSvtBadAnodes); 
  //assert(mSvtBadAnodes);


  if(  !strncmp(mSvtRawData->getConfiguration(), "FULL", strlen("FULL"))){

    //read in bad anodes from file

    //    BadAnodeFile.open("StarDb/svt/HotAnodeList.txt",ios::in);
    ///int barrel, ladder, wafer, hybrid, anode, anode2;
    
 //    while( !BadAnodeFile.eof()){
      
//       BadAnodeFile >> barrel >> ladder >> wafer >> hybrid >> anode;
//       index = mSvtRawData->getHybridIndex(barrel,ladder,wafer,hybrid);
//       mHybridBadAnodeData = new StSvtBadAnode();
//       mSvtBadAnodes->put_at(mHybridBadAnodeData,index);


//       if( anode !=1){
// 	if( anode > 0) mHybridBadAnodeData->SetBadAnode(anode, 1);
// 	else{
// 	  anode *= -1;
// 	  BadAnodeFile >> anode2;
// 	  {for( int i=anode; i<anode2; i++){
// 	    mHybridBadAnodeData->SetBadAnode(i, 1);}
// 	  }
// 	}
//       }
//       cout << " Bad Anode index !!!!!!!!!!!!! = " << index;    
//     }
   //Put in bad Anodes by hand
    
    // L01B1-D3H1 anode 159 to 160    
    index = mSvtRawData->getHybridIndex(1,1,3,1);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=159; i<161; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L03B1-D2H2 anode 33 to 208    
    index = mSvtRawData->getHybridIndex(1,3,2,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=33; i<209; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L06B1-D3H1 anode 81 to 160    
    index = mSvtRawData->getHybridIndex(1,6,3,1);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=81; i<161; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L05B2-D1H2 anode 1 to 80    
    index = mSvtRawData->getHybridIndex(2,5,1,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=1; i<81; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L07B2-D4H2
    index = mSvtRawData->getHybridIndex(2,7,4,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=113; i<130; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    {for( int i=193; i<210; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    
    
    // L07B2-D6H2
    index = mSvtRawData->getHybridIndex(2,7,6,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=1; i<81; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    
    
    // L08B2-D2H2
    index = mSvtRawData->getHybridIndex(2,8,2,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=177; i<193; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L09B2-D1H2
    index = mSvtRawData->getHybridIndex(2,9,1,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=17; i<33; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L09B2-D3H2
    index = mSvtRawData->getHybridIndex(2,9,3,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=129; i<145; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L10B2-D3H2
    index = mSvtRawData->getHybridIndex(2,10,3,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=81; i<161; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L3B3-D1H2 anode 199
    index = mSvtRawData->getHybridIndex(3,3,1,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {mHybridBadAnodeData->SetBadAnode(199, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L3B3-D3H2 Entire hybrid
    index = mSvtRawData->getHybridIndex(3,3,3,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=1; i<241; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L4B3-D4H2 anode 215 to 216
    index = mSvtRawData->getHybridIndex(3,4,4,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=215; i<217; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L5B3-D3H2 anode 129 to 144
    index = mSvtRawData->getHybridIndex(3,5,3,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=129; i<145; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L6B3-D1H2 anode 113 and anode 128
    index = mSvtRawData->getHybridIndex(3,6,1,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {mHybridBadAnodeData->SetBadAnode(113, 1);
     mHybridBadAnodeData->SetBadAnode(128, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L10B3-D7H2 anode 88
    index = mSvtRawData->getHybridIndex(3,10,7,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {mHybridBadAnodeData->SetBadAnode(88, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L11B3-D1H1 anode 177
    index = mSvtRawData->getHybridIndex(3,11,1,1);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {mHybridBadAnodeData->SetBadAnode(177, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;    

    // L14B3-D4H2
    index = mSvtRawData->getHybridIndex(3,14,4,2);
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    {for( int i=1; i<81; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);}
    cout << "Bad Anode index !!!!!!!!!!!!! = " << index;   
  }
  return kStOK; 
}
//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::CreateHist(Int_t tNuOfHyb)
{
   // Create Histograms

  
   mInvProdSeqAdj= new TH1D*[tNuOfHyb];
   mRawAdc = new TH1F*[tNuOfHyb];
   mAdcAfter = new TH1F*[tNuOfHyb];
   mTimeAn = new TH2F*[tNuOfHyb];
 
   char invProdTitle_cut[25], RawTitle[25], AdcAfterTitle[25], TimeAnTitle[25];
   char  Index[4];
   char* prodTitle_cut;
   char* RawTitle_cut;
   char* AdcAf_cut;
   char* TimeAnCh;
  for (int barrel = 1;barrel <= mSvtRawData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtRawData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtRawData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtRawData->getNumberOfHybrids();hybrid++) {
           
            int index = mSvtRawData->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
            sprintf(invProdTitle_cut,"InvProdSeqAdj"); 
            sprintf(RawTitle,"RawAdcIn");
 	    sprintf(AdcAfterTitle,"AdcAfterCuts");
	    sprintf(TimeAnTitle,"TimeAn");
            sprintf(Index,"%d", index);
            prodTitle_cut = strcat(invProdTitle_cut,Index);
            RawTitle_cut = strcat(RawTitle,Index);
	    AdcAf_cut = strcat(AdcAfterTitle,Index);
	    TimeAnCh = strcat(TimeAnTitle,Index);

	    mInvProdSeqAdj[index] = new TH1D(prodTitle_cut,"freqOfInvProd vs log10 of InvProd After Seq Adusting",100,0.,30.);
	    mRawAdc[index] = new TH1F(RawTitle_cut,"freq Of Adc Values Before Seq Adjusting",150,-50.,100.);
	    mAdcAfter[index] = new TH1F(AdcAf_cut,"freq Of Adc Values After Seq Adjusting",150,-50.,100.);
	    mTimeAn[index] = new TH2F(TimeAnCh,"Time vs Anode",240,0,240,128,0,128);
	}
      }
    }
  }

  mCommonModePitch = new TH1F("ComModPitch","Pitch of Common Mode Noise",120,0,120);
  mCommonModeCount = new TH1F("comCount","Anodes that fire",240,-0.5,240.5);

  return kStOK;
}
//_______________________________________________________________________________________________

Int_t StSvtSeqAdjMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << " In StSvtSeqAdjMaker::Make()" << GetName() << endm; 

  int Anode;

  StSvtBadAnode* BadAnode=NULL;

  //  GetBadAnodes();
  if ( GetSvtRawData() ) return kStWarn; // Return if SVT not there

  SetSvtData();

  // copy event information to adjusted data collection
  (*mSvtAdjData) = (*mSvtRawData);

  gMessMgr->Info() << "Working On Event: " << mSvtAdjData->getEventNumber() << endm;

  for(int Barrel = 1;Barrel <= mSvtRawData->getNumberOfBarrels();Barrel++) {    
    for (int Ladder = 1;Ladder <= mSvtRawData->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mSvtRawData->getNumberOfWafers(Barrel);Wafer++) {
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
	    BadAnode = (StSvtBadAnode*)mSvtBadAnodes->at(index);

          mHybridAdjData = (StSvtHybridData *)mSvtAdjData->at(index);
	  if (mHybridAdjData)
	    delete mHybridAdjData;
	  mHybridAdjData = new StSvtHybridData(Barrel,Ladder,Wafer,Hybrid); 
	  mHybridAdjData->setTimeZero(mHybridRawData->getTimeZero());
	  mHybridAdjData->setSCAZero(mHybridRawData->getSCAZero());

	  mNAnodes = FindBlackAnodes();
	  if( doCommon || !mNAnodes ){
	    cout << "Doing Common mode average for index " << index << endl;

	    // Zero Common Mode Noise arrays
	    for(int Timebin=0; Timebin<128; Timebin++){
	      mCommonModeNoise[Timebin]=0;
	      mCommonModeNoiseAn[Timebin]=0;	
	    } 
	  }
	  for( int iAnode= 0; iAnode< mHybridRawData->getAnodeList(anolist); iAnode++)
            {
	      Anode = anolist[iAnode];


	      //if (Barrel == 1 && Ladder==1 && Wafer ==1 && Hybrid==1)
	      //	cout << "raw data, iAnode = " << iAnode << ", Anode = " << Anode << endl;

	      // here anode is real anode number (1-240)
	      if (Debug())MakeHistogramsAdc(mHybridRawData,index,Anode,1);
	      if( doCommon)  CommonModeNoiseCalc(iAnode);
	    }
	 
	  if( doCommon){
	    int TimeLast, TimeAv, TimeSum, TimeAvSav;
	    TimeLast = 0;
	    TimeSum = 0;
	    TimeAv=0;
	    TimeAvSav = 0;
	    for( int TimeBin=0; TimeBin<128; TimeBin++){
	      if(mCommonModeNoiseAn[TimeBin] > 20)
		mCommonModeNoise[TimeBin] /= mCommonModeNoiseAn[TimeBin];
	      else  mCommonModeNoise[TimeBin] =0;
	      
	      if( Debug()){
		mCommonModeCount->Fill(mCommonModeNoiseAn[TimeBin]);
		if( index < 4 && mCommonModeNoiseAn[TimeBin] > 20){
		  if( TimeLast < TimeBin-3 && TimeSum > 0){
		    
		    TimeAv /= TimeSum;
		    TimeLast = TimeBin;
		    mCommonModePitch->Fill(TimeAv-TimeAvSav);
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
	      }
	    }
	  }
	
	  for( int iAnode= 0; iAnode<mHybridRawData->getAnodeList(anolist); iAnode++)
            {
	      Anode = anolist[iAnode];

	      // Skip Bad anodes
	      //
	      if( BadAnode){
		if( BadAnode->IsBadAnode(Anode)){
		  continue;
		}
	      }

	      if( doCommon) CommonModeNoiseSub(iAnode);
	      else SubtractFirstAnode(iAnode);

	      //Perform Asic like zero suppression
	      AdjustSequences1(iAnode, Anode);
	      
	      //Perform E896 type zero-suppresion (look for non-noise like signals
	      if(m_inv_prod_lo){
		mInvProd->FindInvProducts(mHybridAdjData,iAnode,mPedOffSet);
		if(Debug())MakeHistogramsProb(index,Anode);
		AdjustSequences2(iAnode, Anode);

	      }
	      // Here anode is index to real anode number (1-240)
	      if(Debug())MakeHistogramsAdc(mHybridAdjData,index,Anode,2);
	    }
	  
	  mHybridAdjData->setAnodeList();
	  mSvtAdjData->put_at(mHybridAdjData,index);
	}
		
	mInvProd->ResetBuffer();
      }
    }
  }

  return kStOK;
  
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences1(int iAnode, int Anode){

  //Perform ASIC like zero suppression. Need > m_n_seq_lo pixels with a count
  // > than m_thresh_lo and  > m_n_seq_hi pixels with a count
  // > than m_thresh_hi  

  int  nSeqOrig, nSeqNow, length, count1, count2;
  int startTimeBin,  status;
  StSequence* Sequence;
  unsigned char* adc;
  int ExtraBefore = 1;
  int ExtraAfter = 3;
  int firstTimeBin, previousEndTimeBin;

  //Anode is the index into the anolist array
  
  status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);
  //status= mHybridRawData->getSequences(Anode,nSeqOrig,Sequence);

  nSeqNow = 0;
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

	//cout << (int) adc[j] << " " ;
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
	
	nSeqNow++;
      }
      
      
      j++;
    }
  }
   
  mNumOfSeq = MergeSequences(tempSeq1,nSeqNow);

  mHybridAdjData->setListSequences(iAnode, Anode,mNumOfSeq, tempSeq1);
  
  //if (nSeqNow)
  //cout << "For Anode=" << Anode << " Number sequnces was=" << nSeqOrig << " Number now=" << nSeqNow << endl;
  
  //  data << endl;
  
  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences2(int iAnode, int Anode){

  //Perform E896 like zero suppression. Find pixels that have consecutive ADC 
  // counts that do not have the shape of noise

  int nSeqBefore, nSeqNow, count1;
  int startTimeBin, length, status;
  StSequence* Sequence;
  unsigned char* adc;
  int ExtraBefore=1;
  int ExtraAfter=3;

  int firstTimeBin;
  
  double tempBuffer = 0;
  
  nSeqNow = 0;
  
  status = mHybridAdjData->getListSequences(iAnode,nSeqBefore,Sequence);
  
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
	      
	      nSeqNow++;
	    
	    }
	  j++;
	}
	  
    } // Sequence loop
  
  
  if( nSeqBefore >0){
    
    mNumOfSeq = MergeSequences(tempSeq1, nSeqNow);
    mHybridAdjData->setListSequences(iAnode, Anode,mNumOfSeq, tempSeq1);
  }
  //cout << "For Anode=" << Anode << " Number of sequnces was=" << nSeqBefore << " Number now=" << nSeqNow << endl;
  
  return kStOK;

}
//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::MergeSequences( StSequence* seq, int nSeq){

  // Check and see if the end of one sequence overlaps the start of the next
  // if it does merge sequences

  int nSeqNow = 0;
  int EndTime;

  for( int i=1; i<nSeq; i++){
    
    if( (seq[nSeqNow].startTimeBin + seq[nSeqNow].length) 
	>= seq[i].startTimeBin){
      EndTime = seq[i].startTimeBin + seq[i].length;
      seq[nSeqNow].length = EndTime - seq[nSeqNow].startTimeBin;
                }
    else{
      nSeqNow++;
      seq[nSeqNow].startTimeBin = seq[i].startTimeBin;
      seq[nSeqNow].length = seq[i].length;
      seq[nSeqNow].firstAdc = seq[i].firstAdc;
      
      
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
  float adcMean;
  StSequence* Sequence;
  unsigned char* adc;


  status= mHybridRawData->getListSequences(iAnode,nSeqOrig,Sequence);

  for( nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin; 
    j =0;
    while( j<length){

      adcMean = (float)adcCommon[j+startTimeBin]/mNAnodes;
      //adcMean = (float)adcSecond[j];

      //cout << "iAnode = " << iAnode << ", time = " << startTimeBin + j << ", adcFirst = " << (int)adcFirst[j] << ", adcSecond = " << (int)adcSecond[j] << ", adcMean = " << adcMean << endl;

      if( (float)adc[j]-(float)adcMean < 0) adc[j]=0;
      else{
	adc[j] -=adcMean;
      }
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
	adcCommon[startTimeBin+j] = (float)adc[j]-mPedOffSet;
	adcAv += (int) adc[j];
	j++;
      }
    }
    // If this black anode was bad dont count it in subtraction
    if( adcAv <100) {
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
	adcCommon[startTimeBin+j] += (float)adc[j]-mPedOffSet;
	adcAv += (int)  adc[j];
	j++;
      }
    }
    
    if( adcAv <100) {
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
	    adcCommon[startTimeBin+j] -= (float)adc[j]-mPedOffSet;
	    adcAv += (int)  adc[j];
	    j++;
	  }
	}
      }
    }
  }
  return mNAnodes;
}
//______________________________________________________________________________
void StSvtSeqAdjMaker::MakeHistogramsProb(int index,int Anode){
  
  int mSequence;
  int stTimeBin,len,status;
  double tempBuffer = 0;

  StSequence* svtSequence;
    
  status = mHybridAdjData->getSequences(Anode,mSequence,svtSequence);
  
  for(int mSeq = 0; mSeq < mSequence; mSeq++) 
    {
      stTimeBin =svtSequence[mSeq].startTimeBin; 
      len = svtSequence[mSeq].length;
      for(int j = 0 ; j < len; j++)
	{
	  tempBuffer = mInvProd->GetBuffer(stTimeBin + j);
	  mInvProdSeqAdj[index]->Fill(tempBuffer);
	}
    }
  
  //cout<<"******* making histogram finished *******"<<endl; 
}


//_____________________________________________________________________________
void StSvtSeqAdjMaker::MakeHistogramsAdc(StSvtHybridData* hybridData, int index, int Anode, int Count){
  
  int mSequence;
  int len,status;
  unsigned char* adc;

  StSequence* svtSequence;

  // Anode goes between 1-240
  
  status = hybridData->getSequences(Anode,mSequence,svtSequence);
  
  for(int mSeq = 0; mSeq < mSequence; mSeq++) 
    { 
      adc = svtSequence[mSeq].firstAdc;
      len = svtSequence[mSeq].length;
      for(int j = 0 ; j < len; j++)
	{
	  if( Count ==1) {
	    mRawAdc[index]->Fill((int)adc[j]-mPedOffSet);
	    mTimeAn[index]->Fill(Anode,j+svtSequence[mSeq].startTimeBin);
	  }
	  else 
	    mAdcAfter[index]->Fill((int)adc[j]-mPedOffSet);
	}
    }
  
  //cout<<"******* making histogram finished *******"<<endl; 
}

//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Reset(){

  //delete mInvProd;
  //delete mProbValue;
  //delete [] tempSeq1;

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

  m_ConstSet->Delete();
  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Finish(){
  
  
  if (Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Finish() "
				 << GetName() << endm; 
 
  
  return kStOK;
}

//_____________________________________________________________________________
ClassImp(StSvtSeqAdjMaker)
