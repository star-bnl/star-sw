/***************************************************************************
 *
 * $Id: StSvtSeqAdjMaker.cxx,v 1.15 2001/03/22 20:47:10 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Sequence
 **************************************************************************
 *
 * $Log: StSvtSeqAdjMaker.cxx,v $
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
  mHybridRawData = NULL;
  mHybridAdjData = NULL;
  anolist = NULL;
  mInvProd = NULL;
  mProbValue = NULL;
  tempSeq1 = new StSequence[128];


  // Set up some defaults

  mPedFile = NULL;
  mPedOffSet = 100;
  m_thresh_lo = 101;
  m_thresh_hi = 105; 
  m_n_seq_lo  = 2;
  m_n_seq_hi  = 0;
  m_inv_prod_lo = 8;


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
  CreateHist(mTotalNumberOfHybrids);	    

  mInvProd = new StSvtInverseProducts();
  mProbValue = new StSvtProbValues();

  GetSvtPedestals();

  GetBadAnodes();
  
return  StMaker::Init();
  

}

//_____________________________________________________________________________


Int_t StSvtSeqAdjMaker::GetSvtRawData()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtRawData");
  assert(dataSet); 
  mSvtRawData = (StSvtData*)(dataSet->GetObject());
  assert(mSvtRawData);

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
  assert(mSvtAdjData);

  return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetMinAdcLevels( int MinAdc1,  int MinAbove1,
				         int MinAdc2,   int MinAbove2, int PedOffset){

  m_thresh_lo = MinAdc1;
  m_thresh_hi = MinAdc2; 
  m_n_seq_lo  = MinAbove1;
  m_n_seq_hi  = MinAbove2;

  if (PedOffset)
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
//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::GetBadAnodes()
{

  int index;
  StSvtBadAnode* mHybridBadAnodeData;

  mSvtBadAnodeSet = new St_ObjectSet("SvtBadAnodeSet");
  AddData(mSvtBadAnodeSet);  

  mSvtBadAnodes = new StSvtHybridCollection(mSvtRawData->getConfiguration());

  mSvtBadAnodeSet->SetObject((TObject*)mSvtBadAnodes); 
  assert(mSvtBadAnodes);


  if(  !strncmp(mSvtRawData->getConfiguration(), "Y1L", strlen("Y1L"))){

    //Put in bad Anodes by hand
    
    // Wafer 2 Hybrid 2
    
    index = 3;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=80; i<159; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    
    // Wafer 3 Hybrid 2
    
    index = 5;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=80; i<159; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    
    // Wafer 4 Hybrid 1
    
    index = 6;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=165; i<167; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    for( int i=171; i<174; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    
    
    // Wafer 4 Hybrid 2
    
    index = 7;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=38; i<80; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    for( int i=94; i<95; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    for( int i=102; i<160; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    
    // Wafer 5 Hybrid 2
    
    index = 9;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=208; i<240; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
    
    // Wafer 6 Hybrid 1
    
    index = 10;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=69;  i<73;  i++)  mHybridBadAnodeData->SetBadAnode(i,1);
    for( int i=189; i<192; i++)  mHybridBadAnodeData->SetBadAnode(i,1);
    for( int i=224; i<240; i++)  mHybridBadAnodeData->SetBadAnode(i,1);
    
    // Wafer 6 Hybrid 2
    
    index = 11;
    mHybridBadAnodeData = new StSvtBadAnode();
    mSvtBadAnodes->put_at(mHybridBadAnodeData,index);
    for( int i=0; i<240; i++)  mHybridBadAnodeData->SetBadAnode(i, 1);
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
 
   char invProdTitle_cut[25];
   char RawTitle[25];
   char AdcAfterTitle[25];
   char  Index[4];
   char* prodTitle_cut;
   char* RawTitle_cut;
   char* AdcAf_cut;

  for (int barrel = 1;barrel <= mSvtRawData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtRawData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtRawData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtRawData->getNumberOfHybrids();hybrid++) {
           
            int index = mSvtRawData->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
            sprintf(invProdTitle_cut,"InvProdSeqAdj"); 
            sprintf(RawTitle,"RawAdcIn");
 	    sprintf(AdcAfterTitle,"AdcAfterCuts");
            sprintf(Index,"%d", index);
            prodTitle_cut = strcat(invProdTitle_cut,Index);
            RawTitle_cut = strcat(RawTitle,Index);
	    AdcAf_cut = strcat(AdcAfterTitle,Index);	    

	    //mInvProdSeqAdj[index] = new TH1D(prodTitle_cut,"freqOfInvProd vs log10 of InvProd After Seq Adusting",100,0.,30.);
	    //mRawAdc[index] = new TH1F(RawTitle_cut,"freq Of Adc Values Before Seq Adjusting",150,-50.,100.);
	    //mAdcAfter[index] = new TH1F(AdcAf_cut,"freq Of Adc Values After Seq Adjusting",150,-50.,100.);
	}
      }
    }
  }


  return kStOK;
}
//_______________________________________________________________________________________________

Int_t StSvtSeqAdjMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << " In StSvtSeqAdjMaker::Make()" << GetName() << endm; 

  float sigma=0;
//   float mSigma[14];
//   mSigma[0] = 1.4; //the standared deviation Hybrid 0
//   mSigma[1] = 1.4; //the standared deviation Hybrid 1
//   mSigma[2] = 1.3; //the standared deviation Hybrid 2
//   mSigma[3] = 1.2; //the standared deviation Hybrid 3
//   mSigma[4] = 1.5; //the standared deviation Hybrid 4
//   mSigma[5] = 1.5; //the standared deviation Hybrid 5
//   mSigma[6] = 1.9; //the standared deviation Hybrid 6
//   mSigma[7] = 1.9; //the standared deviation Hybrid 7
//   mSigma[8] = 1.8; //the standared deviation Hybrid 8
//   mSigma[9] = 2.0; //the standared deviation Hybrid 9
//   mSigma[10] = 2.2; //the standared deviation Hybrid 10
//   mSigma[11] = 5; //the standared deviation Hybrid 11
//   mSigma[12] = 2.1; //the standared deviation Hybrid 12
//   mSigma[13] = 2.1; //the standared deviation Hybrid 13

  StSvtBadAnode* BadAnode;

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
	  if( mSvtPedSub){
	    mSvtPedSub->SubtractPed(mHybridRawData, index, mPedOffSet);
	  }

	  // retrieve bad anodes
	  BadAnode = (StSvtBadAnode*)mSvtBadAnodes->at(index);

	  // set probability values
	  if (mSvtPedColl)
	    sigma = ((StSvtHybridPed*)mSvtPedColl->at(index))->getRMS();
	  // cout << "sigma = " << sigma << endl;
          mProbValue->SetProbValue(sigma);    
	  // mProbValue->SetProbValue(mSigma[index]);    
          mInvProd->SetProbTable(mProbValue);

          mHybridAdjData = (StSvtHybridData *)mSvtAdjData->at(index);
	  if (mHybridAdjData)
	    delete mHybridAdjData;

	  mHybridAdjData = new StSvtHybridData(Barrel,Ladder,Wafer,Hybrid);

	  // Loop through anodes from Raw data
	  for( int Anode= 0; Anode<mHybridRawData->getAnodeList(anolist); Anode++)
            {
	      if( BadAnode){
		if( BadAnode->IsBadAnode(anolist[Anode]-1)){

		  
		  // If anode is bad set sequences to zero
		  int nSequence = 0;
		  StSequence* seq = NULL;
		  mHybridAdjData->setListSequences(Anode, nSequence, seq);
		  continue;
		}
	      } 

	      MakeHistogramsAdc(mHybridRawData,index,Anode,1);

	     
	      //Perform Asic like zero suppression
	      AdjustSequences1(Anode);
	      
	      //Perform E896 type zero-suppresion (look for non-noise like signals
	      if(m_inv_prod_lo){
		mInvProd->FindInvProducts(mHybridAdjData,Anode,mPedOffSet);
		MakeHistogramsProb(index,Anode);
		AdjustSequences2(Anode);

	      }
	      MakeHistogramsAdc(mHybridAdjData,index,Anode,2);
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

Int_t StSvtSeqAdjMaker::AdjustSequences1(int Anode){

  //Perform ASIC like zero suppression. Need > m_n_seq_lo pixels with a count
  // > than m_thresh_lo and  > m_n_seq_hi pixels with a count
  // > than m_thresh_hi  

  int nSeqOrig, nSeqNow, length, count1, count2;
  int startTimeBin,  status;
  StSequence* Sequence;
  unsigned char* adc;
  int ExtraBefore = 1;
  int ExtraAfter = 3;

  
  status= mHybridRawData->getListSequences(Anode,nSeqOrig,Sequence);

  nSeqNow=0;

  for( int nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin;

    int j =0;
    while( j<length){
      count1=0;
      count2=0;

      // data << (int) adc[j] << " " ;
      
      while( (int)adc[j] > m_thresh_lo && j<length){
	count1++;

	if( (int)adc[j] > m_thresh_hi){
	  count2++;
	}

	j++;

	//	data << (int) adc[j] << " " ;
      }
      if( count2 > m_n_seq_hi && count1 > m_n_seq_lo){
	//	cout << "Adjusting Sequences for Anode=" << Anode<<  endl;
	tempSeq1[nSeqNow].firstAdc=&adc[j- count1 - ExtraBefore];
	tempSeq1[nSeqNow].startTimeBin = startTimeBin + j - count1 - ExtraBefore;
	if((startTimeBin + j - count1 - ExtraBefore)  < 0){
	  tempSeq1[nSeqNow].startTimeBin=0;
	  tempSeq1[nSeqNow].firstAdc=&adc[0];
	}
	tempSeq1[nSeqNow].length= count1+ ExtraAfter+ ExtraBefore;
	if( tempSeq1[nSeqNow].length + tempSeq1[nSeqNow].startTimeBin  > 128) 
	  tempSeq1[nSeqNow].length=128-tempSeq1[nSeqNow].startTimeBin;
	nSeqNow++;
      }
      j++;
    }
  }
   
  mNumOfSeq = nSeqNow;

  mHybridAdjData->setListSequences(Anode, mNumOfSeq, tempSeq1);
  
  if (nSeqNow)
  //cout << "For Anode=" << Anode << " Number sequnces was=" << nSeqOrig << " Number now=" << nSeqNow << endl;
  
  //  data << endl;
  
  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences2(int Anode){

  //Perform E896 like zero suppression. Find pixels that have consecutive ADC 
  // counts that do not have the shape of noise

  int nSeqBefore, nSeqNow, count;
  int startTimeBin, len, status;
  StSequence* Sequence;
  unsigned char* adc;
  int ExtraBefore=0;
  int ExtraAfter=0;
  
  
  double tempBuffer = 0;
  
  
  nSeqNow = 0;
  
  status = mHybridAdjData->getListSequences(Anode,nSeqBefore,Sequence);
  
  for(int Seq = 0; Seq < nSeqBefore; Seq++) 
    {
      startTimeBin =Sequence[Seq].startTimeBin; 
      len = Sequence[Seq].length;
      adc = Sequence[Seq].firstAdc;
      
      int j=0;
      while( j < len)
	{ 
	  count = 0;
	  tempBuffer = mInvProd->GetBuffer(startTimeBin + j);
	  
	  while(tempBuffer > m_inv_prod_lo && j < len)
	    {
	      ++count;
	      ++j;
	      tempBuffer = mInvProd->GetBuffer(startTimeBin + j);
	      if(count > 0  && (tempBuffer < m_inv_prod_lo || j == len))
		{
		  tempSeq1[nSeqNow].firstAdc=&adc[j- count - ExtraBefore];
		  tempSeq1[nSeqNow].startTimeBin = startTimeBin + j - count - ExtraBefore;
		  if( (startTimeBin + j - count - ExtraBefore) < 0){
		    tempSeq1[nSeqNow].startTimeBin=0;
		    tempSeq1[nSeqNow].firstAdc=&adc[0];
		  }
		  tempSeq1[nSeqNow].length=count+ExtraAfter+ExtraBefore;
		  if( tempSeq1[nSeqNow].length + 
		      tempSeq1[nSeqNow].startTimeBin>128)
		    tempSeq1[nSeqNow].length=128-
		      tempSeq1[nSeqNow].startTimeBin;
		  nSeqNow++;
		}
	    }
	  j++;
	}
      
    } // Sequence loop
  
  mNumOfSeq = nSeqNow;
  
  if( nSeqBefore >0){
    mHybridAdjData->setListSequences(Anode, mNumOfSeq, tempSeq1);
  }
  // cout << "For Anode=" << Anode << " Number of sequnces was=" << nSeqBefore << " Number now=" << nSeqNow << endl;
  
  return kStOK;
  
}


//______________________________________________________________________________
void StSvtSeqAdjMaker::MakeHistogramsProb(int index,int Anode){
  
  int mSequence;
  int stTimeBin,len,status;
  double tempBuffer = 0;

  StSequence* svtSequence;
    
  status = mHybridAdjData->getListSequences(Anode,mSequence,svtSequence);
  
  for(int mSeq = 0; mSeq < mSequence; mSeq++) 
    {
      stTimeBin =svtSequence[mSeq].startTimeBin; 
      len = svtSequence[mSeq].length;
      for(int j = 0 ; j < len; j++)
	{
	  tempBuffer = mInvProd->GetBuffer(stTimeBin + j);
	  //mInvProdSeqAdj[index]->Fill(tempBuffer);
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
  
  status = hybridData->getListSequences(Anode,mSequence,svtSequence);
  
  for(int mSeq = 0; mSeq < mSequence; mSeq++) 
    { 
      adc = svtSequence[mSeq].firstAdc;
      len = svtSequence[mSeq].length;
      for(int j = 0 ; j < len; j++)
	{
	  // if( Count ==1) mRawAdc[index]->Fill((int)adc[j]-mPedOffSet);
	  //else  mAdcAfter[index]->Fill((int)adc[j]-mPedOffSet);
	}
    }
  
  //cout<<"******* making histogram finished *******"<<endl; 
}


//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::Finish(){
  
  
  if (Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Finish() "
				 << GetName() << endm; 
 
  
  return kStOK;
}

//_____________________________________________________________________________
ClassImp(StSvtSeqAdjMaker)
 








