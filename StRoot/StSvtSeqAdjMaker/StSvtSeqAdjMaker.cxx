/***************************************************************************
 *
 * $Id: StSvtSeqAdjMaker.cxx,v 1.2 2000/07/03 02:07:56 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Sequence Adjuster Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtSeqAdjMaker.cxx,v $
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
#include "StSvtInverseProducts.hh"
#include "StSvtPedSub.h"
#include "StSvtSeqAdjMaker.h"
#include "StMessMgr.h"

int* anolist; 
ifstream inseqfile;

//___________________________________________________________________________________________
StSvtSeqAdjMaker::StSvtSeqAdjMaker(const char *name) : StMaker(name)
{

  mHybridData= NULL;
  mSvtEvent = NULL;
  anolist = NULL;
  mInvProd = NULL;
  tempSeq1 = new StSequence[128];

}

//____________________________________________________________________________________________
StSvtSeqAdjMaker::~StSvtSeqAdjMaker(){
           

}

//_______________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetInputFiles(char* table, 
				      char* pedFile, int PedOffset)
{
  if(Debug()) gMessMgr->Debug() << "opening file called " << table << " " 
                               << GetName() << endm;  
  
  inseqfile.open(table,ios::in);

  mPedFile = pedFile;

  mPedOffSet = PedOffset;
  return kStOK;
}


//_____________________________________________________________________________________________
Int_t StSvtSeqAdjMaker::Init()
{

  if(Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Init() ... "
                               << GetName() << endm; 

  int ok;

  ok = GetSvtEvent();

  mTotalNumberOfHybrids = mSvtEvent->getTotalNumberOfHybrids();
  CreateHist(mTotalNumberOfHybrids);	    

  mInvProd = new StSvtInverseProducts();
  mInvProd->FillProbTable(inseqfile);

  
  mSvtPedSub = new StSvtPedSub();
  ok = mSvtPedSub->ReadFromFile( mPedFile, mSvtEvent);

  return  StMaker::Init();
  

}

//__________________________________________________________________________________________________

Int_t StSvtSeqAdjMaker::GetSvtEvent()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtData");
  assert(dataSet); 
  mSvtEvent = (StSvtData*)(dataSet->GetObject());
  assert(mSvtEvent);

  return kStOK;
}

//____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetMinAdcLevels( int MinAdc1,  int MinAbove1,
				         int MinAdc2,   int MinAbove2){

  m_thresh_lo = MinAdc1;
  m_thresh_hi = MinAdc2; 
  m_n_seq_lo  = MinAbove1;
  m_n_seq_hi  = MinAbove2;

  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::SetLowInvProd(int LowInvProd)
{
  m_inv_prod_lo = LowInvProd;
  
  return kStOK; 
}
//_____________________________________________________________________________
Int_t StSvtSeqAdjMaker::CreateHist(Int_t tNuOfHyb)
{
   // Create Histograms

   mInvProdSeqAdj = new TH1D*[tNuOfHyb];
 
  char invProdTitle_fcut[25];
  char  Index[3];
  char* prodTitle_fcut;
  
  for (int barrel = 1;barrel <= mSvtEvent->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtEvent->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtEvent->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtEvent->getNumberOfHybrids();hybrid++) {
           
            int index = mSvtEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
            sprintf(invProdTitle_fcut,"InvProdSeqAdj"); 
            sprintf(Index,"%d", index);
            prodTitle_fcut = strcat(invProdTitle_fcut,Index);
	    mInvProdSeqAdj[index] = new TH1D(prodTitle_fcut,"freqOfInvProd vs log10 of InvProd",100,0.,30.);
	}
      }
    }
  }


  return kStOK;
}
//_______________________________________________________________________________________________

Int_t StSvtSeqAdjMaker::Make()
{

  if (Debug()) gMessMgr->Debug() << " In StSvtSeqAdjMaker::Make()"
                               << GetName() << endm; 

  
  for(int Barrel = 1;Barrel <= mSvtEvent->getNumberOfBarrels();Barrel++) {
    
    for (int Ladder = 1;Ladder <= mSvtEvent->getNumberOfLadders(Barrel);Ladder++) {
      
      for (int Wafer = 1;Wafer <= mSvtEvent->getNumberOfWafers(Barrel);Wafer++) {
	
	for( int Hybrid = 1;Hybrid <=mSvtEvent->getNumberOfHybrids();Hybrid++){
	  
          int index = mSvtEvent->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
	  
          mHybridData = (StSvtHybridData *)mSvtEvent->getObject(Barrel,Ladder,Wafer,Hybrid);
	  if( !mHybridData) continue;
          
          mInvProd->SetHybridPointer(mHybridData);


	  mSvtPedSub->SubtractPed(mHybridData, index, mPedOffSet);

	  for( int Anode= 0; Anode<mHybridData->getAnodeList(anolist); Anode++)
            {

	      //Perform Asic like zero suppression
	      AdjustSequences1(Anode);
	      
	      //Perform E896 type zero-suppresion (look for non-noise like signals
	      if(m_inv_prod_lo){
		mInvProd->FindInvProducts(mPedOffSet,Anode);
	        AdjustSequences2(Anode);
		MakeHistograms(index,Anode);
	      }
            }
	  
	  mHybridData->SetAnodeList();
	  
	  
          mInvProd->ResetBuffer();
	}
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
  
  status= mHybridData->getListSequences(Anode,nSeqOrig,Sequence);

  nSeqNow=0;

  for( int nSeq=0; nSeq< nSeqOrig ; nSeq++){
  
    adc=Sequence[nSeq].firstAdc;
    length = Sequence[nSeq].length;
    startTimeBin=Sequence[nSeq].startTimeBin;

    for( int j=0; j<length; j++){
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
	//	cout << "Adjusting Sequences for Anode=" << Anode<<  endl;
	tempSeq1[nSeqNow].startTimeBin = startTimeBin + j - count1;
	tempSeq1[nSeqNow].length=count1;
	tempSeq1[nSeqNow].firstAdc=&adc[j - count1];
	nSeqNow++;
      }
    }
  }
   
  mNumOfSeq = nSeqNow;

    mHybridData->SetListSequences(Anode, mNumOfSeq, tempSeq1);

  // cout << "For Anode=" << Anode << " Number sequnces was=" << nSeqOrig << " Number now=" << nSeqMax+nSeqOrig << endl;

  return kStOK;
}

//_____________________________________________________________________________

Int_t StSvtSeqAdjMaker::AdjustSequences2(int Anode){

  //Perform E896 like zero suppression. Find pixels that have consecutive ADC 
  // counts that do not have the shape of noise

  int nSeqBefore, nSeqNow, count;
  int stTimeBin, len, status;
  StSequence* Sequence;
  unsigned char* adc;

  double tempBuffer = 0;
 

      nSeqNow = 0;

      status = mHybridData->getListSequences(Anode,nSeqBefore,Sequence);
      
      for(int Seq = 0; Seq < nSeqBefore; Seq++) 
         {
          stTimeBin =Sequence[Seq].startTimeBin; 
          len = Sequence[Seq].length;
          adc = Sequence[Seq].firstAdc;

            for(int j = 0 ; j < len; j++)
	      { 
                count = 0;
                tempBuffer = mInvProd->GetBuffer(stTimeBin + j);

                while(tempBuffer > m_inv_prod_lo && j < len)
		  {
                    ++count;
                    ++j;
                    tempBuffer = mInvProd->GetBuffer(stTimeBin + j);
                    if(count >  m_n_seq_lo && (tempBuffer < m_inv_prod_lo || j == len))
		      {
                       	tempSeq1[nSeqNow].startTimeBin = stTimeBin + j - count;
                  	tempSeq1[nSeqNow].length=count;
                	tempSeq1[nSeqNow].firstAdc=&adc[j - count];
                	nSeqNow++;
	//	cout << "nSeqNow = " <<nSeqNow-1 <<" FirstTimeBin = " <<tempSeq[nSeqNow].startTimeBin << " length = " << Sequence[nSeqNow-1].length  << endl;
                      }
                   }
              }
	  } // Sequence loop

        mNumOfSeq = nSeqNow;
    
	if( nSeqBefore >0 && nSeqBefore != nSeqNow){
	  mHybridData->SetListSequences(Anode, mNumOfSeq, tempSeq1);
	}
      // cout << "For Anode=" << Anode << " Number of sequnces was=" << nSeqBefore << " Number now=" << nSeqNow << endl;

 return kStOK;
}


//______________________________________________________________________________
void StSvtSeqAdjMaker::MakeHistograms(int index,int Anode){
  
  int mSequence;
  int stTimeBin,len,status;
  double tempBuffer = 0;

  StSequence* svtSequence;
  
  mHybridData = (StSvtHybridData *)mSvtEvent->at(index);
  if( !mHybridData) return;
  
  
  status = mHybridData->getListSequences(Anode,mSequence,svtSequence);
  
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
Int_t StSvtSeqAdjMaker::Finish(){
  
  
  mSvtPedSub->Clear();

  if (Debug()) gMessMgr->Debug() << "In StSvtSeqAdjMaker::Finish() "
				 << GetName() << endm; 
 
  
  return kStOK;
}

//_____________________________________________________________________________
ClassImp(StSvtSeqAdjMaker)
 








