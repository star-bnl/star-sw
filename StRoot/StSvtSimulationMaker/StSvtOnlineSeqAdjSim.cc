/***************************************************************************
 *
 * $Id: StSvtOnlineSeqAdjSim.cc,v 1.1 2003/11/14 15:55:56 caines Exp $
 *
 * Author: Petr Chaloupka
 ***************************************************************************
 *
 * Description: Simulates online seqence adjusting
 *
 ***************************************************************************/


#include "StSvtOnlineSeqAdjSim.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtHybridSimData.hh"
#include "StSequence.hh"
#include "StMessMgr.h"

StSvtOnlineSeqAdjSim::StSvtOnlineSeqAdjSim()
{
  SetKillBadAnodes(kTRUE);
  SetNumberTBinsToClear(2);

  //defaults is no raw anodes
  SetSaveAnode2Raw(kFALSE);
  SetSaveAnode239Raw(kFALSE);
  
  //default is no extra anodes
  SetExtraPixelsBefore(0);
  SetExtraPixelsAfter(0);

  //these settings are lower then those usualy used
  Set_thresh_hi(7);
  Set_n_seq_hi(2);

  Set_thresh_lo(2);
  Set_n_seq_lo(0);
 
}

StSvtOnlineSeqAdjSim::~StSvtOnlineSeqAdjSim()
{
 
}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSim::Make()
{
  ClearMask();
  if (mKillBadAnodes) KillBadAnodes();
  if (mNumberTBinsToClear>0) ClearFirstTbins();
  RawAnodes();
  SequenceSearch();
  WriteMask();
  FillRawData();
}


//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::SetPixelData(StSvtHybridPixelsC* data)
{
  mData=data;
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::SetRawData(StSvtHybridSimData *data)
{
  mRawData=data;
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::FillRawData()
{
  if (mRawData==NULL){
    gMessMgr->Info() <<"Error in StSvtOnlineSeqAdjSim: Raw Data is NULL!!"<<endm;
    return;
  }

 int anodes=0;  //number of anodes with some sequences

 if (mData==NULL){
    gMessMgr->Info() <<"Error in StSvtOnlineSeqAdjSim:Pixell Data is NULL!!"<<endm;
    return;
  }

 
  Char_t  *mAdcArray=mData->GetArray(); //// array of [128*240]
 
  StSequence tmpSeq[128];  //buffer for sequences on one anode
  for (int ianode=0;ianode<240;ianode++)
    {  
       int seqCount=0; //number of sequences on current anode

      //first check for raw anodes
      if ((ianode==1)&&mSaveAnode2Raw){
	tmpSeq[0].startTimeBin =0;
	tmpSeq[0].firstAdc=(unsigned char*)(mAdcArray+ianode*128);
	tmpSeq[0].length = 128;
	seqCount=1;
	mRawData->setListSequences(anodes,ianode+1, seqCount, tmpSeq);
	anodes++;
	continue;
      }

      if ((ianode==238)&&mSaveAnode239Raw){
	tmpSeq[0].startTimeBin =0;
	tmpSeq[0].firstAdc=(unsigned char*)(mAdcArray+ianode*128);
	tmpSeq[0].length = 128;
	seqCount=1;
	mRawData->setListSequences(anodes,ianode+1, seqCount, tmpSeq);
	anodes++;
	continue;
      }


     
     
      int pixCount=0; ///number of pixels in current sequence
      for(int tim = 0; tim <= 128; tim++)
	{//loop over time bins in one anode
	  unsigned char adc; 
	  if (tim==128)  adc=0; // make an artificial end of time sequence
	  else adc= (unsigned char)mAdcArray[ianode*128 + tim];
	  
	  if (adc>0)
	    {
	      if (pixCount==0){ //starting new sequence
		tmpSeq[seqCount].startTimeBin = tim;
		tmpSeq[seqCount].firstAdc=(unsigned char*)(mAdcArray+ianode*128 + tim);
	      }
	      pixCount++;
	    }
	  else
	    {
	      if(pixCount>0){//end of sequence
		tmpSeq[seqCount].length = pixCount;
		seqCount++;
		pixCount=0;
	      }
	    }
	  
	  
	}
   
      if(seqCount>0){ //save found sequences
	//cout<<"found sequences:"<<seqCount<<endl;
	mRawData->setListSequences(anodes,ianode+1, seqCount, tmpSeq);
	anodes++;
      }
      
      
    }   

  mRawData->setAnodeList();
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::WriteSequence(int anode,int begins, int ends, int NumOfHigh)
{
  //check the proper size
  //cout<<"anode:"<<anode<<"start:"<<begins<<"ends:"<<ends<<"hi:"<<NumOfHigh<<endl;
  if (NumOfHigh<=m_n_seq_hi) return;
  if ((ends-begins+1)<=m_n_seq_lo) return;
  
  //extra anodes
  begins= begins-mExtraBefore;
  if (begins<0) begins=0;
  
  ends=ends+mExtraAfter;
  if (ends>127) ends=127;

  for (int i=begins;i<=ends;i++) mMask[anode*128 + i]=kTRUE;
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::SequenceSearch()
{
  unsigned char adc;
  int loCount;
  int hiCount;
  int SeqBegins;

  int HiTresh=mPedOffset+m_thresh_hi;
  int LoTresh=mPedOffset+m_thresh_lo;

  
  Char_t *mAdcArray=mData->GetArray(); // array of [128*240]

  for(int an = 0; an < 240; an++){
    //get ready for new anode
    loCount=0;hiCount=0; //just for safety

    for(int tim = 0; tim <= 128; tim++)
      {//loop over time bins in one anode
	
	if (tim==128)  adc=0; // make an artificial end of time sequence
	else adc=(unsigned char)mAdcArray[an*128 + tim];
	
	if (adc>HiTresh) hiCount++;

	if (adc>LoTresh)
	  { //inside of sequence or at the beginning
	    if (loCount==0) SeqBegins=tim; //it is the beginning of the sequence
	    loCount++;  
	  }
	else
	  { //ouside or at the end of the sequence
	    if(loCount!=0) //end of sequence
	      {
		WriteSequence(an,SeqBegins,tim-1,hiCount);
		loCount=0;hiCount=0;	
	      }
	  }
	
	
      }
  }

}



//____________________________________________________________________________
void  StSvtOnlineSeqAdjSim::KillBadAnodes()
{
  if (!mSvtBadAnodes){
    cout<<"Warning: cannot simulate bad anodes in online sequence adjusting - no anode list"<<endl;
    return;
  }

  int index= mSvtBadAnodes->getHybridIndex(mData->getBarrelID(),mData->getLadderID(),mData->getWaferID(),mData->getHybridID());
  //cout<<"KillBadAnodes()-index"<<index<<endl;
  
  if (index < 0) {
    cout<<"Warning: StSvtOnlineSeqAdjSim::KillBadAnodes() -bad hybrid index"<<endl;
    return;
  }

  Char_t *mAdcArray=mData->GetArray(); // array of [128*240]
  
  StSvtHybridBadAnodes*  BadAnode = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
  for (int an=0;an<240;an++)
    if (BadAnode->isBadAnode(an+1))
      { //now I've found bad anode and I'm going to delete it
	for(int tim = 0; tim < 128; tim++) mAdcArray[an*128 + tim]=0;
      }
  
}


//____________________________________________________________________________
void StSvtOnlineSeqAdjSim::RawAnodes()
{
  int anode;
  int tb;

  if(mSaveAnode2Raw)
    {
      anode=1;
      for(tb = 0; tb < 128; tb++) mMask[anode*128 + tb]=kTRUE;
    }

 if(mSaveAnode239Raw)
    {
      anode=238;
      for(tb = 0; tb < 128; tb++) mMask[anode*128 + tb]=kTRUE;
    } 

}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSim::ClearMask()
{
  for (int  i=0;i<128*240;i++)mMask[i]=kFALSE;
}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSim::ClearFirstTbins()
{
 Char_t *mAdcArray=mData->GetArray(); // array of [128*240]
  
 for(int tim = 0; tim < mNumberTBinsToClear; tim++){
     for(int an = 0; an < 240; an++){
       mAdcArray[an*128 + tim]=0;
	  
    }
  }
}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSim::WriteMask()
{
  Char_t *mAdcArray=mData->GetArray(); // array of [128*240]
  
  for(int tim = 0; tim < 128; tim++){
    for(int an = 0; an < 240; an++){
      if (mMask[an*128 + tim]==kFALSE)	mAdcArray[an*128 + tim]=0;
    }
  }
}


