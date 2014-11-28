/***************************************************************************
 *
 * $Id: StSvtOnlineSeqAdjSimMaker.cxx,v 1.16 2010/04/15 18:37:35 baumgart Exp $
 *
 * Author: Petr Chaloupka
 ***************************************************************************
 *
 * Description: Simulates online seqence adjusting
 *
 ***************************************************************************/


#include "StSvtOnlineSeqAdjSimMaker.h"
#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsD.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtClassLibrary/StSvtDaq.hh"
#include "StSequence.hh"
#include "StMCTruth.h"
#include "StMessMgr.h"
#include "StSvtConversionTable.h"

//__________________________________________________________________________________________________
ClassImp(StSvtOnlineSeqAdjSimMaker)

StSvtOnlineSeqAdjSimMaker::StSvtOnlineSeqAdjSimMaker(const char* name):StMaker(name)
{
  mRawData=NULL;
  m8bitPixelColl=NULL;
  mPixelColl=NULL;
  mSvtBadAnodes=NULL;
  mCurrentPixelData=NULL;
  mCurrent8bitPixelData=NULL;
  mPedOffsetAdjustment=2;
 
  //This is because of some Makers downd the chain
  GetConfig();
  SetRawData(); 
}

//____________________________________________________________________________
StSvtOnlineSeqAdjSimMaker::~StSvtOnlineSeqAdjSimMaker()
{
}

//____________________________________________________________________________
Int_t StSvtOnlineSeqAdjSimMaker::GetConfig()
{
  mConfig=NULL;
  St_DataSet *dataSet = NULL;
  dataSet = GetDataSet("StSvtConfig");
  if (!dataSet)
    {
      gMessMgr->Warning() << " No SvtConfig  data set" << endm;
      dataSet = new St_ObjectSet("StSvtConfig");
      AddConst(dataSet);
     }
   else mConfig=((StSvtConfig*)(dataSet->GetObject()));
  
  if (!mConfig) {
    gMessMgr->Warning() << "SvtConfig data set is empty- seting default full configuration" << endm;
    mConfig=new StSvtConfig();
    mConfig->setConfiguration("FULL");
    mConfig->setNumberOfAnodes(240);
    mConfig->setNumberOfTimeBins(128);
    dataSet->SetObject(mConfig);
    }
  
  return kStOk;
}

//__________________________________________________________________________________________________
Int_t  StSvtOnlineSeqAdjSimMaker::GetDaqParams()
{
  mDaq=NULL;
 
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtDaq");
  if (dataSet==NULL){
    gMessMgr->Error()<<"BIG TROUBLE:No DAQ parameters from database!!!!"<<endm;
    return kStFatal;
  }
  
  mDaq = (StSvtDaq*)dataSet->GetObject();
  if (mDaq==NULL){
    gMessMgr->Error()<<"BIG TROUBLE:No DAQ parameters are empty!!!!"<<endm;
    return kStFatal;
  }   

  SetNumberTBinsToClear(mDaq->getClearedTimeBins());
  SetExtraPixelsBefore(mDaq->getPixelsBefore());
  SetExtraPixelsAfter(mDaq->getPixelsAfter());
  SetPedOffset(mDaq->getPedOffset());
  Set_n_seq_lo(mDaq->getSeqLo());
  Set_n_seq_hi(mDaq->getSeqHi());
  Set_thresh_lo(mDaq->getThreshLo());
  Set_thresh_hi(mDaq->getThreshHi());

  return kStOk;
}

//____________________________________________________________________________
///Gets data from the rest of the simulator in the chain above 
Int_t StSvtOnlineSeqAdjSimMaker::GetPixelData()
{
  St_DataSet* dataSet=NULL;
  dataSet = GetDataSet("StSvtPixelData");
  if (!dataSet) {
    gMessMgr->Error()<<"no StSvtPixelData dataset"<<endm;
    return kStErr; 
  }

  mPixelColl=(StSvtData*)dataSet->GetObject();
  if (!mPixelColl){
      gMessMgr->Error()<<"StSvtPixelData is empty"<<endm;
      return kStErr;
      }
      
  dataSet=NULL;
  
  dataSet = GetDataSet("StSvt8bitPixelData");
  if (!dataSet) {
    gMessMgr->Error()<<"no StSvt8bitPixelData dataset"<<endm;
    return kStErr;
    }
  m8bitPixelColl=(StSvtData*)dataSet->GetObject();
  if (!m8bitPixelColl){
      gMessMgr->Error()<<"StSvt8bitPixelData is empty"<<endm;
      return kStErr;
      }

  return kStOk;
}  

//____________________________________________________________________________
/// Get list of bad anodes from the database
void StSvtOnlineSeqAdjSimMaker::GetBadAnodes()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtBadAnodes");
  if( !dataSet) {
    gMessMgr->Warning() << "StSvtOnlineSeqAdjSimMaker: No Svt Bad Anodes data set" << endm;
    return;
  }

  mSvtBadAnodes = (StSvtHybridCollection*)(dataSet->GetObject());
  if( !mSvtBadAnodes) {
    gMessMgr->Warning() << "StSvtOnlineSeqAdjSimMaker: No Svt Bad Anodes data " << endm;
    return;
  }

  /*if (Debug())*/  gMessMgr->Info()<<"StSvtOnlineSeqAdjSimMaker:Svt Bad Anode list found"<<endm;
}


//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::SetRawData()
{  //this makes new or replaces raw data for SvtDaqMaker

  St_ObjectSet* set=(St_ObjectSet*)GetDataSet("StSvtRawData");
  
  if (!set) {
    set = new St_ObjectSet("StSvtRawData");
    AddData(set);
  }
  mRawData = new StSvtData(mConfig->getConfiguration());
  set->SetObject(mRawData);
}

//____________________________________________________________________________
Int_t StSvtOnlineSeqAdjSimMaker::Init()
{
  return  StMaker::Init();
}

//____________________________________________________________________________
///Reads run dependent data from the database
Int_t StSvtOnlineSeqAdjSimMaker::InitRun(int runumber)
{
  if (Debug()) gMessMgr->Info()<<"StSvtOnlineSeqAdjSimMaker::InitRun"<<endm;	
  GetConfig();
  GetBadAnodes();
  Int_t res;
  if ((res=GetDaqParams())!=kStOk) return res;
  
  gMessMgr->Info()<< " DAQ parameters used for simulation:"<<endm;
  gMessMgr->Info() << "     PedOffSet = "<<mPedOffset<<endm;
  gMessMgr->Info() << "    thresh_lo = "<<m_thresh_lo <<endm;
  gMessMgr->Info() << "     thresh_hi = "<<m_thresh_hi <<endm;
  gMessMgr->Info() << "     n_seq_lo  = "<<m_n_seq_lo <<endm;
  gMessMgr->Info() << "     n_seq_hi  = "<< m_n_seq_hi <<endm;

  
  if (Debug()) gMessMgr->Info()<<"StSvtOnlineSeqAdjSimMaker::InitRun...END"<<endm;	
  return  StMaker::InitRun(runumber);
}

//____________________________________________________________________________
Int_t StSvtOnlineSeqAdjSimMaker::Finish()
{
  return kStOK;
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::Clear(const char*)
{
  mRawData=NULL;
  StMaker::Clear();
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::SetAdjParams(int thresh_lo,int n_seq_lo,int thresh_hi,int n_seq_hi)
{
m_thresh_lo =thresh_lo;
m_n_seq_lo =n_seq_lo;
m_thresh_hi =thresh_hi;
m_n_seq_hi =n_seq_hi;
}

//____________________________________________________________________________
///Does all data adjusting. At the end creates data in the raw format - the same format as the real data in.
Int_t  StSvtOnlineSeqAdjSimMaker::Make()
{
  if (Debug()) gMessMgr->Info()<<"StSvtOnlineSeqAdjSimMaker::Make"<<endm;	
  SetRawData();
  Int_t res;
  if ((res=GetPixelData())!= kStOk) return res;
  
  for(int Barrel = 1;Barrel <= mPixelColl->getNumberOfBarrels();Barrel++) {
    for (int Ladder = 1;Ladder <= mPixelColl->getNumberOfLadders(Barrel);Ladder++) {
       for (int Wafer = 1;Wafer <= mPixelColl->getNumberOfWafers(Barrel);Wafer++) {
         for( int Hybrid = 1;Hybrid <= mPixelColl->getNumberOfHybrids();Hybrid++){
           
           mCurrentIndex = mPixelColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
           if( mCurrentIndex < 0) continue; 
           //cout<<"index:"	<<mCurrentIndex<<endl;
           
           mCurrentPixelData  = (StSvtHybridPixelsD*)mPixelColl->at(mCurrentIndex);
           mCurrent8bitPixelData = (StSvtHybridPixelsC*)m8bitPixelColl->at(mCurrentIndex);
           
           if(!mCurrent8bitPixelData) {
             mCurrent8bitPixelData = new StSvtHybridPixelsC(Barrel, Ladder, Wafer, Hybrid);
             m8bitPixelColl->put_at(mCurrent8bitPixelData,mCurrentIndex);
           }
           
           if(!mCurrentPixelData) { //no data from simulation Maker
             mCurrent8bitPixelData->Reset();
           }
           
           //No we have the pixel data sructures,turn it now into the real data
           //ie. simulate the DAQ
           
           Conversion10to8bit();
           ClearMask();
           if (mKillBadAnodes) KillBadAnodes();	 
           if (mNumberTBinsToClear>0) ClearFirstTbins();
           RawAnodes();
           SequenceSearch();
           WriteMask();
           FillRawData();
           
         }
       }
    }
  }
   
  if (Debug()) gMessMgr->Info()<<"StSvtOnlineSeqAdjSimMaker::Make...END"<<endm;	
  return kStOK;
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::Conversion10to8bit()
{
  double *fromArray=mCurrentPixelData->GetArray();
  Char_t *toArray=mCurrent8bitPixelData->GetArray();

  for (int i=0;i<mCurrentPixelData->getTotalNumberOfPixels();i++)
    {
      double adc=fromArray[i];
      if (adc<=0) adc=0.;
      if (adc>=1023) adc=1023.;
      unsigned int adc1=(unsigned int)adc; //conversion to 10 bits from double - ?is it "compiler" reliable?
      toArray[i]=(Char_t)StSvt10to8ConversionTable[adc1]; //conversion to 8 bits
    }
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::FillRawData()
{
  StSvtHybridData *hybridData;
  hybridData = new StSvtHybridData(mCurrentPixelData->getBarrelID(), mCurrentPixelData->getLadderID(), mCurrentPixelData->getWaferID(),mCurrentPixelData->getHybridID());
  mRawData->put_at(hybridData,mCurrentIndex);
  
  int anodes=0;  //number of anodes with some sequences
  
  //raw anodes need to checked if they are not flagged bad
  StSvtHybridBadAnodes*  badAnode =NULL;
  if (mKillBadAnodes && mSvtBadAnodes)  badAnode = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(mCurrentIndex);
  
  Char_t *mAdcArray=mCurrent8bitPixelData->GetArray(); // array of [128*240]  
  
  StSequence tmpSeq[128];  //buffer for sequences on one anode
  StMCTruth  tmpTru[128];  //buffer for truth on one anode
  for (int ianode=0;ianode<240;ianode++)
    {  
      int seqCount=0; //number of sequences on current anode

      //first check for raw anodes
      int an=ianode+1;
      if ((an==mDaq->getSavedBlackAnodes(0))||(an==mDaq->getSavedBlackAnodes(1))||(an==mDaq->getSavedBlackAnodes(2))||(an==mDaq->getSavedBlackAnodes(3)))
        {
          if (badAnode->isBadAnode(an)) continue; //don't write out zeros if bad
          tmpSeq[0].startTimeBin =0;
          tmpSeq[0].firstAdc=(unsigned char*)(mAdcArray+ianode*128);
          tmpSeq[0].length = 128;
          tmpTru[0] = 0;
          seqCount=1;
          hybridData->setListSequences(anodes, an, seqCount, tmpSeq);
          anodes++;
          continue; 
        }
               
      int pixCount=0; ///number of pixels in current sequence
      StMCPivotTruth pivo;
        for(int tim = 0; tim <= 128; tim++)
          {//loop over time bins in one anode
            unsigned char adc; 
            if (tim==128)  adc=0; // make an artificial end of time sequence
            else adc= (unsigned char)mAdcArray[ianode*128 + tim];
            
            if (adc>0)
              {
                StMCTruth tru =mCurrentPixelData->getTrackId(ianode*128 + tim);
		if (pixCount==0){ //starting new sequence
                  pivo.Reset();
                  tmpSeq[seqCount].startTimeBin = tim;
                  tmpSeq[seqCount].firstAdc=(unsigned char*)(mAdcArray+ianode*128 + tim);
                }
                if(int(tru)) pivo.Add(tru,adc);
                pixCount++;
              }
            else
              {
                if(pixCount>0){//end of sequence
                  tmpSeq[seqCount].length = pixCount;
                  tmpTru[seqCount] = pivo.Get();
                  seqCount++;
                  pixCount=0;
                }
              }
            
            
          }
   
        if(seqCount>0){ //save found sequences
          //cout<<"found sequences:"<<seqCount<<endl;
          hybridData->setListSequences(anodes,ianode+1, seqCount, tmpSeq);
          hybridData->setListTruth    (anodes,ianode+1, seqCount, tmpTru);
          anodes++;
        }
        
        
    }   
  
  hybridData->setAnodeList();
}

//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::WriteSequence(int anode,int begins, int ends, int NumOfHigh)
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
void StSvtOnlineSeqAdjSimMaker::SequenceSearch()
{
  unsigned char adc;
  int loCount;
  int hiCount;
  int SeqBegins=0;

  int HiTresh=mPedOffset+m_thresh_hi+mPedOffsetAdjustment;
  int LoTresh=mPedOffset+m_thresh_lo+mPedOffsetAdjustment;

  
  Char_t *mAdcArray=mCurrent8bitPixelData->GetArray(); // array of [128*240]

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
void  StSvtOnlineSeqAdjSimMaker::KillBadAnodes()
{

  if (!mSvtBadAnodes){
    cout<<"Warning: cannot simulate bad anodes in online sequence adjusting - no anode list"<<endl;
    return;
  }
    StSvtHybridBadAnodes*  BadAnode = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(mCurrentIndex);
  if (!BadAnode) return;
  
  Char_t *mAdcArray=mCurrent8bitPixelData->GetArray(); // array of [128*240]  
  
  for (int an=0;an<240;an++)
    if (BadAnode->isBadAnode(an+1))
      { //now I've found bad anode and I'm going to delete it
	for(int tim = 0; tim < 128; tim++) mAdcArray[an*128 + tim]=0;
      }
  
}


//____________________________________________________________________________
void StSvtOnlineSeqAdjSimMaker::RawAnodes()
{
 
  for (int i=0 ; i<4 ; i++)
    {
      int an=mDaq->getSavedBlackAnodes(i);
      if ((an<=0)||(an>240)) continue;
      for(int tb = 0; tb < 128; tb++) mMask[an*128 + tb]=kTRUE;
    } 

}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSimMaker::ClearMask()
{
  memset(mMask,0,128*240*sizeof(mMask[0]));
}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSimMaker::ClearFirstTbins()
{
 Char_t *mAdcArray=mCurrent8bitPixelData->GetArray(); // array of [128*240]
  
 for(int tim = 0; tim < mNumberTBinsToClear; tim++){
     for(int an = 0; an < 240; an++){
       mAdcArray[an*128 + tim]=0;
	  
    }
  }
}

//____________________________________________________________________________
void  StSvtOnlineSeqAdjSimMaker::WriteMask()
{
  Char_t *mAdcArray=mCurrent8bitPixelData->GetArray(); // array of [128*240]
  
  for(int tim = 0; tim < 128; tim++){
    for(int an = 0; an < 240; an++){
       if (mMask[an*128 + tim]==kFALSE)	mAdcArray[an*128 + tim]=0;
    }
  }
}


