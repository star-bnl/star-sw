/***************************************************************************
 *
 * $Id: StSvtClusterAnalysisMaker.cxx,v 1.22 2002/05/09 16:55:40 munhoz Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Cluster Analysis Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtClusterAnalysisMaker.cxx,v $
 * Revision 1.22  2002/05/09 16:55:40  munhoz
 * add reading bad anodes from DB
 *
 * Revision 1.21  2002/05/08 16:03:52  caines
 * Fix again memory leak - accidentally pput back in data has to be a const() not data()
 *
 * Revision 1.20  2002/04/25 20:34:50  caines
 * Pass bad anode information into cluster fitter
 *
 * Revision 1.17  2002/01/05 21:45:18  caines
 * Inlcude t0 correction in hit
 *
 * Revision 1.16  2001/09/22 01:07:09  caines
 * Fixes now that AddData() is cleared everyevent
 *
 * Revision 1.15  2001/09/16 22:09:28  caines
 * Add extra checks for when SVT isnt in every event
 *
 * Revision 1.14  2001/08/07 20:52:15  caines
 * Implement better packing of svt hardware and charge values
 *
 * Revision 1.13  2001/07/19 20:42:20  caines
 *  Add Reset functions
 *
 * Revision 1.12  2001/05/04 14:20:05  caines
 * Improved historgramming
 *
 * Revision 1.11  2001/04/25 18:37:28  perev
 * HPcorrs
 *
 * Revision 1.10  2001/03/22 20:46:53  caines
 * Comment out some of the QA histograms
 *
 * Revision 1.9  2001/02/18 00:10:48  caines
 * Improve and use StSvtConfig
 *
 * Revision 1.8  2000/11/30 20:43:17  caines
 * Use database
 *
 * Revision 1.7  2000/10/31 16:20:57  caines
 * Added more functions to make the code more readable
 *
 * Revision 1.6  2000/09/14 22:17:57  caines
 * Put back in flagging hot anodes and timebuckets
 *
 * Revision 1.5  2000/08/29 22:46:26  caines
 * Fixed some memory leaks
 *
 * Revision 1.4  2000/08/21 13:06:58  caines
 * Much improved hit finding and fitting
 *
 * Revision 1.3  2000/07/16 22:32:54  caines
 * Fills spacepoint table correctly
 *
 * Revision 1.2  2000/07/13 14:50:49  caines
 * Improvements on not saving single pixels
 *
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#include <fstream.h>
#include <iomanip.h>

#include "StChain.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "TH1.h"
#include "TH2.h"

#include "StMessMgr.h"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridPixels.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSequence.hh"
#include "StSvtAnalysis.hh"
#include "StSvtAnalysedHybridClusters.hh"
#include "StSvtClusterAnalysisMaker.h"
#include "StSvtSeqAdjMaker/StSvtSeqAdjMaker.h"


//___________________________________________________________________________________________
StSvtClusterAnalysisMaker::StSvtClusterAnalysisMaker(const char *name) : StMaker(name)
{
   mSvtAdjEvent     = NULL;

   mHybridRawData   = NULL;
   mHybridAdjData   = NULL;
   mHybridPixelData = NULL;

   mSvtBadAnode     = NULL;
   mSvtBadAnodeSet  = NULL;

   mSvtRawEventColl = NULL;
   mSvtClusterColl  = NULL;
   mSvtPixelColl    = NULL;
   mSvtAnalColl     = NULL;

   mSvtAnalysis     = NULL;
   mSvtHit          = NULL;
   mSvtAnalClusters = NULL; 

   mNumOfClusters   = 0;
   mTotNumOfClusters = 0;
   mTotNumOfGoodClusters = 0;
   mTotNumOfBadClusters = 0;

   for(int i = 0; i < 128; i++)
    for(int j = 0; j < 240; j++)
       adcArray[i + j*128] = 0.0;

}

//____________________________________________________________________________________________
StSvtClusterAnalysisMaker::~StSvtClusterAnalysisMaker(){

}


//_____________________________________________________________________________________________
Int_t StSvtClusterAnalysisMaker::Init()
{
  if( Debug()) gMessMgr->Debug() <<"In StSvtClusterAnalysisMaker::Init()"
				 << GetName() <<endm;

  mNoEvents=0;

  
  GetSvtRawEvent();
  GetSvtEvent();
  GetSvtCluster();
  SetSvtAnalysis();

   
  mTotalNumberOfHybrids = mSvtRawEventColl->getTotalNumberOfHybrids();
  CreateClusterHist(mTotalNumberOfHybrids);

  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtBadAnodes"); 
  if (dataSet)
    mSvtBadAnodeSet = (StSvtHybridCollection*)(dataSet->GetObject());

  mSvtAnalysis = new StSvtAnalysis(mTotalNumberOfHybrids);

  return  StMaker::Init();

}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtRawEvent()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtRawData");
  if( !dataSet) return kStWarn;
  mSvtRawEventColl = (StSvtHybridCollection*)(dataSet->GetObject());
  if( !mSvtRawEventColl) return kStWarn;
 
  return kStOK;
}
//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtEvent()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtData");
  mSvtAdjEvent = (StSvtData*)(dataSet->GetObject());

  return kStOK;
}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtCluster()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtCluster");
  if( !dataSet) return kStWarn;
  mSvtClusterColl = (StSvtHybridCollection*)(dataSet->GetObject());
  if( !mSvtClusterColl) return kStWarn;
  return kStOK;
}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtPixels()
{

  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtPixels");
  assert(dataSet);
  mSvtPixelColl = (StSvtHybridCollection*)(dataSet->GetObject());
  assert(mSvtPixelColl);

  return kStOK;
}

//__________________________________________________________________________________________________
Int_t StSvtClusterAnalysisMaker::SetSvtAnalysis()
{
  mSvtAnalSet = new St_ObjectSet("StSvtAnalResults");
  // AddData(mSvtAnalSet); 
  AddConst(mSvtAnalSet); 
  SetOutput(mSvtAnalSet); //Declare for output

  mSvtAnalColl = new StSvtHybridCollection(mSvtAdjEvent->getConfiguration());
  //cout<<"mSvtAnalColl  = "<<mSvtAnalColl<<endl;
  mSvtAnalSet->SetObject((TObject*)mSvtAnalColl); 
  //assert(mSvtAnalColl);

  return kStOK;
}


//__________________________________________________________________________________________________


Int_t StSvtClusterAnalysisMaker::CreateClusterHist(Int_t tNuOfHyb)
{

  // Create Histograms
  m_n_seq = new TH1F("NSeqClust","No. Pixels on cluster",100,0.,300.);
  m_nClust = new TH2F("NClust","No. clusters per event",1000,0.,1000.,100,0.,3000.);
  m_SumADCvsTime = new TH2F("SumAdcVsTime" ,"Time bucket vs Sum ADC",128,0.,128.,1000,0,100);
  m_PeakADCvsTime = new TH2F("PeakAdcVsTime" ,"Time bucket vs PeakADC",128,0.,128.,50,0,50);
  m_time_anode_clu = new TH2F*[tNuOfHyb];
  m_time_anode_raw = new TH2F*[tNuOfHyb];
  m_sumADC = new TH1F*[tNuOfHyb];
   

  char title1[20];
  char titleraw[20];
  char titleadc[20];
  char  title2[4];
  char* title3;
  char* titlerawc;
  char* titleadcc;
  
   for (int barrel = 1;barrel <= mSvtRawEventColl->getNumberOfBarrels();barrel++) {
     for (int ladder = 1;ladder <= mSvtRawEventColl->getNumberOfLadders(barrel);ladder++) {
       for (int wafer = 1;wafer <= mSvtRawEventColl->getNumberOfWafers(barrel);wafer++) {
	 for (int hybrid = 1;hybrid <= mSvtRawEventColl->getNumberOfHybrids();hybrid++) {
           
	   int index = mSvtRawEventColl->getHybridIndex(barrel,ladder,wafer,hybrid);
	   if(index < 0) continue;
	   sprintf(title1,"TimAnodecluster");
	   sprintf(titleraw,"TimAnodeRaw");
	   sprintf(titleadc,"ADC");
	   sprintf(title2,"%d", index);
	   title3 = strcat(title1,title2);
	   titlerawc = strcat(titleraw,title2);
	   titleadcc = strcat(titleadc,title2);
	   //	   cout << title3 <<" " << titlerawc << " " << titleadcc << " " << index << " " <<  tNuOfHyb << endl;
	   if( Debug()){
	     m_time_anode_clu[index] = new TH2F(title3 ,"Time bucket vs anode",240,0.5,240.5,129,-0.5,128.5);	   
	     m_time_anode_raw[index] = new TH2F(titlerawc ,"Time bucket vs anode",240,0.5,240.5,129,-0.5,128.5);
	     m_sumADC[index] = new TH1F(titleadcc,"Sum of ADC counts in cluster",100,0,1000);
	   }
	}
      }
     }
   }


  return kStOK;
}


//_______________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::Make()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtClusterAnalysisMaker::Make() ..."
				 <<  GetName() << endm;

  if( GetSvtRawEvent()){
    gMessMgr->Warning() << " StSvtClusterAnalysisMaker::Make :No SVT RAW data " << endm;
    return kStWarn;
  }
  if( GetSvtEvent()){
    gMessMgr->Warning() << " StSvtClusterAnalysisMaker::Make :No SVT seq data " << endm;
    return kStWarn;
  }
  if( GetSvtCluster()){
    gMessMgr->Warning() << " StSvtClusterAnalysisMaker::Make :No SVT cluster data " << endm;
    return kStWarn;
  }

  //SetSvtAnalysis();
  SetClusterAnalysis();

  if( Debug()) MakeHistograms();

  return kStOK;

}

//___________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::SetClusterAnalysis()
{
 int index =0;
 float T0Jitter;

 StSvtSeqAdjMaker* StSvtSeqMaker = (StSvtSeqAdjMaker*)GetMaker("SvtSeqAdj");
 
 for(int barrel = 1;barrel <= mSvtAdjEvent->getNumberOfBarrels();barrel++) {
   //cout<<mSvtAdjEvent->getNumberOfBarrels()<<endl;
   for (int ladder = 1;ladder <= mSvtAdjEvent->getNumberOfLadders(barrel);ladder++) {
     //cout<<mSvtAdjEvent->getNumberOfLadders(barrel)<<endl;
      for (int wafer = 1;wafer <= mSvtAdjEvent->getNumberOfWafers(barrel);wafer++) {
	// cout<<mSvtAdjEvent->getNumberOfWafers(barrel)<<endl;
	for (int hybrid = 1;hybrid <=mSvtAdjEvent->getNumberOfHybrids();hybrid++){
          //cout<<mSvtAdjEvent->getNumberOfHybrids()<<endl;  
           
          index = mSvtAdjEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
          if(index < 0) continue;

          mHybridAdjData = (StSvtHybridData *)mSvtAdjEvent->at(index);
	  if( !mHybridAdjData) continue;
	  // Adjust recorded phase for each hybrid into frcation of a 
	  // time bucket
	  T0Jitter = (float)(mHybridAdjData->getTimeZero());
	  if( T0Jitter < 4. && T0Jitter > 2.){
	    T0Jitter = 0.;
	  }
	  else if( T0Jitter < 5. && T0Jitter > 3.){
	    T0Jitter = 3. - (8./3.);
	  }
	  else if( T0Jitter < 6. && T0Jitter > 4.){
	    T0Jitter =  6. - ( 2.*8./3.);
	  }

          mHybridCluster = (StSvtHybridCluster*)mSvtClusterColl->at(index); 

          mHybridRawData = (StSvtHybridData *)mSvtRawEventColl->at(index);

          mNumOfClusters = mHybridCluster->getNumberOfClusters(); 
 
	  mSvtBadAnode=0;
	  if( mSvtBadAnodeSet) mSvtBadAnode = (StSvtHybridBadAnodes*)mSvtBadAnodeSet->at(index);
          mSvtAnalysis->SetPointers(mHybridAdjData,mHybridRawData,
				    mHybridCluster,mSvtBadAnode,
				    mSvtAdjEvent->getTotalNumberOfHybrids(),
	                            StSvtSeqMaker->GetPedOffset());
	  mSvtAnalysis->SetHybIndex(index);
          mSvtAnalysis->FirstAndLastAnodes();
          mSvtAnalysis->CluFirstTimeBin();
          mSvtAnalysis->CluLastTimeBin();
          mSvtAnalysis->MomentAnalysis();
          mSvtAnalysis->SetBadAnTb(mNumOfClusters);   //note I dont look at decon here
          if( Debug()) mSvtAnalysis->Report(index);


	  mSvtAnalClusters = (StSvtAnalysedHybridClusters*)
	     mSvtAnalColl->at(index);

	  if( mSvtAnalClusters){
	    delete mSvtAnalClusters;
	  }
	  mSvtAnalClusters = new StSvtAnalysedHybridClusters(barrel, ladder, wafer, hybrid);
          if(mSvtAnalClusters) {
	    mSvtAnalClusters->setMembers(mSvtAnalysis->GetnSvtClu(),
					 mSvtAdjEvent->getProperHybridIndex(
					 barrel,ladder,wafer,hybrid));
	    mSvtAnalClusters->setSvtHit(mSvtAnalysis,T0Jitter);
	    mSvtAnalColl->at(index) = mSvtAnalClusters;
	   
	    for( int clu=0; clu<mSvtAnalysis->GetnSvtClu(); clu++){
	      if(Debug()) m_sumADC[index]->Fill(mSvtAnalysis->GetCluCharge(clu));
	      if( mSvtAnalysis->GetCluFlag(clu) < 4){
		m_SumADCvsTime->Fill((float)mSvtAnalysis->GetMeanClusterTimeBin(clu),(float)mSvtAnalysis->GetCluCharge(clu));
	       
		m_PeakADCvsTime->Fill((float)mSvtAnalysis->GetMeanClusterTimeBin(clu),(float)mSvtAnalysis->GetCluPeakAdc(clu));
		m_n_seq->Fill(mSvtAnalysis->GetCluNumPixels(clu));
	      }
	    
	    }
	  }
        }
      }
    }
  }

  //printClusterInfo();
 
  //GetPixelData();
  return kStOK;
}

//___________________________________________________________________________________________________
void StSvtClusterAnalysisMaker::printClusterInfo()
{

 int index = 0;
 mTotNumOfGoodClusters = 0;
 mTotNumOfBadClusters = 0;

 for(int barrel = 1;barrel <= mSvtAdjEvent->getNumberOfBarrels();barrel++) {
   //cout<<mSvtEvent->getNumberOfBarrels()<<endl;
   for (int ladder = 1;ladder <= mSvtAdjEvent->getNumberOfLadders(barrel);ladder++) {
     //cout<<mSvtEvent->getNumberOfLadders(barrel)<<endl;
      for (int wafer = 1;wafer <= mSvtAdjEvent->getNumberOfWafers(barrel);wafer++) {
	// cout<<mSvtEvent->getNumberOfWafers(barrel)<<endl;
	for (int hybrid = 1;hybrid <=mSvtAdjEvent->getNumberOfHybrids();hybrid++){
          //cout<<mSvtEvent->getNumberOfHybrids()<<endl;

          index = mSvtAnalColl->getHybridIndex(barrel,ladder,wafer,hybrid);
          if(index < 0) continue;

          if(index == 8 || index == 9 || index == 12 || index == 13) 
	   {
             mSvtAnalClusters = (StSvtAnalysedHybridClusters*)mSvtAnalColl->getObject(barrel,ladder,wafer,hybrid);

             mNumOfClusters = mSvtAnalClusters->numOfHits();
             mSvtHit = mSvtAnalClusters->svtHit();

             for(int i = 0; i < mNumOfClusters; i++)
               {
                int flag = mSvtHit[i].flag();
                if(flag == 0)
                 {
                  ++mTotNumOfGoodClusters;
		 }
	       }

	    }
	} //barrel loop
      } //ladder loop
   } //wafer loop
 } // hybrid loop


}
                  
 
//___________________________________________________________________________________________________


Int_t StSvtClusterAnalysisMaker::GetRawData(int index)
{
  int* anodeList;
  int numOfAnodes,numOfSeq,seqStart;
  unsigned char* adc;
  StSequence* svtSequence;

  numOfAnodes = mHybridRawData->getAnodeList(anodeList);
 
  //cout<<"numOfAnodes for hybrid index"<<index<<" = "<<numOfAnodes<<endl;

  int counter = 0;

  int an  = 0;
  int seq = 0, mseq = 0;
  do 
    {
     while(an + 1)
       {
        if(an < numOfAnodes)
         {
           mHybridRawData->getListSequences(an,numOfSeq,svtSequence);
           seqStart =  svtSequence[seq].startTimeBin;
           adc = svtSequence[seq].firstAdc;
           //cout<<(int)adc[seqStart + mseq] - 100<<" ";
           ++an;
	 }
        else if(an == numOfAnodes)
        an = -1; 
      }

     cout<<"\n\n";
     an = 0;
     ++mseq;

    } while(!an && seqStart + mseq < 128);

         
 cout<<"\n\n";
 
 cout<<"numOfSeq for hybrid index"<<index<<" = "<<counter<<endl;
 
 return kStOK;
}

//___________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetPixelData(int index)
{
  cout<<"hybrid index:  "<<index<<endl;
  cout<<"\n";

  for(int tim = 0; tim < 128; tim++)
    {
      cout<<"\n";
     for(int an = 1; an <= 240; an++)
       {
         if(an < 30)
	 cout<<"Pixel"<<"["<<an - 1<<"]"<<"["<<tim<<"] = "<< mHybridPixelData->getPixelContent(an,tim)<<endl;
      }
    }
 
 return kStOK;
}

//____________________________________________________________________________
 

void StSvtClusterAnalysisMaker::MakeHistograms(){
  
  int mSequence;
  int listAn, actualAn,seq,stTimeBin,len;
  unsigned char* adc;
  int index = 0;

  StSequence* svtSequence;
  StSvtClusterMemberInfo** tempMemberInfo; 
  
  mNoEvents++;
  int TotalClusters=0;
 
 for(int barrel = 1;barrel <= mSvtAdjEvent->getNumberOfBarrels();barrel++) {
   for(int ladder = 1;ladder <= mSvtAdjEvent->getNumberOfLadders(barrel);ladder++) {
     for(int wafer = 1;wafer <= mSvtAdjEvent->getNumberOfWafers(barrel);wafer++) {
       for(int hybrid = 1;hybrid <=mSvtAdjEvent->getNumberOfHybrids();hybrid++){
           
         index = mSvtAdjEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
         if(index < 0) continue;

         //Reset histogram each event
 
         m_time_anode_clu[index]->Reset();
         m_time_anode_raw[index]->Reset();

         mHybridAdjData = (StSvtHybridData *)mSvtAdjEvent->at(index); 
	 if( !mHybridAdjData) continue;
         mHybridCluster = (StSvtHybridCluster*)mSvtClusterColl->at(index); 

         mNumOfClusters = mHybridCluster->getNumberOfClusters(); 

	 TotalClusters+=mNumOfClusters;
	 gMessMgr->Message()<<"numOfClusters = "<<mNumOfClusters << " For index = " << index<< " " ;
	 
         tempMemberInfo =  new StSvtClusterMemberInfo*[mNumOfClusters];
	 
         for(int clu = 0; clu < mNumOfClusters; clu++)
           {
	     
	     mTotNumOfClusters += mNumOfClusters;

            tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
            mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);

            for(int mem = 0; mem < mNumOfMembers; mem++)
              {
               listAn = tempMemberInfo[clu][mem].listAnode;
               seq =  tempMemberInfo[clu][mem].seq;
               actualAn =  tempMemberInfo[clu][mem].actualAnode; //actual anode
               mHybridAdjData->getListSequences(listAn,mSequence,svtSequence);
	       

               stTimeBin =svtSequence[seq].startTimeBin; 
               len = svtSequence[seq].length; 
               adc = svtSequence[seq].firstAdc;
	       
	       int count=0;
	       for(int k = 0; k < len ; k++)
                 {
	          count = (int) adc[k];
	          m_time_anode_clu[index]->Fill(actualAn,stTimeBin + k,clu + 1);
	          m_time_anode_raw[index]->Fill(actualAn,stTimeBin + k,count); 
	         }
	       
	      }
	   }
	 delete [] tempMemberInfo;
	} //hybrid loop
      } //wafer loop
   } //ladder loop
 } //barrel loop


 gMessMgr->Message()<< " Found " << TotalClusters << " clusters."<< endm;

 m_nClust->Fill((float)mNoEvents,(float)TotalClusters);
}
//____________________________________________________________________________
Int_t StSvtClusterAnalysisMaker::Reset(){

   delete mSvtAnalClusters; 
   delete mSvtAnalColl;
   delete mSvtAnalysis;

   mSvtAdjEvent     = NULL;

   mHybridRawData   = NULL;
   mHybridAdjData   = NULL;
   mHybridPixelData = NULL;

   mSvtRawEventColl = NULL;
   mSvtClusterColl  = NULL;
   mSvtPixelColl    = NULL;
   mSvtAnalColl     = NULL;

   mSvtAnalysis     = NULL;
   mSvtHit          = NULL;
   mSvtAnalClusters = NULL; 

   m_ConstSet->Delete();

   return kStOK;
}

//____________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::Finish(){

 if (Debug()) gMessMgr->Debug() << "In StSvtClusterAnalysisMaker::Finish() ..."
				<<   GetName() << endm;

  return kStOK;
}
//____________________________________________________________________________

void StSvtClusterAnalysisMaker::Clear(Option_t *option){

  StMaker::Clear(option);
}

//_____________________________________________________________________________
ClassImp(StSvtClusterAnalysisMaker)
 








