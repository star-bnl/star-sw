/***************************************************************************
 *
 * $Id: StSvtClusterAnalysisMaker.cxx,v 1.1 2000/07/06 03:50:33 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Cluster Analysis Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtClusterAnalysisMaker.cxx,v $
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#include "StChain.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "TH1.h"
#include "TH2.h"

#include "StMessMgr.h"

#include "StSvtCluster.hh"
#include "StSvtHybridCluster.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSequence.hh"
#include "StSvtAnalysis.hh"
#include "StSvtClusterAnalysisMaker.h"



//___________________________________________________________________________________________
StSvtClusterAnalysisMaker::StSvtClusterAnalysisMaker(const char *name) : StMaker(name)
{
   mSvtEvent = NULL;
   mHybridData = NULL;
   mHybridCluster = NULL;
   mSvtCluster = NULL;
   mSvtAnalysis = NULL;
}

//____________________________________________________________________________________________
StSvtClusterAnalysisMaker::~StSvtClusterAnalysisMaker(){

}


//_____________________________________________________________________________________________
Int_t StSvtClusterAnalysisMaker::Init()
{
  if( Debug()) gMessMgr->Debug() <<"In StSvtClusterAnalysisMaker::Init()"<<endm;

  mNoEvents=0;

  GetSvtCluster();
  GetSvtEvent();
 
  mTotalNumberOfHybrids = mSvtCluster->getTotalNumberOfHybrids();
  CreateClusterHist(mTotalNumberOfHybrids);

  mSvtAnalysis = new StSvtAnalysis();

  return  StMaker::Init();

}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtEvent()
{
  St_DataSet *dataSet;
  
  dataSet = GetDataSet("StSvtData");
  assert(dataSet); 
  mSvtEvent = (StSvtData*)(dataSet->GetObject());
  assert(mSvtEvent);

  return kStOK;
}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::GetSvtCluster()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtCluster");
  assert(dataSet);
  mSvtCluster = (StSvtCluster*)(dataSet->GetObject());
  assert(mSvtCluster);

  return kStOK;
}

//__________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::CreateClusterHist(Int_t tNuOfHyb)
{
   // Create Histograms
   m_n_seq = new TH1F("NSeqClust","No. Pixels on cluster",100,0.,30.);
   m_nClust = new TH2F("NClust","No. clusters per event",1000,0.,1000.,100,0.,30.);
   m_SumADCvsTime = new TH2F("SumAdcVsTime" ,"Time bucket vs Sum ADC",128,0.,128.,1000,0,100);
   m_time_anode_clu = new TH2F*[tNuOfHyb];
   m_time_anode_raw = new TH2F*[tNuOfHyb];
   m_sumADC = new TH1F*[tNuOfHyb];

  char title1[16];
  char titleraw[16];
  char titleadc[16];
  char  title2[3];
  char* title3;
  char* titlerawc;
  char* titleadcc;
  
  for (int barrel = 1;barrel <= mSvtCluster->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtCluster->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtCluster->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtCluster->getNumberOfHybrids();hybrid++) {
           
            int index = mSvtCluster->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
            sprintf(title1,"TimAnodecluster");
	    sprintf(titleraw,"TimAnodeRaw");
	    sprintf(titleadc,"ADC");
            sprintf(title2,"%d", index);
            title3 = strcat(title1,title2);
            titlerawc = strcat(titleraw,title2);
	    titleadcc = strcat(titleadc,title2);
	    // cout << title3 << endl;
            m_time_anode_clu[index] = new TH2F(title3 ,"Time bucket vs anode",240,0.,240.,128.,0.,128.);	    m_time_anode_raw[index] = new TH2F(titlerawc ,"Time bucket vs anode",240,0.,240.,128.,0.,128.);
	     m_sumADC[index] = new TH1F(titleadcc,"Sum of ADC counts in cluster",100,0,1000);

	}
      }
    }
  }


  return kStOK;
}


//_______________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::Make()
{
  cout<<" In StSvtClusterAnalysisMaker::Make()"<<endl;


  //              Create output tables
  St_scs_spt    *scs_spt    = new St_scs_spt("scs_spt",100);
  m_DataSet->Add(scs_spt);

  SetClusterAnalysis();

  MakeHistograms();

  return kStOK;

}

//___________________________________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::SetClusterAnalysis()
{
 int index =0;
 
 for(int barrel = 1;barrel <= mSvtEvent->getNumberOfBarrels();barrel++) {
   //cout<<mSvtEvent->getNumberOfBarrels()<<endl;
   for (int ladder = 1;ladder <= mSvtEvent->getNumberOfLadders(barrel);ladder++) {
     //cout<<mSvtEvent->getNumberOfLadders(barrel)<<endl;
      for (int wafer = 1;wafer <= mSvtEvent->getNumberOfWafers(barrel);wafer++) {
	// cout<<mSvtEvent->getNumberOfWafers(barrel)<<endl;
	for (int hybrid = 1;hybrid <=mSvtEvent->getNumberOfHybrids();hybrid++){
          //cout<<mSvtEvent->getNumberOfHybrids()<<endl;  
           
          index = mSvtEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
          if(index < 0) continue;

          mHybridData = (StSvtHybridData *)mSvtEvent->at(index);
	  if( !mHybridData) continue;
          mHybridCluster = (StSvtHybridCluster*)mSvtCluster->at(index); 

          numOfClust = mHybridCluster->getNumberOfClusters(); 
 
          mSvtAnalysis->SetPointers(mHybridData,mHybridCluster);
          mSvtAnalysis->FirstAndLastAnodes();
          mSvtAnalysis->CluFirstTimeBin();
          mSvtAnalysis->CluLastTimeBin();
          mSvtAnalysis->MomentAnalysis(mSvtEvent->getPedOffset());
          mSvtAnalysis->Report(index);

          SaveIntoTable(numOfClust,barrel, ladder, wafer, hybrid);
	  if( index != 11 && index !=10){
	    for( int clu=0; clu< numOfClust; clu++){
	      m_sumADC[index]->Fill(mSvtAnalysis->GetSumADC(clu));
	      m_SumADCvsTime->Fill((float)mSvtAnalysis->MeanClusterTimeBin(clu),(float)mSvtAnalysis->GetSumADC(clu));
	    }
	  }
          mSvtAnalysis->ResetMeanValues();

        }
      }
    }
  }


 return kStOK;
}

//___________________________________________________________________________________________________

void StSvtClusterAnalysisMaker::MakeHistograms(){
  
  int mSequence;
  int listAn, actualAn,seq,stTimeBin,len;
  unsigned char* adc;
  int index = 0;

  StSequence* svtSequence;
  StSvtClusterMemberInfo** tempMemberInfo; 
  
  mNoEvents++;
  int TotalClusters=0;
 
 for(int barrel = 1;barrel <= mSvtEvent->getNumberOfBarrels();barrel++) {
   for(int ladder = 1;ladder <= mSvtEvent->getNumberOfLadders(barrel);ladder++) {
     for(int wafer = 1;wafer <= mSvtEvent->getNumberOfWafers(barrel);wafer++) {
       for(int hybrid = 1;hybrid <=mSvtEvent->getNumberOfHybrids();hybrid++){
           
         index = mSvtEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
         if(index < 0) continue;

         //Reset histogram each event
 
         m_time_anode_clu[index]->Reset();
         m_time_anode_raw[index]->Reset();

         mHybridData = (StSvtHybridData *)mSvtEvent->at(index); 
	 if( !mHybridData) continue;
         mHybridCluster = (StSvtHybridCluster*)mSvtCluster->at(index); 

         numOfClust = mHybridCluster->getNumberOfClusters(); 
	 TotalClusters+=numOfClust;
  
         cout<<"numOfClusters = "<<numOfClust << " For index = " << index<<endl;

         tempMemberInfo =  new StSvtClusterMemberInfo*[numOfClust];
           
         for(int clu = 0; clu < numOfClust; clu++)
           {


            tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
            numOfMembers = mHybridCluster->getNumberOfMembers(clu);

	    if( index !=11){
	    m_n_seq->Fill(numOfMembers);
	    }

            for(int mem = 0; mem < numOfMembers; mem++)
              {
               listAn = tempMemberInfo[clu][mem].listAnode;
               seq =  tempMemberInfo[clu][mem].seq;
               actualAn =  tempMemberInfo[clu][mem].actualAnode; //actual anode
               mHybridData->getListSequences(listAn,mSequence,svtSequence);

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
	} //hybrid loop
      } //wafer loop
   } //ladder loop
 } //barrel loop

 
 m_nClust->Fill((float)mNoEvents,(float)TotalClusters);
   //cout<<"******* making histograms finished *******"<<endl; 
}


//____________________________________________________________________________
 
 void StSvtClusterAnalysisMaker::SaveIntoTable(int numOfClusters, int barrel, int ladder, int wafer, int hybrid)
   {
     int nRows=0;
     StSvtHybridObject *myHybridObject;
     
     //            Create an iterator
      St_DataSetIter svt_spt(m_DataSet);
     St_scs_spt  *scs_spt=0  ;
     scs_spt  = (St_scs_spt *) svt_spt.Find("scs_spt");
     

  if( scs_spt){

    nRows=scs_spt->GetNRows();
    if( nRows+numOfClusters > scs_spt->GetTableSize()){
      scs_spt->ReAllocate(nRows+numOfClusters);
    }

    scs_spt_st *spt= scs_spt->GetTable();
    
    spt +=nRows;
    for( int i=0; i<numOfClusters; i++,spt++){
      spt->id = i+1;
      spt->xl[0] = mSvtAnalysis->MeanClusterTimeBin(i);
      spt->xl[1] = mSvtAnalysis->MeanClusterAnode(i);
      spt->xl[2] = 0.015;
      spt->flag=0;
      spt->id_cluster=i+1;
      spt->id_globtrk=0;
      spt->id_match=0;
      spt->id_mctrack=0;
      spt->id_track=0;
      spt->mom2[0]= mSvtAnalysis->GetLastTimeBin(i) -
	mSvtAnalysis->GetFirstTimeBin(i) +1;
      spt->mom2[1]= mSvtAnalysis->GetLastAnode(i) -
	mSvtAnalysis->GetFirstAnode(i) +1;
      spt->de[0]=mSvtAnalysis->GetSumADC(i);
      spt->de[1]=0;
      myHybridObject = (StSvtHybridObject *)
	mSvtCluster->getObject(barrel, ladder, wafer,  hybrid);
      spt->id_wafer=myHybridObject->getLayerID()*1000+100*
	wafer+ladder;
 
      for( int j=0; j<3; j++){
	spt->cov[j]=0;
	spt->res[j]=0;
	spt->x[j]=0;
      }
      
    }
    scs_spt->SetNRows(nRows+numOfClusters);
    }

}

//____________________________________________________________________________

Int_t StSvtClusterAnalysisMaker::Finish(){

  printf("In StSvtClusterAnalysisMaker::Finish() ...\n"); 


  return kStOK;
}

//_____________________________________________________________________________
ClassImp(StSvtClusterAnalysisMaker)
 








