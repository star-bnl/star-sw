/***************************************************************************
 *
 * $Id: StSvtEmbeddingMaker.cxx,v 1.2 2003/09/07 03:49:06 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Svt Embedding Maker class
 *
 ***************************************************************************
 *
 * $Log: StSvtEmbeddingMaker.cxx,v $
 * Revision 1.2  2003/09/07 03:49:06  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.1  2003/07/31 19:18:09  caines
 * Petrs improved simulation code
 *
 **************************************************************************/

#include "Stiostream.h"
#include <math.h>

#include "TH2.h"
#include "TFile.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StSequence.hh"
#include "StSvtEmbeddingMaker.h"
#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPixelsC.hh"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StSvtHybridSimData.hh"

ClassImp(StSvtEmbeddingMaker)

//____________________________________________________________________________
StSvtEmbeddingMaker::StSvtEmbeddingMaker(const char *name):StMaker(name)
{
  mSimPixelData = NULL;
  mRealData = NULL;
  mSimPixelDataColl = NULL;
  mRealDataColl = NULL;
}

//____________________________________________________________________________
StSvtEmbeddingMaker::~StSvtEmbeddingMaker()
{

}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Init()
{

  if (Debug()) gMessMgr->Debug() << "In StSvtEmbeddingMaker::Init() ..."  << 
		 GetName() << endm;

  St_DataSet *dataSet = GetDataSet("StSvtRawData");
  assert(dataSet); 
  mRealDataColl= (StSvtData*)(dataSet->GetObject());
  assert(mRealDataColl);

  dataSet = GetDataSet("StSvtSimPixels");
  assert(dataSet); 
  mSimPixelDataColl= (StSvtData*)(dataSet->GetObject());
  assert(mSimPixelDataColl);

  bookHistograms();

  return StMaker::Init();
}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Make()
{
  mixData();

  fillHistograms();

  return kStOK;
}

//____________________________________________________________________________
void StSvtEmbeddingMaker::mixData()
{
  StSequence* Sequence;
  int numOfSeq ,startTimeBin, length, adcVal, time, index, pixelIndex, status, previous, newAdc, offset;
  unsigned char* adc;
  int* anolist;

  for(int Barrel = 1;Barrel <= mRealDataColl->getNumberOfBarrels();Barrel++) {  
    for (int Ladder = 1;Ladder <= mRealDataColl->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mRealDataColl->getNumberOfWafers(Barrel);Wafer++) {	
	for( int Hybrid = 1;Hybrid <= mRealDataColl->getNumberOfHybrids();Hybrid++){

	  index = mRealDataColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);

	  if( index < 0) continue;

	  mRealData = (StSvtHybridData *)mRealDataColl->at(index);
	  if (!mRealData) continue; 
            
	  mSimPixelData = (StSvtHybridPixelsC *)mSimPixelDataColl->at(index);
	  if (!mSimPixelData) continue;
            
	  //cout << "mSimPixelData = " << mSimPixelData << endl;
	  offset =  mSimPixelData->getPedOffset();
	  anolist = 0;

	  for (int iAnode= 0; iAnode<mRealData->getAnodeList(anolist); iAnode++){
              
	    int Anode = anolist[iAnode];
	    status= mRealData->getSequences(Anode,numOfSeq,Sequence);

	    for (int nSeq=0; nSeq< numOfSeq ; nSeq++){
  
	      adc=Sequence[nSeq].firstAdc;
	      length = Sequence[nSeq].length;
	      startTimeBin=Sequence[nSeq].startTimeBin;

	      for(int t = 0; t<length; t++){
		time = startTimeBin + t;
		adcVal = (int)adc[t];
		pixelIndex = mSimPixelData->getPixelIndex(Anode,time);
		/*
		if ((Anode == 109) && (time == 10) ) {
		  cout << "pixel(before) = " << (int)mSimPixelData->getPixelContent(Anode,time) << endl;
		  cout << "offset = " << offset << ", adc = " << adcVal << endl;
		}
		*/
	    
		previous = (int)mSimPixelData->getPixelContent(Anode,time);
		if (previous > 0)
		  previous -= offset;
		newAdc = previous + adcVal;

		if (newAdc >=0 && newAdc<255)
		  mSimPixelData->AddAt((unsigned char)newAdc,pixelIndex);
		else if (newAdc < 0)
		  mSimPixelData->AddAt((unsigned char)0,pixelIndex);
		else if (newAdc > 255)
		  mSimPixelData->AddAt((unsigned char)255,pixelIndex);
		//mSimPixelData->setPedOffset(0);
		/*
		if ((Anode == 109) && (time == 10) )
		  cout << "pixel(after) = " << (int)mSimPixelData->getPixelContent(Anode,time) << endl;
		*/
	      }
	    }
	  }
          
	  if(mRealData) 
	    delete mRealData;

	  mSvtEmbeddedData = new StSvtHybridSimData(Barrel,Ladder,Wafer,Hybrid);
	  mSvtEmbeddedData->setSimHybridData(mSimPixelData);
	  mRealDataColl->put_at(mSvtEmbeddedData,index);

	  mSimPixelData->Reset();
	}
      }
    }
  }
}

//____________________________________________________________________________
Int_t StSvtEmbeddingMaker::Finish()
{ 
  if (Debug()) gMessMgr->Debug() << "In StSvtSimulationMaker::Finish() ..."
			       << endm;
  
  mFile->Write();
  mFile->Close();

  return kStOK;
}

//____________________________________________________________________________
void StSvtEmbeddingMaker::bookHistograms()
{ 
  mFile = new TFile("embedding.root","RECREATE","Embedding");
  
  char index[10];
  char preTitle[25]; 
  char* title; 
  int indexHyb;

  mDataHist = new TH2F*[mRealDataColl->getTotalNumberOfHybrids()];
  
  for(int Barrel = 1;Barrel <= mRealDataColl->getNumberOfBarrels();Barrel++) {    
    for (int Ladder = 1;Ladder <= mRealDataColl->getNumberOfLadders(Barrel);Ladder++) {      
      for (int Wafer = 1;Wafer <= mRealDataColl->getNumberOfWafers(Barrel);Wafer++) {	
	for( int Hybrid = 1;Hybrid <= mRealDataColl->getNumberOfHybrids();Hybrid++){
	  {
	    indexHyb = mRealDataColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
	    if( indexHyb < 0) continue; 
	 
	    sprintf(preTitle,"embeddedData");
	    sprintf(index,"b%dl%dw%dh%d",Barrel, Ladder, Wafer, Hybrid);
      
	    title = strcat(preTitle,index);
	    
	    mDataHist[indexHyb] = new TH2F(title,"ADC vs time and anode",240,0.5,240.5,128,0.0,127.0);
	  }
	}
      }
    }
  }
}

//____________________________________________________________________________
void StSvtEmbeddingMaker::fillHistograms()
{
  int* anolist; 
  int mSequence,index;
  int stTimeBin,len,status;
  unsigned char* adc;

 StSequence* svtSequence;

 for(int Barrel = 1;Barrel <= mRealDataColl->getNumberOfBarrels();Barrel++) {    
   for (int Ladder = 1;Ladder <= mRealDataColl->getNumberOfLadders(Barrel);Ladder++) {      
     for (int Wafer = 1;Wafer <= mRealDataColl->getNumberOfWafers(Barrel);Wafer++) {	
       for( int Hybrid = 1;Hybrid <= mRealDataColl->getNumberOfHybrids();Hybrid++){
	 
	 index = mRealDataColl->getHybridIndex(Barrel,Ladder,Wafer,Hybrid);
	 
	 if( index < 0) continue; 
	 
	 mSvtEmbeddedData = (StSvtHybridSimData *)mRealDataColl->at(index);
	 if(!mSvtEmbeddedData)
	   continue;
	 
	 mDataHist[index]->Reset();
	 for(int ianode = 0; ianode < mSvtEmbeddedData->getAnodeList(anolist); ianode++)
	   {
	     int anode = anolist[ianode];
	     status = mSvtEmbeddedData->getSequences(anode,mSequence,svtSequence);
	     
	     for(int mSeq = 0; mSeq < mSequence; mSeq++) 
	       {
		 stTimeBin = svtSequence[mSeq].startTimeBin; 
		 len = svtSequence[mSeq].length;
		 adc = svtSequence[mSeq].firstAdc;
		 for(int j = 0 ; j < len; j++)
		   {
		     float c = (float) adc[j];
		     
		     mDataHist[index]->Fill(anode,stTimeBin + j,c);
		   }
	       }
	   }
       }
     }
   }
 }
}
