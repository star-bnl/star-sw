/***************************************************************************
 *
 * $Id: StSvtBadAnodesMaker.cxx,v 1.2 2004/02/04 16:13:56 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Bad Anodes Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtBadAnodesMaker.cxx,v $
 * Revision 1.2  2004/02/04 16:13:56  munhoz
 * fix few problems with pedestal reading
 *
 * Revision 1.1  2004/02/03 20:06:28  munhoz
 * first version of bad anode maker
 *
 *
 **************************************************************************/

#include <fstream>

#include "TH2.h"
#include "TString.h"
#include "TFile.h"
#include "TKey.h"
#include "TObjectSet.h"
#include "StMessMgr.h"

#include "StSvtBadAnodesMaker.h"
#include "StarClassLibrary/StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridPed.hh"
#include "map.h"

#define MAX_NUMBER_OF_ANODES 240

ClassImp(StSvtBadAnodesMaker)

//_____________________________________________________________________________
StSvtBadAnodesMaker::StSvtBadAnodesMaker(const char *name):StMaker(name)
{
  mSvtData = NULL;
  mHybridData = NULL;
  mSvtBadAnodes = NULL;
  mHybridBadAnodes = NULL;
  mSvtPed = NULL;
  mSvtRMSPed = NULL;

  mBadAnodesSet = NULL;
  
  mRmsScaleFactor = 16;

  NULL_ADC = 0;                      //
  OVERLOADED_ADC = 255;              // 
  BAD_RMS = 10;                      // in ADC counts
  BAD_MEAN_PED_MIN = 0;                //
  BAD_MEAN_PED_MAX = 255;                //
  BAD_MEAN_RMS_MIN = 0;                //
  BAD_MEAN_RMS_MAX = 255;                //

  NULL_ADC_THRESHOLD = 129;           //
  OVERLOADED_ADC_THRESHOLD = 129;     //  in number of time bins
  OCCUP_THRESHOLD = 129;              //
  RMS_THRESHOLD = 129;                //
  
  FREQ_OVERLOADED_ADC = 1.1;           //
  FREQ_NULL_ADC = 1.1;                 //  fraction of events
  FREQ_OCCUP = 1.1;                    //

  mFileName = new TString("badAnodes.root");
}

//_____________________________________________________________________________
StSvtBadAnodesMaker::~StSvtBadAnodesMaker()
{}

void StSvtBadAnodesMaker::setOutputFile(const char* name)
{
  if (mFileName)
    delete mFileName;

  mFileName = new TString(name);
}

//_____________________________________________________________________________
Int_t StSvtBadAnodesMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSvtBadAnodesMaker::Init" << endm;

  mEvents = 0;

  setSvtData();
  setPedestal();
  setRMSPedestal();

  setSvtBadAnodes();

  /*
  setPedestal();

  setRMSPedestal();
  */

  if (Debug()) bookHistograms();

  gMessMgr->Info() << "NULL_ADC = "  << NULL_ADC << endm;               
  gMessMgr->Info() << "OVERLOADED_ADC = " << OVERLOADED_ADC << endm;

  gMessMgr->Info() << "THRESHOLD NULL_ADC (fraction of time bins) = " << NULL_ADC_THRESHOLD << endm; 
  gMessMgr->Info() << "THRESHOLD OVERLOADED_ADC (fraction of time bins) = " << OVERLOADED_ADC_THRESHOLD << endm;
  gMessMgr->Info() << "THRESHOLD OCCUP (fraction of time bins) = " << OCCUP_THRESHOLD << endm;
  
  gMessMgr->Info() << "FREQUENCY OVERLOADED_ADC (fraction of events) = " << FREQ_OVERLOADED_ADC << endm;
  gMessMgr->Info() << "FREQUENCY NULL_ADC (fraction of events) = " << FREQ_NULL_ADC << endm;
  gMessMgr->Info() << "FREQUENCY OCCUP (fraction of events) = " << FREQ_OCCUP << endm; 

  gMessMgr->Info() << "BAD MEAN PED_MIN = " << BAD_MEAN_PED_MIN << endm;
  gMessMgr->Info() << "BAD MEAN PED_MAX = " << BAD_MEAN_PED_MAX << endm;
  gMessMgr->Info() << "BAD MEAN RMS_MIN = " << BAD_MEAN_RMS_MIN << endm;
  gMessMgr->Info() << "BAD MEAN RMS_MAX = " << BAD_MEAN_RMS_MAX << endm;

  return StMaker::Init();
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::setSvtData()
{
  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtRawData");
  assert(dataSet);
  mSvtData = (StSvtData*)(dataSet->GetObject());
  assert(mSvtData);
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::setPedestal()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::setPedestal" << endm;

  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtPedestal");

  if (dataSet) {
    mSvtPed = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(mSvtPed);
  }
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::setRMSPedestal()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::setRMSPedestal" << endm;

  St_DataSet *dataSet;

  dataSet = GetDataSet("StSvtRMSPedestal");

  if (dataSet) {
    mSvtRMSPed = (StSvtHybridCollection*)(dataSet->GetObject());
    assert(mSvtRMSPed);
  }
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::setSvtBadAnodes()
{
  St_DataSet *dataset;

  dataset = new TObjectSet("StSvtBadAnodes");
  AddConst(dataset);  

  if (!mSvtBadAnodes) {
    mSvtBadAnodes = new StSvtHybridCollection(mSvtData->getConfiguration());
    dataset->SetObject((TObject*)mSvtBadAnodes);
    assert(mSvtBadAnodes);
  }
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::bookHistograms()
{
  char posTitle[10];
  char preTitle[25]; 
  char* title; 
  int index, index2;

  mFile = new TFile(mFileName->Data(),"RECREATE");

  mBadAnodesHist = new TH2F*[mSvtData->getTotalNumberOfHybrids()];

  mBadAnodesBarrel = new TH1F*[3];
  mBadAnodesBarrel[0] = new TH1F("barrel1","Fraction of Bad Anodes",8,0.5,8.5);
  mBadAnodesBarrel[1] = new TH1F("barrel2","Fraction of Bad Anodes",12,0.5,12.5);
  mBadAnodesBarrel[2] = new TH1F("barrel3","Fraction of Bad Anodes",16,0.5,16.5);

  mBadAnodesLadder = new TH1F*[36];

  for (int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
	for (int hybrid = 1;hybrid <= mSvtData->getNumberOfHybrids();hybrid++) {
           
	    index = mSvtData->getHybridIndex(barrel,ladder,wafer,hybrid);
            if(index < 0) continue;
            
	    sprintf(preTitle,"BadAnodes");
	    sprintf(posTitle,"b%dl%dw%dh%d",barrel, ladder, wafer, hybrid);      
	    title = strcat(preTitle,posTitle);

	    mBadAnodesHist[index] = new TH2F(title,"Bad Anodes ADC",240,0.5,240.5,128,-0.5,127.5);
	}
      }

      sprintf(preTitle,"ladder");
      sprintf(posTitle,"b%dl%d",barrel, ladder);      
      title = strcat(preTitle,posTitle);

      switch (barrel) {
      case 1:
	index2 = (ladder-1);
	mBadAnodesLadder[index2] = new TH1F(title,"Fraction of Bad Anodes",8,0.5,8.5);
	break;
      case 2:
	index2 = mSvtData->getNumberOfLadders(barrel) + (ladder-1);
	mBadAnodesLadder[index2] = new TH1F(title,"Fraction of Bad Anodes",12,0.5,12.5);
	break;
      case 3:
	index2 = mSvtData->getNumberOfLadders(barrel) + (ladder-1);
	mBadAnodesLadder[index2] = new TH1F(title,"Fraction of Bad Anodes",14,0.5,14.5);
	break;
      }
    }
  }

}

//_____________________________________________________________________________
Int_t StSvtBadAnodesMaker::Make()
{
  int adc, anode, nAnodes, nSeq, iseq, time, timeSeq, status, index;
  int* anodeList;
  StSequence* Seq;
  int nullAdc, overloadedAdc, occup, badRMS;
  float ped,rms,meanPed, meanRms;

  setSvtData();

  mEvents++;

  // scan through the data
  if (mSvtData) {
  for (int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
      for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid = 1;hybrid <= mSvtData->getNumberOfHybrids();hybrid++) {
	    
	    index = mSvtData->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index < 0) continue;
	    
	    mHybridData = (StSvtHybridData*)mSvtData->at(index);
	    
	    if (!mHybridData) continue;
	    
	    mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	    if (!mHybridBadAnodes)
	      mHybridBadAnodes = new StSvtHybridBadAnodes(barrel, ladder, wafer, hybrid);
	    
	    anodeList = NULL;
	    nAnodes = mHybridData->getAnodeList(anodeList);
	    
	    for (int ianode=0;ianode<nAnodes;ianode++) { // loop over anodes
	      
	      // initialize variables
	      nullAdc = 0;
	      overloadedAdc = 0;
	      occup = 0;
	      badRMS = 0;
	      
	      anode = anodeList[ianode];
	      Seq = NULL;
	      nSeq = 0;
	      
	      status = mHybridData->getSequences(anode,nSeq,Seq);
	      
	      for (iseq=0;iseq<nSeq;iseq++) {	  	  
		for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {
		  
		  time = Seq[iseq].startTimeBin + timeSeq;
		  adc =(int)Seq[iseq].firstAdc[timeSeq];
		  
		  // check whether adc is NULL
		  if (adc == NULL_ADC)
		    nullAdc++;
		  
		  // check whether adc is overloaded
		  if (adc == OVERLOADED_ADC)
		    overloadedAdc++;
		  
		  // check the occupancy
		  if ((adc > 0) && (adc < 255))
		    occup++;		
		}
	      }
	      
	      if (nullAdc >= NULL_ADC_THRESHOLD)
		mHybridBadAnodes->addNullAdc(anode,kTRUE,mEvents);
	      else
		mHybridBadAnodes->addNullAdc(anode,kFALSE,mEvents);
	      if (overloadedAdc >= OVERLOADED_ADC_THRESHOLD)
		mHybridBadAnodes->addOverloadedAdc(anode,kTRUE,mEvents);
	      else
		mHybridBadAnodes->addOverloadedAdc(anode,kFALSE,mEvents);
	      if (occup >= OCCUP_THRESHOLD) 
		mHybridBadAnodes->addHighOccup(anode,kTRUE,mEvents);
	      else
		mHybridBadAnodes->addHighOccup(anode,kFALSE,mEvents);
	    }
	    
	    mSvtBadAnodes->put_at((TObject*)mHybridBadAnodes,index);
 }
	}
      }
    }
  }

  // scan through the data
  if (mSvtPed) {
    for (int barrel = 1;barrel <= mSvtPed->getNumberOfBarrels();barrel++) {
      for (int ladder = 1;ladder <= mSvtPed->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= mSvtPed->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid = 1;hybrid <= mSvtPed->getNumberOfHybrids();hybrid++) {
	    
            index = mSvtPed->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index < 0) continue;

	    mHybridPed = (StSvtHybridPed*)mSvtPed->at(index);

	    if (!mHybridPed) continue;

	    mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	    if (!mHybridBadAnodes)
	      mHybridBadAnodes = new StSvtHybridBadAnodes(barrel, ladder, wafer, hybrid);
	    
	    for (int anode=1;anode<=240;anode++) { // loop over anodes
              meanPed=0;
	      for (time=4; time<127; time++) {
		
                 // check pedestal
                 ped = mHybridPed->At(mHybridPed->getPixelIndex(anode, time));		
		 meanPed += ped;
              }
	      
	      meanPed /= 123;

	      if ((meanPed < BAD_MEAN_PED_MIN) || (meanPed > BAD_MEAN_PED_MAX))
		mHybridBadAnodes->setBadAnode(anode);
	    }
	    
	    mSvtBadAnodes->put_at((TObject*)mHybridBadAnodes,index);
	  }
	}
      }
    }
  }

  // scan through the data
  if (mSvtRMSPed) {
    for (int barrel = 1;barrel <= mSvtRMSPed->getNumberOfBarrels();barrel++) {
      for (int ladder = 1;ladder <= mSvtRMSPed->getNumberOfLadders(barrel);ladder++) {
	for (int wafer = 1;wafer <= mSvtRMSPed->getNumberOfWafers(barrel);wafer++) {
	  for (int hybrid = 1;hybrid <= mSvtRMSPed->getNumberOfHybrids();hybrid++) {
	    
	    index = mSvtRMSPed->getHybridIndex(barrel, ladder, wafer, hybrid);
	    if (index < 0) continue;

	    mHybridRMSPed = (StSvtHybridPed*)mSvtRMSPed->at(index);
	
 	    if (!mHybridRMSPed) continue;
   
	    mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);
	    if (!mHybridBadAnodes)
	      mHybridBadAnodes = new StSvtHybridBadAnodes(barrel, ladder, wafer, hybrid);

	    for (int anode=1;anode<=240;anode++) { // loop over anodes
	      meanRms=0;
	      for (time=4; time<127; time++) {
		
		// check pedestal RMS
                rms = (mHybridRMSPed->At(mHybridRMSPed->getPixelIndex(anode, time)))/mRmsScaleFactor;		
		meanRms += rms;		
		//if (rms > BAD_RMS)
		//badRMS++;		
	      }
	    
	      meanRms /= 123;
	      
	      //if (badRMS >= RMS_THRESHOLD)
	      //mHybridBadAnodes->setBadAnode(anode);
	      if ((meanRms < BAD_MEAN_RMS_MIN) || (meanRms > BAD_MEAN_RMS_MAX))
		mHybridBadAnodes->setBadAnode(anode);
	    }

	    mSvtBadAnodes->put_at((TObject*)mHybridBadAnodes,index);
	  }
	}
      }
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::writeToFile(const char* fileName)
{
  mFileNameTxt = new TString(mFileName->Data());
  mFileNameTxt->ReplaceAll("root","txt");
  ofstream file2(mFileNameTxt->Data());
  mFileNameTxt->ReplaceAll("txt","Daq.txt");
  ofstream file(mFileNameTxt->Data());

  int anode, index, nTotalBadAnodes = 0;
  int recBoard, mezz, mz_hyb;

  // scan through the bad anodes data and write them out
  for (int barrel = 1;barrel <= mSvtBadAnodes->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtBadAnodes->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtBadAnodes->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtBadAnodes->getNumberOfHybrids();hybrid++) {

	  blwh2rma(barrel, ladder, wafer, hybrid, recBoard, mezz, mz_hyb);

	  index = mSvtBadAnodes->getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (index < 0) continue;

	  mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);

	  if (!mHybridBadAnodes) continue;

	  for (int ianode=0;ianode<MAX_NUMBER_OF_ANODES;ianode++) {
	    anode = ianode + 1;
	    if (mHybridBadAnodes->isBadAnode(anode)) {
	      	  file << recBoard << "  " << mezz << "  " << mz_hyb << "  " 
		       << anode << "  " << "0.0" << endl;
		  file2 << barrel << "  " << ladder << "  " << wafer << "  " << hybrid << "  " << anode << endl;
		  nTotalBadAnodes++;	
	    }
	  }
	}
      }
    }
  }

  file2 << (float)nTotalBadAnodes/((float)MAX_NUMBER_OF_ANODES*432.) << endl;
}

//_____________________________________________________________________________
Int_t StSvtBadAnodesMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSvtBadAnodesMaker::Finish" << endm;

  cout << FREQ_NULL_ADC << "  " << FREQ_OVERLOADED_ADC << "  " << FREQ_OCCUP << endl;

  int adc, anode, nSeq, iseq, time, timeSeq, status, index, index2, ihybrid;
  int nBadAnodesHyb=0, nBadAnodesLadder=0;
  StSequence* Seq;
  float fraction;

  // scan through the bad anodes data and check frequency of appearance
  for (int barrel = 1;barrel <= mSvtBadAnodes->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtBadAnodes->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtBadAnodes->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtBadAnodes->getNumberOfHybrids();hybrid++) {

	  index = mSvtBadAnodes->getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (index < 0) continue;

	  mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);

	  if (!mHybridBadAnodes) continue;

	  mHybridData = (StSvtHybridData*)mSvtData->at(index);

	  if (!mHybridData) continue;

	  for (int ianode=0;ianode<MAX_NUMBER_OF_ANODES;ianode++) {
	    anode = ianode + 1;

	    if ((mHybridBadAnodes->getNullAdc(anode) >= FREQ_NULL_ADC) ||
		(mHybridBadAnodes->getOverloadedAdc(anode) >= FREQ_OVERLOADED_ADC) ||
		(mHybridBadAnodes->getHighOccup(anode) >= FREQ_OCCUP))
	      mHybridBadAnodes->setBadAnode(anode);
	    
	    // fill histograms
	    if (Debug()) {

	      if (mHybridBadAnodes->isBadAnode(anode)) {

		nBadAnodesHyb++;
		nBadAnodesLadder++;
	    
		Seq = NULL;
		nSeq = 0;
		
		status = mHybridData->getSequences(anode,nSeq,Seq);
	    
		for (iseq=0;iseq<nSeq;iseq++) {	  	  
		  for (timeSeq=0; timeSeq<Seq[iseq].length; timeSeq++) {

		    time = Seq[iseq].startTimeBin + timeSeq;
		    adc =(int)Seq[iseq].firstAdc[timeSeq];
		
		    mBadAnodesHist[index]->Fill(anode,time,adc);
		  }
		}
	      }
	    }
	  }

	  if (Debug()) {
	    ihybrid = (wafer-1)*2+hybrid;
	    fraction = (float)nBadAnodesHyb/240.;
	    
	    switch (barrel) {
	    case 1:
	      index2 = (ladder-1);
	      mBadAnodesLadder[index2]->Fill(ihybrid,fraction);
	      break;
	    case 2:
	      index2 = mSvtData->getNumberOfLadders(barrel) + (ladder-1);
	      mBadAnodesLadder[index2]->Fill(ihybrid,fraction);
	      break;
	    case 3:
	      index2 = mSvtData->getNumberOfLadders(barrel) + (ladder-1);
	      mBadAnodesLadder[index2]->Fill(ihybrid,fraction);
	      break;
	    }	  

	  nBadAnodesHyb = 0;
	  }
	}
      }

      if (Debug()) {
	fraction = (float)nBadAnodesLadder/(240.*(float)mSvtBadAnodes->getNumberOfLadders(barrel));
	mBadAnodesBarrel[barrel-1]->Fill(ladder,fraction);
	nBadAnodesLadder = 0;
      }
    }
  }

  writeToFile();

  if (Debug()) {
    mFile->Write();
    mFile->Close();
  }

  return kStOK;
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::Reset()
{
  if (Debug()) gMessMgr->Debug() << "StSvtBadAnodesMaker::Clear" << endm;

  int index;
  for (int barrel = 1;barrel <= mSvtBadAnodes->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtBadAnodes->getNumberOfLadders(barrel);ladder++) {
      for (int wafer = 1;wafer <= mSvtBadAnodes->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtBadAnodes->getNumberOfHybrids();hybrid++) {

	  index = mSvtBadAnodes->getHybridIndex(barrel, ladder, wafer, hybrid);
	  if (index < 0) continue;

	  mHybridBadAnodes = (StSvtHybridBadAnodes*)mSvtBadAnodes->at(index);

	  if (mHybridBadAnodes) delete mHybridBadAnodes;
	  mSvtBadAnodes->put_at(NULL,index);
	}
      }
    }
  }
}

//_____________________________________________________________________________
void StSvtBadAnodesMaker::blwh2rma(int barrel, int ladder, int wafer, int hybrid, 
				   int& recBoard, int& mezz, int& mz_hyb)
{
  switch (barrel) {
    case 1:
      recBoard = key_barrel1[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel1[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel1[ladder-1][wafer-1][hybrid-1][2];
      break;
    case 2:
      recBoard = key_barrel2[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel2[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel2[ladder-1][wafer-1][hybrid-1][2];
      break;
    case 3:
      recBoard = key_barrel3[ladder-1][wafer-1][hybrid-1][0];
      mezz = key_barrel3[ladder-1][wafer-1][hybrid-1][1];
      mz_hyb = key_barrel3[ladder-1][wafer-1][hybrid-1][2];
      break;
  }
}
