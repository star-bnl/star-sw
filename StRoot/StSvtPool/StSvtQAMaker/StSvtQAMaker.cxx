/***************************************************************************
 *
 * $Id: StSvtQAMaker.cxx,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT QA Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtQAMaker.cxx,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 *
 **************************************************************************/

#include "StSvtQAMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"
#include "St_ObjectSet.h"
#include "StDAQMaker/StDAQReader.h"
#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtMonitor.hh"

#include "TOrdCollection.h"

St_DataSet *dataSet;
St_DataSet *dataSetPed;

// Here one decides which histograms to be included in QA histos. 
// See StSvtMonitor for complete list of histos
int mListHist[] = {101,102,103,104,105,106};
int maxHist = sizeof(mListHist)/sizeof(int);

ClassImp(StSvtQAMaker)

//_____________________________________________________________________________
StSvtQAMaker::StSvtQAMaker(const char *name, StSvtMonitor* monitor, char* data):
  StMaker(name)
{
  if (monitor)
    mMonitor = monitor;
  else
    mMonitor = NULL;

  mDataType = data;
}

//_____________________________________________________________________________
StSvtQAMaker::~StSvtQAMaker()
{
  //delete mSvtData;
  //delete mSvtPed;
  delete mMonitor;        
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::Init()
{
  //if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::Init" << endm;
  cout << "StSvtQAMaker::Init" << endl;

  SetSvtData();

  // instantiate monitor (book histograms)
  if (!mMonitor)
    mMonitor = new StSvtMonitor(mSvtData->getConfiguration(),mListHist,maxHist);

  /*
  // Set keys
  mMonitor->setkRaw(kTRUE);
  mMonitor->setkPedSub(kFALSE);
  mMonitor->setkCMNSub(kFALSE);
  mMonitor->setkEventStat(kFALSE);
  mMonitor->setkPedTime(kTRUE);
  mMonitor->setkPedCap(kFALSE);
  mMonitor->setkPed1stOrd(kTRUE);
  mMonitor->setkPed2ndOrd(kFALSE);
  */

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetSvtData()
{
  if ((!strncmp(mDataType, "RAW", strlen("RAW"))) || (!strncmp(mDataType, "ZS", strlen("ZS"))))
    SetSvtRawData();
  else if (!strncmp(mDataType, "ADJ", strlen("ADJ")))
    SetSvtAdjData();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetSvtAdjData()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::Init" << endm;

  dataSet = GetDataSet("StSvtData");
  assert(dataSet);

  mSvtData = (StSvtData*)(dataSet->GetObject());
  assert(mSvtData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetSvtRawData()
{
  dataSet = GetDataSet("StSvtRawData");
  assert(dataSet);

  mSvtRawData = (StSvtData*)(dataSet->GetObject());
  assert(mSvtRawData);
 
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetData()
{
  SetSvtData();

  if ((!strncmp(mDataType, "RAW", strlen("RAW"))) || (!strncmp(mDataType, "ZS", strlen("ZS"))))
    SetRawData();
  else if (!strncmp(mDataType, "ADJ", strlen("ADJ")))
    SetAdjData();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetAdjData()
{
  if (!mMonitor)
    mMonitor = new StSvtMonitor(mSvtData->getConfiguration(),mListHist,maxHist);

  mMonitor->setEvent(mSvtData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetRawData()
{
  if (!mMonitor)
    mMonitor = new StSvtMonitor(mSvtRawData->getConfiguration(),mListHist,maxHist);

  mMonitor->setEvent(mSvtRawData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetPedestal()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::SetPedestal" << endm;

  dataSetPed = GetDataSet("StSvtPedestal");
  assert(dataSetPed);

  mSvtPed = (StSvtHybridCollection*)(dataSetPed->GetObject());
  assert(mSvtPed);

  if (!mMonitor)
    if (mSvtData)
      mMonitor = new StSvtMonitor(mSvtData->getConfiguration(),mListHist,maxHist);
    else {
      gMessMgr->Message("  NO event to set pedestal! Get one!  ","W");
      return -1;
    }

  mMonitor->setPedestal(mSvtPed);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetRMSPedestal()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::SetRMSPedestal" << endm;

  dataSetPed = GetDataSet("StSvtRMSPedestal");
  assert(dataSetPed);

  mSvtRMSPed = (StSvtHybridCollection*)(dataSetPed->GetObject());
  assert(mSvtRMSPed);

  if (!mMonitor)
    if (mSvtData)
      mMonitor = new StSvtMonitor(mSvtData->getConfiguration(),mListHist,maxHist);
    else {
      gMessMgr->Message("  NO event to set pedestal! Get one!  ","W");
      return -1;
    }

  mMonitor->setRMSPedestal(mSvtRMSPed);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::SetPedestal2ndOrd()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::SetPedestal" << endm;

  dataSetPed = GetDataSet("StSvtPedestal2ndOrd");
  assert(dataSetPed);

  mSvtPed = (StSvtHybridCollection*)(dataSetPed->GetObject());
  assert(mSvtPed);

  if (!mMonitor)
    if (mSvtData)
      mMonitor = new StSvtMonitor(mSvtData->getConfiguration(),mListHist,maxHist);
    else {
      gMessMgr->Message("  NO event to set pedestal! Get one!  ","W");
      return -1;
    }

  mMonitor->setPedestal2ndOrd(mSvtPed);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::FillHistograms()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::FillHistograms" << endm;

  int histID;

  for (int barrel = 1;barrel <= mSvtData->getNumberOfBarrels();barrel++) {
    for (int ladder = 1;ladder <= mSvtData->getNumberOfLadders(barrel);ladder++) {

      for (int wafer = 1;wafer <= mSvtData->getNumberOfWafers(barrel);wafer++) {
    	for (int hybrid = 1;hybrid <= mSvtData->getNumberOfHybrids();hybrid++) {
	  
	  // check if the hybrid is part of the SVT
	  if (mSvtData->getHybridIndex(barrel, ladder, wafer, hybrid) < 0) continue;

	  mMonitor->setHybridID(barrel, ladder, wafer, hybrid);

	  // Loop over histograms
	  for (int hist=0;hist<mMonitor->getHistograms()->GetSize();hist++) {
	    histID = mMonitor->getHistID(hist);
	    if (histID>0) {
	      mMonitor->setHistID(histID);
	      mMonitor->fillHist();
	    }
	  }

	} // end of loop on hybrids
      } // end of loop on wafers
    } // end of loop on ladders
  } // end of loop on barrels
  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::Make()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::Make" << endm;

  SetData();

  mMonitor->setStatistics();

  //FillHistograms();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::Finish()
{
  if (Debug()) gMessMgr->Debug() << "StSvtQAMaker::Finish" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSvtQAMaker::Clear(const char*)
{
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSvtQAMaker::Reset()
{  
  return kStOK;
}

//_____________________________________________________________________________
void StSvtQAMaker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: StSvtQAMaker.cxx,v 1.1 2004/02/06 02:30:36 munhoz Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

