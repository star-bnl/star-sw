/***************************************************************************
 *
 * $Id: StSvtDaqMaker.cxx,v 1.1 2000/06/13 20:42:05 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Library Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqMaker.cxx,v $
 * Revision 1.1  2000/06/13 20:42:05  caines
 * StRoot/StSvtDaqMaker
 *
 *
 **************************************************************************/

#include "StChain.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "StDAQMaker/StSVTReader.h"
#include "StSvtDaqData.hh"
#include "StSvtHybridDaqData.hh"
#include "StSvtDaqMaker.h"

ClassImp(StSvtDaqMaker)

//_____________________________________________________________________________
  StSvtDaqMaker::StSvtDaqMaker(const char *name, char* config, char* data):StMaker(name)
{
  fConfig = config;
  fDataType = data;

  fData = NULL;
  fSvtData = NULL;

  fSvtSet = NULL;
  fHybridSet = NULL;

  daqReader = NULL;
  svtReader = NULL;
}

//_____________________________________________________________________________
StSvtDaqMaker::~StSvtDaqMaker()
{
  delete fData;     
  delete fSvtData;    

  //delete daqReader;   
  //delete svtReader;   
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Init()
{
  //cout << "StSvtDaqMaker::Init" << endl;

  daqReader = NULL;

  St_DataSet *dataSet;
  dataSet = GetDataSet("StDAQReader");
  assert(dataSet);
  daqReader = (StDAQReader*)(dataSet->GetObject());
  assert(daqReader);

  SetSvtData();

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetSvtData()
{
  fSvtSet = new TObjectSet("StSvtData");
  AddConst(fSvtSet);  
  SetOutput(fSvtSet); //Declare for output

  if (!fSvtData) {
    fSvtData = new StSvtDaqData(fConfig);
    fSvtSet->SetObject((TObject*)fSvtData);
  }

  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetHybridData()
{
  fHybridSet = new TObjectSet("StHybridData");
  AddConst(fHybridSet);  
  SetOutput(fHybridSet); //Declare for output

  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtDaqMaker::Make()
{
  //cout << "StSvtDaqMaker::Make" << endl;

  GetSvtData();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetSvtData()
{
  svtReader = daqReader->getSVTReader();
  assert(svtReader);

  if (!fSvtData) {
    fSvtData = new StSvtDaqData(fConfig);
    fSvtSet->SetObject((TObject*)fSvtData);
  }

  fSvtData->setData(svtReader, fDataType);

  fSvtData->setRunNumber(daqReader->getRunNumber()); 
  fSvtData->setEventNumber(daqReader->getEventNumber()); 
  fSvtData->setTrigWord(daqReader->getTrigWord()); 
  fSvtData->setSCAZero(svtReader->getSCAZero()); 
  //  fSvtData->setTimeZero(svtReader->getTimeZero()); 

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetHybridData(int barrel, int ladder, int wafer, int hybrid)
{
  //cout << "StSvtDaqMaker::Make" << endl;

  int status = 0;

  if (!fData) {
    fData = new StSvtHybridDaqData(barrel, ladder, wafer, hybrid);
    fHybridSet->SetObject((TObject*)fData);
  }

  svtReader = daqReader->getSVTReader();
  assert(svtReader);
  
  if (fSvtData->getHybridIndex(barrel,ladder,wafer,hybrid) < 0)
    delete fData;
  else
    status = fData->setHybridData(svtReader, fDataType);
  
  return kStOK;
}

//_____________________________________________________________________________
void StSvtDaqMaker::PrintEventInfo()
{
  if (daqReader)
    daqReader->printEventInfo();
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Clear()
{
  //cout << "StSvtDaqMaker::Clear" << endl;

  if (fSvtData) {
    fSvtData->Delete();
    fData = NULL;
  }

  if (fHybridSet) {
    fHybridSet->Delete();
    fData = NULL;
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Reset()
{
  //cout << "StSvtDaqMaker::Reset" << endl;

  Clear();

  if (fSvtSet) {
    fSvtSet->Delete();
    fSvtData = NULL;
  }

  fSvtSet = 0;
  fHybridSet = 0;
  m_ConstSet->Delete();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Finish()
{
  //cout << "StSvtDaqMaker::Finish" << endl;

  Reset();

  return kStOK;
}

//_____________________________________________________________________________
void StSvtDaqMaker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: StSvtDaqMaker.cxx,v 1.1 2000/06/13 20:42:05 caines Exp $\n");
  printf("**************************************************************\n");
  if (gStChain->Debug()) StMaker::PrintInfo();
}

