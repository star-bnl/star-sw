/***************************************************************************
 *
 * $Id: StSvtDaqMaker.cxx,v 1.4 2000/07/04 02:36:53 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Library Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqMaker.cxx,v $
 * Revision 1.4  2000/07/04 02:36:53  perev
 * formal corrections, gStChain removed
 *
 * Revision 1.3  2000/07/01 20:14:06  caines
 * Removed unneccesary delete that was crashing code
 *
 * Revision 1.2  2000/06/25 20:40:16  caines
 * Add debugging statements and protection against SVT not being there
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
#include "StMessMgr.h"

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
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Init()
{
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Init" << endm;

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
   if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Make" << endm;

  GetSvtData();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetSvtData()
{

  if(  !daqReader->SVTPresent ()){
    gMessMgr->Error() << "SVT -No SVT Present but trying to read it" << endm;
    return kStErr;
  }
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
  for( int i=1; i<=N_SECTORS; i++){
    fSvtData->setTimeZero(128*40-(32*105)-(12*105)+svtReader->getTimeZero(),i); 
  }
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetHybridData(int barrel, int ladder, int wafer, int hybrid)
{
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Make" << endm;

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
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Clear" << endm;

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
  if (Debug()) gMessMgr->Debug()<< "StSvtDaqMaker::Reset" << endm;

  //Clear();

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
  if (Debug()) gMessMgr->Debug()<< "StSvtDaqMaker::Finish" << endm;

  Reset();

  return kStOK;
}

//_____________________________________________________________________________
void StSvtDaqMaker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: StSvtDaqMaker.cxx,v 1.4 2000/07/04 02:36:53 perev Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

