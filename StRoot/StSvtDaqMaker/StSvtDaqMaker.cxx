/***************************************************************************
 *
 * $Id: StSvtDaqMaker.cxx,v 1.12 2002/02/27 01:37:20 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Library Maker
 *
 ***************************************************************************
 *
 * $Log: StSvtDaqMaker.cxx,v $
 * Revision 1.12  2002/02/27 01:37:20  munhoz
 * move GetDataSet(StDAQReader) from Init() to Make()
 *
 * Revision 1.11  2002/02/15 02:39:28  munhoz
 * switching from .const to .data
 *
 * Revision 1.10  2001/10/24 16:49:42  munhoz
 * adding capability to retrieve t0 and first SCA
 *
 * Revision 1.9  2001/09/16 22:08:44  caines
 * Add extra checks for when SVT isnt in every event
 *
 * Revision 1.8  2001/07/11 23:29:47  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 * Revision 1.6  2000/08/23 22:29:08  munhoz
 * add time to StSvtData object
 *
 * Revision 1.5  2000/08/04 21:03:51  perev
 * Leaks + Clear() cleanup
 *
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
#include "StSvtDaqPed.hh"
#include "StSvtHybridDaqData.hh"
#include "StSvtDaqMaker.h"
#include "StMessMgr.h"
#include "StSvtClassLibrary/StSvtConfig.hh"

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

  /*
  St_DataSet *dataSet;
  dataSet = GetDataSet("StDAQReader");
  assert(dataSet);
  daqReader = (StDAQReader*)(dataSet->GetObject());
  assert(daqReader);
  */

  SetSvtData();

  return StMaker::Init();
}

/*
//_____________________________________________________________________________
Int_t StSvtDaqMaker::InitRun(int runnumber)
{
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::InitRun" << endm;

  SetSvtData();

  return 0;
}
*/

//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetSvtData()
{
  fSvtSet = new TObjectSet("StSvtRawData");
  AddData(fSvtSet);  

  St_DataSet *dataSet = NULL;
  dataSet = GetDataSet("StSvtConfig");


  if (dataSet)
    fSvtData = new StSvtDaqData((StSvtConfig*)(dataSet->GetObject()));
  else
    fSvtData = new StSvtDaqData(fConfig);

  fSvtSet->SetObject((TObject*)fSvtData);

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetSvtPed()
{
  St_DataSet *dataSet;
  dataSet = (TObjectSet*)GetDataSet("StSvtPedestal");

  if (!dataSet) {
    fPedSet = new TObjectSet("StSvtPedestal");
    AddConst(fPedSet);

    St_DataSet *dataSet2;
    dataSet2 = GetDataSet("StSvtConfig");
    
    if (!fSvtPed) {
      if (dataSet2)
	fSvtPed = new StSvtDaqPed((StSvtConfig*)(dataSet->GetObject()));
      else
	fSvtPed = new StSvtDaqPed(fConfig);
      fPedSet->SetObject((TObject*)fSvtPed);
      //assert(fSvtPed);
    }
  }
  else {
    fSvtPed = (StSvtDaqPed*)(dataSet->GetObject());
    assert(fSvtPed);
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetSvtRMSPed()
{
  St_DataSet *dataSet;
  dataSet = (TObjectSet*)GetDataSet("StSvtRMSPedestal");

  if (!dataSet) {
    fRMSPedSet = new TObjectSet("StSvtRMSPedestal");
    AddConst(fRMSPedSet);

    St_DataSet *dataSet2;
    dataSet2 = GetDataSet("StSvtConfig");
    
    if (!fSvtRMSPed) {
      if (dataSet2)
	fSvtRMSPed = new StSvtDaqPed((StSvtConfig*)(dataSet->GetObject()));
      else
	fSvtRMSPed = new StSvtDaqPed(fConfig);
      fRMSPedSet->SetObject((TObject*)fSvtRMSPed);
      //assert(fSvtPed);
    }
  }
  else {
    fSvtRMSPed = (StSvtDaqPed*)(dataSet->GetObject());
    assert(fSvtRMSPed);
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::SetHybridData()
{
  fHybridSet = new TObjectSet("StHybridRawData");
  AddData(fHybridSet);  

  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtDaqMaker::Make()
{
   if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Make" << endm;

  St_DataSet *dataSet;
  dataSet = GetDataSet("StDAQReader");
  assert(dataSet);
  daqReader = (StDAQReader*)(dataSet->GetObject());
  assert(daqReader);

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

  if( !fSvtSet) SetSvtData();

  fSvtData->setData(svtReader, fDataType);

  fSvtData->setRunNumber(daqReader->getRunNumber()); 
  fSvtData->setEventNumber(daqReader->getEventNumber()); 
  fSvtData->setTrigWord(daqReader->getTrigWord()); 
  fSvtData->setUnixTime(daqReader->getUnixTime());

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetSvtPed()
{

  if(  !daqReader->SVTPresent ()){
    gMessMgr->Error() << "SVT -No SVT Present but trying to read it" << endm;
    if( fSvtPed) Reset();
    return kStErr;
  }
  svtReader = daqReader->getSVTReader();
  assert(svtReader);

  if (!fSvtPed) {
    fSvtPed = new StSvtDaqPed(fConfig);
    fPedSet->SetObject((TObject*)fSvtPed);
  }

  fSvtPed->setPed(svtReader);

  fSvtPed->setRunNumber(daqReader->getRunNumber()); 

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::GetSvtRMSPed()
{

  if(  !daqReader->SVTPresent ()){
    gMessMgr->Error() << "SVT -No SVT Present but trying to read it" << endm;
    if( fSvtRMSPed) Reset();
    return kStErr;
  }
  svtReader = daqReader->getSVTReader();
  assert(svtReader);

  if (!fSvtRMSPed) {
    fSvtRMSPed = new StSvtDaqPed(fConfig);
    fRMSPedSet->SetObject((TObject*)fSvtRMSPed);
  }

  fSvtRMSPed->setPed(svtReader,"RMS");

  fSvtRMSPed->setRunNumber(daqReader->getRunNumber()); 

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
Int_t StSvtDaqMaker::GetUnixTime()
{
  if (daqReader)
    return daqReader->getUnixTime();
  else
    return 0;
}

//_____________________________________________________________________________
void StSvtDaqMaker::PrintEventInfo()
{
  if (daqReader)
    daqReader->printEventInfo();
}

//_____________________________________________________________________________
void StSvtDaqMaker::Clear(const char*)
{
  if (Debug()) gMessMgr->Debug() << "StSvtDaqMaker::Clear" << endm;

  fSvtData = NULL;
  fHybridSet = NULL;
  fSvtPed = NULL;
  fSvtSet = NULL;

  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Reset()
{
  if (Debug()) gMessMgr->Debug()<< "StSvtDaqMaker::Reset" << endm;
  /*
  fSvtData = NULL;
  fHybridSet = NULL;
  fSvtPed = NULL;
  fSvtSet = NULL;
  m_ConstSet->Delete();
  */
  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtDaqMaker::Finish()
{
  if (Debug()) gMessMgr->Debug()<< "StSvtDaqMaker::Finish" << endm;

  return kStOK;
}

//_____________________________________________________________________________
void StSvtDaqMaker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: StSvtDaqMaker.cxx,v 1.12 2002/02/27 01:37:20 munhoz Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
void StSvtDaqMaker::UpdateReader()
{
  if (svtReader)
    svtReader->Update();
}
