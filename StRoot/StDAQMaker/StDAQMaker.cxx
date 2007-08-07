//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDAQMaker class for Makers                                          //
// Author Victor Perev							//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <Stiostream.h>
#include "TObjectSet.h"
#include "StDAQMaker.h"
#include "StDAQReader.h"
#include "StTPCReader.h"

ClassImp(StDAQMaker)

//_____________________________________________________________________________
  StDAQMaker::StDAQMaker(const char *name, const char *inputFile):
    StIOInterFace(name)
{
  fDAQReader 	= 0;
  if (inputFile && inputFile[0]) SetFile(inputFile);
  fEvtHddr = 0;
  fDAQReaderSet = 0;
}
//_____________________________________________________________________________
StDAQMaker::~StDAQMaker()
{
  delete fDAQReader;
  //delete fEvtHddr;	Better small leak than crash
  fDAQReaderSet=0;
}
//_____________________________________________________________________________
Int_t StDAQMaker::Init()
{
  if (fDAQReaderSet) return 0;
  fDAQReaderSet = new TObjectSet("StDAQReader",0,kFALSE);
  AddConst(fDAQReaderSet); SetOutput(fDAQReaderSet);	//Declare for output

  fEvtHddr = GetEvtHddr();
// 		Get run parameters from input file

  return Open();  
}
//_____________________________________________________________________________
Int_t StDAQMaker::Open(const char*)
{
  if (fDAQReader && fDAQReader->isOpened()) return 0;
  LOG_INFO << "Open Input file" << GetFile() << endm;
  if(!fDAQReader) fDAQReader = new StDAQReader();
  fDAQReader->setVerbose(GetDebug());
  fDAQReader->open(GetFile());
  fDAQReaderSet->SetObject((TObject*)fDAQReader,kFALSE);
  return 0;
}
//_____________________________________________________________________________
void StDAQMaker::Close(Option_t *)
{
  Clear();
  fDAQReader->close();
}
//_____________________________________________________________________________
Int_t StDAQMaker::Skip(int nskip){
  return fDAQReader->skipEvent(nskip);
}
//_____________________________________________________________________________
Int_t StDAQMaker::Make(){

  int iret = fDAQReader->readEvent();  

  if (iret==kStErr) return kStErr; // Herb, July 5 2000.  Skip "Token 0" events.
  if (iret) iret = kStEOF;
  if (iret) return iret;


  fEvtHddr->SetRunNumber  	( fDAQReader->getRunNumber()   	);
  fEvtHddr->SetEventNumber	( fDAQReader->getEventNumber() 	);
  fEvtHddr->SetInputTriggerMask	( fDAQReader->getTrigInputWord());
  fEvtHddr->SetTriggerMask	( fDAQReader->getTrigWord() 	);
  fEvtHddr->SetGMTime		( fDAQReader->getUnixTime()	);
  fEvtHddr->SetEventSize	( fDAQReader->getEventSize()	);
  if (Debug()) {
    fDAQReader->printEventInfo();
    fEvtHddr->Print();
  }  

  AddData(fDAQReader->getSCTable());

  if (GetDebug()<=1) return 0;


  StTPCReader *myTPCReader = fDAQReader->getTPCReader();
  for (int sector =1; sector <=12; sector++)
  {
    unsigned char* padList;
    for (int padRow=1; padRow<=45; padRow++)
    {
      int npad = myTPCReader->getPadList(sector,padRow,padList);
      if (npad <0) break;
      if (npad==0) continue;
      LOG_INFO << Form("Sector=%2d PadRow=%2d nPads=%3d\n",sector,padRow,npad) << endm;
    }
  }
  return 0;
}
//_____________________________________________________________________________
void  StDAQMaker::Clear(const char*)
{
  if (fDAQReader) fDAQReader->clear();
  StMaker::Clear();
}



