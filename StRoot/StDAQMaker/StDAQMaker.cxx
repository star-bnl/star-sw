//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StDAQMaker class for Makers                                          //
// Author Victor Perev							//
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include "St_ObjectSet.h"
#include "StDAQMaker.h"
#include "StDAQReader.h"

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
delete fEvtHddr;
}
//_____________________________________________________________________________
Int_t StDAQMaker::Init()
{
  fDAQReaderSet = new St_ObjectSet("StDAQReader");
  AddConst(fDAQReaderSet); SetOutput(fDAQReaderSet);	//Declare for output

  fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
  if (!fEvtHddr) {
    fEvtHddr = new StEvtHddr(m_ConstSet);
    SetOutput(fEvtHddr); //Declare this "EvtHddr" for output
  }
// 		Get run parameters from input file

  return Open();  
}
//_____________________________________________________________________________
Int_t StDAQMaker::Open(const char*)
{
  if (fDAQReader) return 0;
  printf("*** StDAQMaker::Init:  Open Input file %s ***\n",GetFile());
  fDAQReader = new StDAQReader(GetFile());
  fDAQReaderSet->SetObject((TObject*)fDAQReader);
  return 0;
}
//_____________________________________________________________________________
void StDAQMaker::Close(Option_t *)
{
  Clear();
  m_ConstSet->Delete();
  delete fDAQReader; 	fDAQReader	=0;
}
//_____________________________________________________________________________
Int_t StDAQMaker::Skip(int nskip){
  return fDAQReader->skipEvent(nskip);
}
//_____________________________________________________________________________
Int_t StDAQMaker::Make(){

  int iret = fDAQReader->readEvent();  

  if (iret) iret = kStEOF;
  if (iret) return iret;


  fEvtHddr->SetRunNumber  	( fDAQReader->getRunNumber()   	);
  fEvtHddr->SetEventNumber	( fDAQReader->getEventNumber() 	);
  fEvtHddr->SetInputTriggerMask	( fDAQReader->getTrigInputWord());
  fEvtHddr->SetTriggerMask	( fDAQReader->getTrigWord() 	);
  fEvtHddr->SetGMTime		( fDAQReader->getUnixTime()	);
  if (Debug()) fEvtHddr->Print();
  

  if (GetDebug()<=1) return 0;
  StTPCReader *myTPCReader = fDAQReader->getTPCReader();
  for (int sector =1; sector <=12; sector++)
  {
    TPCSequence *SeqData = 0;
    unsigned char* padList;
    for (int padRow=1; padRow<=45; padRow++)
    {
      int npad = myTPCReader->getPadList(sector,padRow,padList);
      if (npad <0) break;
      if (npad==0) continue;
      printf(" Sector=%2d PadRow=%2d nPads=%3d\n",sector,padRow,npad);
    }
  }
  return 0;
}
