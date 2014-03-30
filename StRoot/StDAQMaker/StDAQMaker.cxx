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
#include "StSCReader.h"
#include "StRtsTable.h"
#include "StRtsReaderMaker.h"

ClassImp(StDAQMaker)

//_____________________________________________________________________________
  StDAQMaker::StDAQMaker(const char *name, const char *inputFile):
    StIOInterFace(name),fEvtHddr(0),fDAQReader(0),fDAQReaderSet(0),fRtsMaker(0)
{
  if (inputFile && inputFile[0]) SetFile(inputFile);
  fRtsMaker = new StRtsReaderMaker; // Create StRtsMaker to assure its correct position
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
  if(!fDAQReader) fDAQReader = new StDAQReader(0,fRtsMaker);
  if (GetDebug()>1) fDAQReader->setVerbose(1);
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

  int iret = fDAQReader->Make();  
  
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
  
  if ( !( fDAQReader->getUnixTime() || fDAQReader->getEventSize() ) ) {
     LOG_ERROR << " The run: " << fDAQReader->getRunNumber() 
               << "/ event:  " << fDAQReader->getEventNumber() 
               <<  ((!fDAQReader->getUnixTime()) ? " timestamp " : " size" )
               << " is not correct "
               << endm;
     fDAQReader->printEventInfo();
     fEvtHddr->Print();
     iret = kStEOF;
     return iret;
  }

  StRtsTable *daqTofTable = GetNextLegacy();
  if (daqTofTable) {
     sc_t *c = (sc_t*)*DaqDta()->begin();
     StSCReader scReader(c,fDAQReader->getUnixTime());
     AddData(scReader.getSCTable());
  }
  int adcOnly = IAttr("adcOnly");
  if (GetDebug()<=1 && !adcOnly) return 0;


  StTPCReader *myTPCReader = fDAQReader->getTPCReader();
  
  int nPads=0;
  if ( myTPCReader ) {
    for (int sector =1; sector <=12; sector++)
      {
	unsigned char* padList;
	for (int padRow=1; padRow<=45; padRow++)
	  {
	    int npad = myTPCReader->getPadList(sector,padRow,padList);
	    if (npad <0) break;
	    if (npad==0) continue;
	    nPads+=npad;
	    if (GetDebug()>1)
	      LOG_INFO << Form("Sector=%2d PadRow=%2d nPads=%3d\n",sector,padRow,npad) << endm;
	  }
      }
  }
  if (nPads==0 && adcOnly) return kStSKIP;
  return 0;
}
//_____________________________________________________________________________
void  StDAQMaker::Clear(const char*)
{
  if (fDAQReader) fDAQReader->clear();
  StMaker::Clear();
}



