/***************************************************************************
 *
 * $Id: StDAQReader.cxx,v 1.38 2003/12/24 21:30:45 perev Exp $
 *
 * Author: Victor Perev
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ reader classes
 *
 ***************************************************************************
 *
 * $Log: StDAQReader.cxx,v $
 * Revision 1.38  2003/12/24 21:30:45  perev
 * Cleanup of DAQ
 *
 * Revision 1.37  2003/09/28 01:54:58  jeromel
 * Missing one zero
 *
 * Revision 1.36  2003/07/16 19:58:29  perev
 * Cleanup of StTriggerData2003 at all
 *
 * Revision 1.35  2003/05/15 19:25:47  jeromel
 * Missing Update() ??
 *
 * Revision 1.34  2003/05/14 18:25:08  perev
 * Remove error for No TPC
 *
 * Revision 1.33  2003/04/22 20:12:42  ward
 * So the chain can run when there is no TPC data.
 *
 * Revision 1.32  2003/03/24 18:12:10  ward
 * Full support for EEMC from Herbert Ward.
 *
 * Revision 1.31  2003/02/16 16:02:49  perev
 * new method added
 *
 * Revision 1.30  2002/12/19 22:28:19  perev
 * PMD added
 *
 * Revision 1.29  2002/01/17 21:14:38  perev
 * Akio FPD reader
 *
 * Revision 1.28  2001/07/16 21:38:44  perev
 * EMC added
 *
 * Revision 1.27  2001/07/10 18:13:04  jeromel
 * Changes commited for Frank Geurts (TOF) after approval from Herb Ward
 * on Tue, 10 Jul 2001 11:19:48 and review by Victor.
 * Changes implements TOF DAQ Reader.
 *
 * Revision 1.26  2001/06/19 21:10:26  jeromel
 * Changes for FTPCReader (Janet S.)
 *
 * Revision 1.25  2001/03/20 01:43:14  perev
 * first event skip fixed
 *
 * Revision 1.24  2001/01/03 23:08:35  perev
 * Skip over EOF added
 *
 * Revision 1.23  2000/09/13 17:02:09  perev
 * add delete of L3Reader in close
 *
 * Revision 1.22  2000/08/28 22:19:10  ward
 * Skip corrupted events. StDaqLib/GENERIC/EventReader.cxx & StDAQMaker/StDAQReader.cxx.
 *
 * Revision 1.21  2000/07/13 22:29:52  perev
 * Return kStErr when TPC data is not in event.
 *
 * Revision 1.20  2000/07/08 22:53:39  perev
 * next offset before error return
 *
 * Revision 1.19  2000/07/06 20:39:28  fisyak
 * Herb correction for Token '0' events
 *
 * Revision 1.18  2000/06/30 21:53:40  perev
 * L3 stuff added
 *
 * Revision 1.17  2000/06/27 23:56:47  perev
 * Helen DAQ SVT update
 *
 * Revision 1.16  2000/06/12 15:04:02  perev
 * SVT + cleanup
 *
 * Revision 1.15  2000/05/25 20:01:58  perev
 * EventSize added
 *
 * Revision 1.14  2000/04/07 15:43:18  perev
 * SetVerbose method added
 *
 * Revision 1.13  2000/02/03 23:19:11  fisyak
 * Reduce file map size
 *
 * Revision 1.12  2000/01/24 20:35:37  ward
 * Access trigger data.
 *
 * Revision 1.11  2000/01/24 14:39:27  perev
 * FTPC (HolmMade) is added
 *
 * Revision 1.10  1999/12/15 18:40:59  perev
 * Keep undeleted all DAQ wrapper classes
 *
 * Revision 1.9  1999/09/24 01:22:52  fisyak
 * Reduced Include Path
 *
 * Revision 1.8  1999/09/10 16:35:38  fine
 * The oreder of deleting object has been changed to avoid crash in StDaqLib
 *
 * Revision 1.7  1999/08/19 22:28:40  perev
 * fix Skip & EventNumber
 *
 * Revision 1.6  1999/08/10 00:28:50  fisyak
 * Herb/Victor corrects for errors
 *
 * Revision 1.5  1999/08/06 16:20:58  perev
 * RICHReader added
 *
 * Revision 1.4  1999/08/01 00:14:49  perev
 * leak removed author added
 *
 * Revision 1.3  1999/08/01 00:09:07  perev
 * leak removed author added
 *
 **************************************************************************/
#include "Stypes.h"

//	non standard open,close,read
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "Stypes.h"

#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQReader.h"
#include "StTPCReader.h"
#include "StEMCReader.h"
#include "StEEMCReader.h"
#include "StPMDReader.h"
#include "StFTPCReader.h"
#include "StTRGReader.h"
#include "StSVTReader.h"
//


typedef EventInfo DAQEventInfo;
//_____________________________________________________________________________
StDAQReader::StDAQReader(const char *file)
{
  fFd = -1;
  fVerbose = 0;
  fEventReader	= 0;
  fTPCReader 	= 0;
  fEMCReader 	= 0;
  fEEMCReader 	= 0;
  fPMDReader 	= 0;
  fFTPCReader   = 0;
  fRICHReader 	= 0;
  fTRGReader    = 0;
  fSVTReader 	= 0;
  fL3Reader 	= 0;
  fTOFReader    = 0;
  fFPDReader    = 0;
  fOffset = 0;
  fFile = 0;
  fEventInfo = new DAQEventInfo;
  memset(fEventInfo,0,sizeof(DAQEventInfo));
  assert(sizeof(DAQEventInfo)==sizeof(EventInfo));
  setTPCVersion();
  setFTPCVersion();
  fTrigSummary = new StTrigSummary();

  if(file && file[0]) open(file);
}

//_____________________________________________________________________________
int StDAQReader::open(const char *file)
{
  assert(file);
  if (fFd!=(-1) && fFile && strcmp(file,fFile)==0) return 0;
  close();
  fFile = new char[strlen(file)+1];  strcpy(fFile,file);

  fFd = ::open(fFile,O_RDONLY);
   if (fFd==-1) { 
     printf("<StDAQReader::open>  %s %s ",fFile, strerror( errno ) );
     return kStErr;
  }
  fOffset =0;   
  return 0;  
}
//_____________________________________________________________________________
int StDAQReader::close()
{
  delete [] fFile; fFile=0;
  if (fFd != (-1)) ::close(fFd);
  fFd = -1;
  delete fEventReader;	fEventReader 	= 0;  

  if(fTPCReader) 	fTPCReader ->close();  
  if(fEMCReader) 	fEMCReader ->close();  
  if(fEEMCReader) 	fEEMCReader ->close();  
  if(fPMDReader) 	fPMDReader ->close();  
  if(fSVTReader) 	fSVTReader ->close();  
//if (fRICHReader) 	fRICHReader->close();  
  if(fFTPCReader)       fFTPCReader->close();
  if(fTRGReader)        fTRGReader ->close();
  delete fL3Reader; 	fL3Reader  = 0;  
  fOffset = -1;
  return 0;
}
//_____________________________________________________________________________
StDAQReader::~StDAQReader()
{
  close();
}
//_____________________________________________________________________________
int StDAQReader::readEvent()
{  
  delete fEventReader;	fEventReader=0;
  delete fRICHReader; 	fRICHReader = 0;
  delete fL3Reader; 	fL3Reader   = 0;
  delete fTOFReader;    fTOFReader  = 0;
  delete fFPDReader;    fFPDReader  = 0;
  if (fOffset == -1) return kStEOF;
  fEventReader = new EventReader();
  fEventReader->setVerbose(fVerbose);
  //  fEventReader->InitEventReader(fFd, fOffset, 0);
  fEventReader->InitEventReader(fFd, fOffset);
  int oldOffset = fOffset;
  fOffset = fEventReader->NextEventOffset();
  if(fEventReader->eventIsCorrupted(fFd,oldOffset)) return kStErr; // Herb, Aug 28 2000
  if(fEventReader->errorNo()) return kStErr;  
  *fEventInfo = fEventReader->getEventInfo();
  if(fEventInfo->Token==0) return kStErr;  // Herb, July 5 2000

  if (fTPCReader&&TPCPresent  ())       fTPCReader ->Update();
  if (fFTPCReader&&FTPCPresent())	fFTPCReader->Update();  
  if (fTRGReader&&TRGPresent  ())       fTRGReader ->Update();
  if (fSVTReader&&SVTPresent  ()) 	fSVTReader ->Update();
  if (fEMCReader&&EMCPresent  ()) 	fEMCReader ->Update();
  if (fEEMCReader&&EMCPresent ()) 	fEEMCReader->Update();
  if (fPMDReader&&PMDPresent  ()) 	fPMDReader ->Update();

//	Trigger Summary, code provided by Herb
  Bank_DATAP *datap = (Bank_DATAP*)(fEventReader->getDATAP());
   assert(datap->header.BankType[0]=='D'); // first letter of DATAP
   { int i;
     for(i=0;i<2;i++) fTrigSummary->L1summary[i]=datap->reserved[i+0];
     for(i=0;i<2;i++) fTrigSummary->L2summary[i]=datap->reserved[i+2];
     for(i=0;i<4;i++) fTrigSummary->L3summary[i]=datap->reserved[i+4];
   }

  return 0;
}
//_____________________________________________________________________________
int StDAQReader::skipEvent(int nskip)
{
  for (int isk=0; nskip; nskip--,isk++) 
  {
    delete fEventReader;
    if (fOffset == -1) {
      printf("<Warning: StDAQReader::skipEvent> EOF after record %d\n",isk);
      break;}  
      
    fEventReader = new EventReader();
    //    fEventReader->InitEventReader(fFd, fOffset, 0);
    fEventReader->InitEventReader(fFd, fOffset);
    if(fEventReader->errorNo()) {
      printf("<Warning: StDAQReader::skipEvent> ReadError on record %d\n",isk);
      fOffset = -1; break;}  
    fOffset = fEventReader->NextEventOffset();
  }
  return  nskip;
}


//_____________________________________________________________________________
void StDAQReader::setTPCVersion(const char* vers)
{strcpy(fTPCVersion,vers);} 
//_____________________________________________________________________________
void StDAQReader::setFTPCVersion(const char* vers)
{strcpy(fFTPCVersion,vers);} 

//_____________________________________________________________________________
  int StDAQReader::getRunNumber()   const { return fEventReader->runno();}
//_____________________________________________________________________________
  int StDAQReader::getEventNumber() const {return fEventInfo->EventSeqNo;}
//_____________________________________________________________________________
  unsigned int StDAQReader::getUnixTime() const {return fEventInfo->UnixTime;}
//_____________________________________________________________________________
  unsigned int StDAQReader::getTrigWord() const {return fEventInfo->TrigWord;}
//_____________________________________________________________________________
  unsigned int StDAQReader::getTrigInputWord() const {return fEventInfo->TrigInputWord;}
//_____________________________________________________________________________
   int StDAQReader::TPCPresent()  const {return  fEventInfo->TPCPresent;}
//_____________________________________________________________________________
   int StDAQReader::SVTPresent()  const {return  fEventInfo->SVTPresent;}
//_____________________________________________________________________________
   int StDAQReader::TOFPresent()  const {return  fEventInfo->TOFPresent;}
//_____________________________________________________________________________
   int StDAQReader::FPDPresent()  const {return  fEventInfo->FPDPresent;}
//_____________________________________________________________________________
   int StDAQReader::EMCPresent()  const {return  fEventInfo->EMCPresent;}
//_____________________________________________________________________________
   int StDAQReader::PMDPresent()  const {return  fEventInfo->PMDPresent;}
//_____________________________________________________________________________
   int StDAQReader::SMDPresent()  const {return  fEventInfo->BSMDPresent|fEventInfo->ESMDPresent;}
//_____________________________________________________________________________
   int StDAQReader::FTPCPresent() const {return  fEventInfo->FTPCPresent;}
//_____________________________________________________________________________
   int StDAQReader::RICHPresent() const {return  fEventInfo->RICHPresent;}
//_____________________________________________________________________________
   int StDAQReader::TRGPresent()  const{return   fEventInfo->TRGPresent;}
//_____________________________________________________________________________
   int StDAQReader::L3Present()   const {return  fEventInfo->L3Present;}
//_____________________________________________________________________________
   int StDAQReader::getEventSize()const {return  fEventInfo->EventLength;}
//_____________________________________________________________________________
StTPCReader *StDAQReader::getTPCReader() 
{
  int updateStatus;
  if(!TPCPresent()) return 0;
  if (!fTPCReader) {
    fTPCReader = new StTPCReader(this);
    updateStatus=fTPCReader->Update();
    if(updateStatus) { delete fTPCReader; fTPCReader=0; return 0; } // No TPC data.  Herb Ward, Apr 22 2003.
  }
  return fTPCReader;
}
//_____________________________________________________________________________
StEEMCReader *StDAQReader::getEEMCReader()
{
  if (!fEEMCReader) {
    fEEMCReader = new StEEMCReader(this);
  }
  return fEEMCReader;
} 
//_____________________________________________________________________________
StEMCReader *StDAQReader::getEMCReader()
{
  if (!EMCPresent()) return 0;
  if (!fEMCReader) {
    fEMCReader = new StEMCReader(this);
  }
  return fEMCReader;
} 
//_____________________________________________________________________________
//_____________________________________________________________________________
StPMDReader *StDAQReader::getPMDReader()
{
  if (!PMDPresent()) return 0;
  if (!fPMDReader) {
    fPMDReader = new StPMDReader(this);
//     fPMDReader->Update(); 
  }
  return fPMDReader;
} 
//_____________________________________________________________________________
StRICHReader *StDAQReader::getRICHReader() 
{
  if (!RICHPresent()) return 0;
  if (!fRICHReader) {
    fRICHReader = ::getRICHReader(fEventReader);
  }
  return fRICHReader;
}
//_____________________________________________________________________________
StL3Reader *StDAQReader::getL3Reader() 
{
  if (!L3Present()) return 0;
  if (!fL3Reader) {
    fL3Reader = ::getL3Reader(fEventReader);
  }
  return fL3Reader;
}
//_____________________________________________________________________________
StFTPCReader *StDAQReader::getFTPCReader() 
{
  if (!FTPCPresent()) return 0;
  if (!fFTPCReader) {
    fFTPCReader = new StFTPCReader(this);
    fFTPCReader->Update();
  }
  return fFTPCReader;
}
//_____________________________________________________________________________
StTOFReader *StDAQReader::getTOFReader()
{
  if (!TOFPresent()) return 0;
  if (!fTOFReader) {
    fTOFReader = ::getTOFReader(fEventReader);
  }
  return fTOFReader;
}
//_____________________________________________________________________________
StFPDReader *StDAQReader::getFPDReader()
{
  if (!FPDPresent()) return 0;
  if (!fFPDReader) {
    fFPDReader = ::getFPDReader(fEventReader);
  }
  return fFPDReader;
}
//_____________________________________________________________________________
StTRGReader *StDAQReader::getTRGReader()
{
  if (!TRGPresent()) return 0;
  if (!fTRGReader) {
    fTRGReader = new StTRGReader(this);
    fTRGReader->Update();
  }
  return fTRGReader;
}
//_____________________________________________________________________________
StSVTReader *StDAQReader::getSVTReader()
{
  if (!SVTPresent()) return 0;
  if (!fSVTReader) {
    fSVTReader = new StSVTReader(this);
    fSVTReader->Update();
  }
  return fSVTReader;
}
//_____________________________________________________________________________
void StDAQReader::printEventInfo()
{fEventReader->printEventInfo();}
