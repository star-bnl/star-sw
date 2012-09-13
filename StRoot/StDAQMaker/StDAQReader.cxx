/***************************************************************************
 *
 * $Id: StDAQReader.cxx,v 1.88 2012/09/13 20:01:49 fisyak Exp $
 *
 * Author: Victor Perev
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ reader classes
 *
 ***************************************************************************
 *
 * $Log: StDAQReader.cxx,v $
 * Revision 1.88  2012/09/13 20:01:49  fisyak
 * Clean up, use Jeff's skip_then_get
 *
 * Revision 1.87  2010/01/25 17:17:10  fine
 * Remove the redundant RecordSize method
 *
 * Revision 1.86  2010/01/15 21:26:50  fine
 * RT #1816. Eliminate the side effect from RT 1803 fix
 *
 * Revision 1.85  2010/01/06 20:42:26  fine
 * Fix type EventNumber shoould be RunNumber . Thanks Akio
 *
 * Revision 1.84  2010/01/06 20:09:38  fine
 * RT #1794. Add EventNumber method to the StStreamFile interface RT # 1794
 *
 * Revision 1.83  2009/11/02 21:42:52  fine
 * allow the Akio DAT file to be read in 1999-2008 format
 *
 * Revision 1.82  2009/10/13 19:32:44  fine
 * Re-Activate DAQ reader
 *
 * Revision 1.81  2009/10/13 15:51:48  fine
 * Activate the new DAT file format
 *
 * Revision 1.80  2009/10/07 23:21:56  fine
 * Move the StRtsReaderMaker instantiation fro  StDAQReader to StDAQMaker to make sure it is located just after StDAQMaker in chain
 *
 * Revision 1.79  2009/10/07 00:52:31  fine
 * Move daqReader instantiation from StDAQMaker to StDAQReader to switch between input files properly
 *
 * Revision 1.78  2009/08/08 18:04:03  fine
 * Add Event num / Run num to the error message
 *
 * Revision 1.77  2009/04/06 18:22:33  fine
 * remove the redundant methods and fix L1/L2/L3 summary
 *
 * Revision 1.76  2009/03/27 23:07:33  fine
 * Pick the SC data via the new DAQ READER
 *
 * Revision 1.75  2009/01/23 00:09:54  fine
 * restore the missed EventInfo. Should fix bug 1376, 1377
 *
 * Revision 1.74  2009/01/14 18:20:49  fine
 * Remove the redundant devReader type
 *
 * Revision 1.73  2009/01/14 17:04:05  fine
 * Fix bug 1357
 *
 * Revision 1.72  2009/01/08 23:58:07  fine
 * Pick the token info from the new reader
 *
 * Revision 1.71  2009/01/08 23:41:25  fine
 * Fill the Event Header from the new daqReader if available
 *
 * Revision 1.70  2009/01/08 22:14:46  fine
 * teach EventReader tp provide the new daqReader pointer
 *
 * Revision 1.69  2009/01/05 17:55:36  fine
 * Fix datap issue with the newest DAQ files
 *
 * Revision 1.68  2008/11/26 18:01:29  fine
 * prepare StRtsReaderMaker for DAQ_READER transition
 *
 * Revision 1.67  2008/11/25 21:42:53  fine
 * preparing  DAQ maker for DAQ_READER
 *
 * Revision 1.66  2008/10/01 18:12:19  fine
 * Adjust the code to satisfy the new evpReader interface
 *
 * Revision 1.65  2008/06/09 16:11:34  fine
 * restore the DATAP summary information if needed. Thanx Jeff
 *
 * Revision 1.64  2008/03/11 15:10:47  fine
 * adjust evpReaderClass header file
 *
 * Revision 1.63  2008/03/06 01:32:51  fine
 * Fix skip event problem
 *
 * Revision 1.62  2008/02/08 15:38:50  fine
 * Add CPP flag to distinguish the old and new EVP_READER at the compilation time. Thankx Lidia
 *
 * Revision 1.61  2008/02/07 22:06:35  fine
 * return the correct run number with the new EVP_READER. Tnx Fisyak
 *
 * Revision 1.60  2008/02/01 15:00:31  fine
 * Fix the compilation problem
 *
 * Revision 1.59  2008/02/01 01:23:24  fine
 * Add the message
 *
 * Revision 1.58  2008/02/01 01:20:57  fine
 * :fix the buffer size for DATAP
 *
 * Revision 1.57  2008/01/22 21:13:01  fine
 * move the dependency from the class desclaration to class implematation to allo for the new EVP_READER
 *
 * Revision 1.56  2008/01/10 01:27:56  fine
 * set the correct buffer size
 *
 * Revision 1.55  2008/01/06 02:58:32  fine
 * clean up
 *
 * Revision 1.54  2008/01/06 02:55:21  fine
 * treat new DAQ  EOF issue properly
 *
 * Revision 1.53  2008/01/06 01:48:01  fine
 * fix missuse of the tot_bytes member use bytes instead
 *
 * Revision 1.52  2007/12/24 05:19:58  fine
 * Introduce the shadow copy of the evp buffer for the new EVP_READER
 *
 * Revision 1.51  2007/12/23 03:04:26  fine
 * Add the debug print puts to traceDAQ file problem
 *
 * Revision 1.50  2007/12/22 01:14:58  fine
 * version compatible with new/old DAQ readers
 *
 * Revision 1.49  2007/08/07 19:44:10  perev
 * Gene scalers added
 *
 * Revision 1.48  2007/05/29 22:12:18  fine
 * Introduce logger-based output
 *
 * Revision 1.47  2007/04/24 17:10:57  akio
 * correct byte swapping issue
 *
 * Revision 1.46  2004/11/11 19:58:14  jeromel
 * Added thereIsTriggerData() check and logic for return kStErr (BT 478)
 *
 * Revision 1.45  2004/09/10 22:07:44  perev
 * more defence agains corrupted DAQ data
 *
 * Revision 1.44  2004/08/07 02:39:00  perev
 * Traditional Clear added
 *
 * Revision 1.43  2004/03/04 21:51:27  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.42  2004/02/18 20:17:44  ward
 * Access SSD data in makers.
 *
 * Revision 1.41  2004/02/04 01:39:43  jeromel
 * Minor change (forgot a \n)
 *
 * Revision 1.40  2004/02/03 03:25:06  jeromel
 * Added counter for ZeroToken events
 *
 * Revision 1.39  2004/02/02 20:21:42  ward
 * Passing Token 0 events, on request of J. Lauret.
 *
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
#include "StMessMgr.h"
#include "StDAQReader.h"

#include "StDaqLib/GENERIC/EventReader.hh"
#   include "RTS/src/DAQ_READER/daqReader.h"

#include "StDaqLib/RICH/RICH_Reader.hh"
#include "StDaqLib/L3/L3_Reader.hh"
#include "StDaqLib/TOF/TOF_Reader.hh"
#include "StDaqLib/FPD/FPD_Reader.hh"

#include "StTPCReader.h"
#include "StEMCReader.h"
#include "StSSDReader.h"
#include "StEEMCReader.h"
#include "StPMDReader.h"
#include "StFTPCReader.h"
#include "StTRGReader.h"
#include "StSVTReader.h"
#include "StMessMgr.h" 
#include "TString.h" 
#include "StRtsReaderMaker.h"
#include "StStreamFileFactory.h"
#include "StStreamFile.h"

//


typedef EventInfo DAQEventInfo;
//_____________________________________________________________________________
StDAQReader::StDAQReader(const char *file, StRtsReaderMaker* rtsMaker) :
 fRtsMaker(rtsMaker)
 {
  fFd = -1;
  fVerbose = 0;
  fEventReader	= 0;
  fTPCReader 	= 0;
  fEMCReader 	= 0;
  fSSDReader 	= 0;
  fEEMCReader 	= 0;
  fPMDReader 	= 0;
  fFTPCReader   = 0;
  fRICHReader 	= 0;
  fTRGReader    = 0;
  fSVTReader 	= 0;
  fL3Reader 	= 0;
  fTOFReader    = 0;
  fFPDReader    = 0;
  m_ZeroTokens  = 0;
  fOffset = 0;
  fFile = 0;
  fEventInfo = new DAQEventInfo;
  memset(fEventInfo,0,sizeof(DAQEventInfo));
  assert(sizeof(DAQEventInfo)==sizeof(EventInfo));
  setTPCVersion();
  setFTPCVersion();
  fTrigSummary = new StTrigSummary();
  fDaqFileReader = 0;
  fDatFileReader = 0;
  fDATAP = 0;
  if(file && file[0]) open(file);
}

//_____________________________________________________________________________
int StDAQReader::open(const char *file)
{
  assert(file);
  if (!fRtsMaker) fRtsMaker = new StRtsReaderMaker;
  if (StStreamFileFactory::Factory()) {
     // check the file extension.
     const char *dat_ext= ".dat";
     LOG_INFO << " StDAQReader::open(const char *file):  " << file  << "  - "
              << (const char*)&file[strlen(file)-sizeof(dat_ext)] 
              << "  len=" << strlen(file) 
              << " size=" << sizeof(dat_ext) 
              << fDatFileReader << endm;
     if (TString(dat_ext) ==  (const char*)&file[strlen(file)-sizeof(dat_ext)] && !fDatFileReader) {
        fDatFileReader = StStreamFileFactory::Factory()->Create();
        fRtsMaker->SetDatReader(fDatFileReader);
     }
  }
  if (fDatFileReader) {
     fDatFileReader->open(file);
  } else {
     if (fDaqFileReader) close();
     fDaqFileReader = new daqReader((char *)file);
     fRtsMaker->SetDaqReader(fDaqFileReader);
  }
  LOG_INFO << "StDAQReader::open the DAQ " <<  file << " via "
        << (fDatFileReader ? "DAT reader ->" : "" ) << fDatFileReader
        << (fDaqFileReader ? "daqReader ->" : "") << fDaqFileReader 
        << endm;
  fOffset =0;
  return 0;  
}
//_____________________________________________________________________________
void StDAQReader::clear()
{
  delete fEventReader;	fEventReader 	= 0;
}
//_____________________________________________________________________________
int StDAQReader::close()
{
  if (fDaqFileReader ) {
     delete fDaqFileReader; 
     fDaqFileReader = 0;
  } else if (fDatFileReader) {
     fDatFileReader->close();
  }
  if (fRtsMaker) {
     fRtsMaker->SetDaqReader(0);
  }
  delete fEventReader;	fEventReader 	= 0;  

  if(fTPCReader) 	fTPCReader ->close();  
  if(fSSDReader)        fSSDReader ->close();
  if(fEMCReader) 	fEMCReader ->close();  
  if(fEEMCReader) 	fEEMCReader->close();  
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
  if (m_ZeroTokens > 1){
    LOG_WARN << m_ZeroTokens << " events with token==0" << endm;
  }
  free(fDATAP);
  fDATAP = 0;
  close();
  delete fDatFileReader; fDatFileReader=0;
}
//_____________________________________________________________________________
/// NextEvent - this method is called to advance the next daq event if any
void StDAQReader::nextEvent()
{
   // Create the next event from evp data
   // qDebug() << " StEvpReader::NextEvent() - fEventType = " <<  fEventType;
   char *currentData = fDaqFileReader->get(0,EVP_TYPE_ANY); // EventNumber(),fEventType);
   LOG_DEBUG << " StEvpReader::NextEvent - data = "
             <<  (void *)currentData <<" :: " << fDaqFileReader
             << " status " << fDaqFileReader->status << " EVP_STAT_OK=" << EVP_STAT_OK
             << " token " << fDaqFileReader->token
             << endm
           ;

    fOffset = -1;
    if(currentData && (fDaqFileReader->status == EVP_STAT_OK) ) {
       fOffset = 1;
    } else { // event is not valid
      /* STAT_EOR is end of run.   In the case of reading from files, it means end of the file.
	 STAT_EVT is an error or corrupt event.  The next event can still be read but this one is junk
	 STAT_CRIT is an error that prevents us from reading more events... */
       switch(fDaqFileReader->status) {
          case EVP_STAT_EOR :  // EOR or EOR - might contain token 0!
             if(fDaqFileReader->IsEvp()) { // keep going until the next run...
                LOG_FATAL << "StEvpReader::NextEvent - waiting event" << endm;
                nextEvent();
             } else {
                LOG_DEBUG << "StEvpReader::NextEvent - End Of File"  << endm;
             }
             break;
          case EVP_STAT_EVT :
              LOG_ERROR <<  "Problem getting event - skipping" << endm;
              nextEvent();
              break;
          case EVP_STAT_CRIT :
              LOG_ERROR << "Critical error - halting..." << endm;
              nextEvent();
              break;
          default:
              LOG_FATAL << "Unknow DAQ file I/O problem " << endm;
       };
    } 
    fEventStatus = fDaqFileReader->status;
 }
//_____________________________________________________________________________
int StDAQReader::Make() 
{
   int ret = readEvent();
   if (fRtsMaker) fRtsMaker->Make();
   return ret;
}
//_____________________________________________________________________________
int StDAQReader::readEvent()
{
  int retStatus = kStOk;

  if (fDatFileReader) {
     fDatFileReader->Read();
     if (!fDatFileReader->good()) {
         fOffset   = -1;
         retStatus =  kStErr;
         if (fDatFileReader->eof() )   retStatus = kStEOF;
     }
  } else if (fDaqFileReader) {
     nextEvent();
     if (fOffset == -1)    retStatus = kStEOF;
  }

  delete fEventReader;  fEventReader=0;
  delete fRICHReader;   fRICHReader = 0;
  delete fL3Reader;     fL3Reader   = 0;
  delete fTOFReader;    fTOFReader  = 0;
  delete fFPDReader;    fFPDReader  = 0;
  if (retStatus == kStOk) {
  fEventReader = new EventReader();
  fEventReader->setVerbose(fVerbose);

  fEventReader->setDaqReader(fDaqFileReader);
  if (fDaqFileReader) {
     if (fDaqFileReader->mem) {
     //
     // the buffer of the new EVP_READER is mmap read-only file.
     // This means one can not chnage the buffer the kind of thing
     // the current STAR StDaqLib relies on.
     // To fix the issue we have to create the memory resided copy of the buffer
     // vf 26.12.2007
#  if 1
        fDATAP = (char *)realloc(fDATAP, fDaqFileReader->bytes);
        memcpy(fDATAP,fDaqFileReader->mem, fDaqFileReader->bytes);
        // Fix the DATAP Summary data
        fDaqFileReader->fixDatapSummary((DATAP*)fDATAP);
#  else
        fDATAP = (char *)realloc(fDATAP, fDaqFileReader->bytes_mapped);
        memcpy(fDATAP,fDaqFileReader->mem, fDaqFileReader->bytes_mapped);
#  endif
        fEventReader->InitEventReader(fDATAP);
       *fEventInfo = fEventReader->getEventInfo();
        if(fEventInfo->Token==0){
           LOG_INFO << 
              Form("StDAQReader::readEvent: found event with token==0") << endm;
           m_ZeroTokens++;
       // return kStErr;  // Herb, July 5 2000
     }
   }
    else if(fDaqFileReader && fDaqFileReader->token==0){
       LOG_INFO << 
              Form("StDAQReader::readEvent: found event with token==0") << endm;
       m_ZeroTokens++;
    }
    if (fTPCReader  && TPCPresent() ) fTPCReader ->Update();
    if (fFTPCReader && FTPCPresent()) fFTPCReader->Update();
    if (fTRGReader  && TRGPresent() ) {
         fTRGReader ->Update();
         if ( ! fTRGReader->thereIsTriggerData() ){
            LOG_INFO <<
               Form("StDAQReader::readEvent: No or bad TRG data - Skipping event: ") 
               << getRunNumber() << " : " <<getEventNumber()<< endm;
               return kStErr;
         }
     }
     if (fSVTReader  && SVTPresent() ) fSVTReader ->Update();
     if (fSSDReader  && SSDPresent() ) fSSDReader ->Update();
     if (fEMCReader  && EMCPresent() ) fEMCReader ->Update();
     if (fEEMCReader && EMCPresent() ) fEEMCReader->Update();
     if (fPMDReader  && PMDPresent() ) fPMDReader ->Update();
   
   // Trigger Summary
     int i;
     for(i=0;i<2;i++) fTrigSummary->L1summary[i]=fDaqFileReader->L1summary[i];
     for(i=0;i<2;i++) fTrigSummary->L2summary[i]=fDaqFileReader->L2summary[i];
     for(i=0;i<4;i++) fTrigSummary->L3summary[i]=fDaqFileReader->L3summary[i];
  } }
  return retStatus;
}
//_____________________________________________________________________________
int StDAQReader::skipEvent(int nskip)
{
  if (nskip == 1) {
     nextEvent();
  } else {
    char *currentData = 0;
    if (nskip == 2) currentData = fDaqFileReader->get(0,EVP_TYPE_ANY);
    else            currentData = fDaqFileReader->skip_then_get(nskip-2,0,EVP_TYPE_ANY); 
    fOffset = -1;
    if(currentData && (fDaqFileReader->status == EVP_STAT_OK) ) {
      fOffset = 1;
      nextEvent();
    } 
  }
  Int_t ok = 0;
  if (fOffset == -1) {
    ok = 1;
    LOG_WARN << Form("EOF with skipEvent(%d)",nskip)<< endm;
  }  
  return ok;
}


//_____________________________________________________________________________
void StDAQReader::setTPCVersion(const char* vers)
{strcpy(fTPCVersion,vers);} 
//_____________________________________________________________________________
void StDAQReader::setFTPCVersion(const char* vers)
{strcpy(fFTPCVersion,vers);} 

//_____________________________________________________________________________
int StDAQReader::getRunNumber()   const 
{
     // return the run number from the DAQ file header

   int runNum = -1;
   if (fDatFileReader) 
      runNum = fDatFileReader->RunNumber();
   else if (fDaqFileReader ) 
      runNum = fDaqFileReader->run;
   else 
      runNum =  fEventReader->runno(); 
   return runNum;
}
//_____________________________________________________________________________
int StDAQReader::getEventNumber() const {
     return fDaqFileReader  ? 
           fDaqFileReader->seq
                     :
           fEventInfo->EventSeqNo;
}
//_____________________________________________________________________________
unsigned int StDAQReader::getUnixTime() const 
{
   int unixTime = 0;
   if (fDatFileReader) 
      unixTime = fDatFileReader->RecordUnixTime();
   else if (fDaqFileReader ) 
      unixTime = fDaqFileReader->evt_time;
   else 
      unixTime = fEventInfo->UnixTime;
   return unixTime;
}

//_____________________________________________________________________________
unsigned int StDAQReader::getTrigWord() const {
     return fDaqFileReader  ?  
           fDaqFileReader->trgword
                      :
           fEventInfo->TrigWord;
}
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
   int StDAQReader::SSDPresent()  const {return  fEventInfo->SSDPresent;}
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
   int StDAQReader::TRGPresent()  const {return  ( fEventInfo && fEventInfo->TRGPresent) ||  (fDatFileReader &&  StTRGReader::OldFormat(fDatFileReader->Version()));}
//_____________________________________________________________________________
   int StDAQReader::L3Present()   const {return  fEventInfo->L3Present;}
//_____________________________________________________________________________
int StDAQReader::getEventSize()const 
{
   int eventSize = 0;
   if (fDatFileReader) 
      eventSize = fDatFileReader->Length();
   else
      eventSize = fEventInfo->EventLength; 
   return eventSize;
 }
//_____________________________________________________________________________
StTPCReader *StDAQReader::getTPCReader(char mergeSequences) 
{
  int updateStatus;
  if(!TPCPresent()) return 0;
  if (!fTPCReader) {
    fTPCReader = new StTPCReader(this,mergeSequences);
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
StSSDReader *StDAQReader::getSSDReader()
{
  if (!SSDPresent()) return 0;
  if (!fSSDReader) {
    fSSDReader = new StSSDReader(this);
  }
  return fSSDReader;
}
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
    if (!fRICHReader) fEventInfo->RICHPresent=0;
  }

  return fRICHReader;
}
//_____________________________________________________________________________
StL3Reader *StDAQReader::getL3Reader() 
{
  if (!L3Present()) return 0;
  if (!fL3Reader) {
    fL3Reader = ::getL3Reader(fEventReader);
    if (!fL3Reader) fEventInfo->L3Present=0;
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
    if(!fTOFReader) fEventInfo->TOFPresent=0;
  }
  return fTOFReader;
}
//_____________________________________________________________________________
StFPDReader *StDAQReader::getFPDReader()
{
  if (!FPDPresent()) return 0;
  if (!fFPDReader) {
    fFPDReader = ::getFPDReader(fEventReader);
    if (!fFPDReader) fEventInfo->FPDPresent=0;
  }
  return fFPDReader;
}
//_____________________________________________________________________________
StTRGReader *StDAQReader::getTRGReader()
{
  if (!TRGPresent()) return 0;
  if (!fTRGReader) {
    fTRGReader = new StTRGReader(this);
    //fTRGReader->Update();
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
