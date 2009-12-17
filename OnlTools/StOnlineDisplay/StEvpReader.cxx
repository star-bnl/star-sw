//*-- Author : Valeri Fine
// 
// $Id: StEvpReader.cxx,v 1.4 2009/12/17 21:39:45 fine Exp $

#include "StMemStat.h"
#include "StEvpReader.h"
#include "Riostream.h"
#include <QMutexLocker>
#include "TModule.h"
#include <QDebug> 
#include <QTimer> 
#include <QWaitCondition>
#include <QMessageBox>
#include <string>
#include "DAQ_READER/daqReader.h"
#include "rtsLog.h"

bool StEvpReader::fgRts_LogActive=false;
//_____________________________________________________________________________
/// StEvpReader constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
//_____________________________________________________________________________
StEvpReader::StEvpReader() :
 fEventType(EVP_TYPE_ANY),fEventNumber(0),fEventStatus(kStEOF),fInterval(-1), fEvpReader(0)
, fLiveEvent(false)
{
   if (!fgRts_LogActive) {
      fgRts_LogActive = true;
      rtsLogOutput(RTS_LOG_STDERR);
      rtsLogLevel((char*)ERR);
   }
   fEvpReader = new daqReader(0);
   fLiveEvent = true;
   fEventStatus = kStOK;
   qDebug() << " Live event StEvpReader::StEvpReader - fEventType = " <<  fEventType << " evp=" << fEvpReader;
}
//_____________________________________________________________________________
StEvpReader::StEvpReader(const TString &fileName, const TString &mountPoint):
  fEventType(EVP_TYPE_ANY),fEventNumber(0),fEventStatus(kStEOF),fInterval(-1), fEvpReader(0)
, fLiveEvent(false)
{
   if (!fgRts_LogActive) {
      fgRts_LogActive = true;
      rtsLogOutput(RTS_LOG_STDERR);
      rtsLogLevel((char *)ERR);
   }
   RestartReader(fileName,mountPoint);
}
//_____________________________________________________________________________
/// SetEvpDisk -set the STAREvent Pool location
void StEvpReader::SetEvpDisk(const TString &mountPoint)
{
   if (fEvpReader) {
      if (mountPoint.IsNull() )
         fEvpReader->setEvpDisk((char *)"") ;
      else 
         fEvpReader->setEvpDisk((char *)mountPoint.Data()) ;
   }
}

//_____________________________________________________________________________
/// This is StEvpReader destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StEvpReader::~StEvpReader()
{
   //
   delete fEvpReader;
}

//_____________________________________________________________________________
int  StEvpReader::EventNumber() const
{
   // QMutexLocker locker(&mutex);
   return fEventNumber;
}

//_____________________________________________________________________________
void StEvpReader::SetEventNumber(int eventNumber2beRead)
{
   // QMutexLocker locker(&mutex);
   fEventNumber = eventNumber2beRead;
}
/// RestartReader - restart the DAQ reader  for new file/mount point combination.
//_____________________________________________________________________________
void StEvpReader::RestartReader(const TString &fileName, const TString &mountPoint)
{
   fInterval = -1;  // to stop event.
   std::string fn = fileName.Data();
   StMemStat::PrintMem("1. StEvpReader::RestartReader  . . .");
   if (fEvpReader) { 
      StMemStat::PrintMem("2. StEvpReader::RestartReader  . . .");
      delete fEvpReader;  fEvpReader = 0; 
      StMemStat::PrintMem("3. StEvpReader::RestartReader daqReader has been deleted . . .");
   }
   const char *c_fn = fn.c_str();
   if (c_fn && c_fn[0]) 
       fEvpReader = new daqReader((char *)fn.c_str());
   else  {
       fLiveEvent = true;
       fEvpReader = new daqReader(0);
       qDebug() << " --> Live event StEvpReader::StEvpReader - fEventType = " <<  fEventType << " evp=" << fEvpReader;
   }
   StMemStat::PrintMem("4. StEvpReader::RestartReader  daqReader has been recreated. . .");
   SetEvpDisk(mountPoint);
   fEventStatus = kStOk;
   qDebug() << " StEvpReader::StEvpReader - fEventType = " <<  fEventType 
            << " evp=" << fEvpReader << " from file:" <<fileName;
   StMemStat::PrintMem("5. StEvpReader::RestartReader  Done. . .");
}
//_____________________________________________________________________________
/// StopEvents - this method is called open the next daq file if any
void StEvpReader::StopEvents()
{
   fInterval = -1;
}
//_____________________________________________________________________________
/// NextEvent - this method is called open the next daq file if any
void StEvpReader::NextEvents(int interval)
{
   fInterval = interval;
//   qDebug() <<" StEvpReader::NextEvents(" << fInterval <<")";
   if (fInterval >=0 ) NextEvent();
}

//_____________________________________________________________________________
/// NextEvent - this method is called open the next daq file if any
void StEvpReader::NextEvent()
{
   // vf 24.02.2009 QMutexLocker locker(StEvpReaderThread::Mutex());
   // Create the next event from evp data
   if ( fEvpReader && ((fEventStatus == kStOK) || (fEventStatus == kStERR) ) ) {
      int retStatus=fEventStatus;
      qDebug() << " StEvpReader::NextEvent() - getting event ... ' "
               <<  EventNumber() << " fEventType = " <<  fEventType;
      bool searchEvent = false;
      do {
          searchEvent  = false;
          daqReader *currentData =(daqReader *) fEvpReader->get(EventNumber(),fEventType); 
#if 1
//   if (EventNumber() != reader->event_number) continue;
      qDebug() << " StEvpReader::NextEvent - data = " 
               <<  (void *)currentData << fEvpReader 
               << ", event # = " << EventNumber() << " :: " << fEvpReader->seq
               << ", run   # = " << fEvpReader->run
               << " event type " << fEventType << "::" << EVP_TYPE_ANY
               << " status " << fEvpReader->status << " EVP_STAT_OK=" << EVP_STAT_OK
               << " token " << fEvpReader->token << " Interval= " << fInterval << fLiveEvent;
            ;
#endif
           retStatus = kStErr; 
           if (currentData && fEvpReader->status == EVP_STAT_OK  && fEvpReader->token) {
               retStatus = kStOk;
               if ( EventNumber() ) {
                  if ( EventNumber() ==  (int) fEvpReader->seq  )  {  
                      SetEventNumber(0);
                  } else if ( EventNumber() < (int)fEvpReader->seq) {
                     QMessageBox::warning(0,"No Event Number",QString(EventNumber()));
                  } else {
                     searchEvent = true;
                  }
               } else  if (fInterval >=0 ) {
               }
            } else {
               switch(fEvpReader->status) {
                  case EVP_STAT_EOR :	// EOR or EOR - might contain token 0!
                     if(fEvpReader->isevp) {	// keep going until the next run...
                        //  				retStatus = kOK;
                        qDebug() << " StEvpReader::NextEvent - End OfRun" ;
                     } else {
                        retStatus = kStEOF;
                        qDebug() << " StEvpReader::NextEvent - End Of File" ;
                     // let's kill this reader 
                        delete fEvpReader;  fEvpReader = 0;
                     }
                     break;
                  case EVP_STAT_EVT :
                     qDebug () << "Problem getting event - skipping";
                     break;
                  case EVP_STAT_CRIT :
                     qDebug () << "Critical error - halting...";
                     break;
               };
               if (fLiveEvent && fInterval>=0) {
   // emit               QTimer::singleShot (340, this, SLOT(NextEvent()));
               } else {
   // emit               emit EvpEOF();
               }
           }
           fEventStatus = retStatus;
      } while (searchEvent );
   }
//    if (fInterval>=0 ) QTimer::siEXgleShot(fInterval, this, SLOT(NextEvent()));
}
