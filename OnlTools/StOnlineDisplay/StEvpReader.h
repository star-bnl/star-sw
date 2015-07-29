// $Id: StEvpReader.h,v 1.3 2015/07/29 16:53:18 smirnovd Exp $

#ifndef STAR_StEvpReader
#define STAR_StEvpReader

/*!
 *                                                                     
 * \class  StEvpReader
 * \author  Fine
 * \date   2004/11/17
 * \brief  Online display module
 *
 * This class is to reade the DAQ events from the DAQ event pool via
 * separate  thread
 *
 * 
 * StEvpReader virtual base class for Maker                        
 * Template Maker doing nothing. See README file in StRoot/StEvpReader
 *
 *
 */                                                                      
#ifdef __ROOT__
#  undef __ROOT__
#endif
#include "TString.h"

class daqReader;

// You may forward declare other classes if you have data-members
// used in pointer-only context by using declaration like
// class St_SomeExternClass;
//
// You do need in such simple case to add the include file
// (and compilation is much faster).
class GeomBrowser;
class StEvpReader {
 private:
   // Private method declaration if any
    int  fEventType;
    int  fEventNumber;  // event number of ZERO for next event
    int  fEventStatus;
    int  fInterval;    // timer interval
    daqReader *fEvpReader;
    bool  fLiveEvent;
 protected:
    static bool fgRts_LogActive;

public: 
    StEvpReader();
    StEvpReader(const TString &daqSrc, const TString &mountPoint);
    virtual       ~StEvpReader();
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEvpReader.h,v 1.3 2015/07/29 16:53:18 smirnovd Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  void SetEvpDisk(const TString &mountPoint);
  int  EventNumber() const;
  daqReader *GetReader() const { return fEvpReader; }
  daqReader *GetReader()       { return fEvpReader; }
 
public:
  void NextEvents(int interval=-1);
  void NextEvent();
  void SetEventNumber(int eventNumber2beRead);
  void StopEvents();
  void RestartReader(const TString &fileName, const TString &mountPoint);
  int EventStatus() const { return fEventStatus; }
};

#endif
