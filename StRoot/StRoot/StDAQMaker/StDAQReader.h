/***************************************************************************
 *
 * $Id: StDAQReader.h,v 1.43 2012/09/13 20:01:49 fisyak Exp $
 *
 * Author: Victor Perev
 ***************************************************************************
 *
 * Description: Offline Wrapper for DAQ reader classes
 *
 ***************************************************************************
 *
 * $Log: StDAQReader.h,v $
 * Revision 1.43  2012/09/13 20:01:49  fisyak
 * Clean up, use Jeff's skip_then_get
 *
 * Revision 1.42  2009/10/13 15:51:48  fine
 * Activate the new DAT file format
 *
 * Revision 1.41  2009/10/07 23:21:56  fine
 * Move the StRtsReaderMaker instantiation fro  StDAQReader to StDAQMaker to make sure it is located just after StDAQMaker in chain
 *
 * Revision 1.40  2009/10/07 00:52:32  fine
 * Move daqReader instantiation from StDAQMaker to StDAQReader to switch between input files properly
 *
 * Revision 1.39  2009/04/06 18:22:33  fine
 * remove the redundant methods and fix L1/L2/L3 summary
 *
 * Revision 1.38  2009/01/20 22:56:32  fine
 * remove the redundant CPP flag
 *
 * Revision 1.37  2009/01/14 18:20:49  fine
 * Remove the redundant devReader type
 *
 * Revision 1.36  2008/11/26 15:53:12  fine
 * fix the CPP flags for StDAQReader.h
 *
 * Revision 1.35  2008/11/25 21:42:53  fine
 * preparing  DAQ maker for DAQ_READER
 *
 * Revision 1.34  2008/01/29 01:55:56  fine
 * Add extra CPP flag to simply the Conscript
 *
 * Revision 1.33  2008/01/22 21:13:01  fine
 * move the dependency from the class desclaration to class implematation to allo for the new EVP_READER
 *
 * Revision 1.32  2007/12/24 05:19:58  fine
 * Introduce the shadow copy of the evp buffer for the new EVP_READER
 *
 * Revision 1.31  2007/12/22 01:14:59  fine
 * version compatible with new/old DAQ readers
 *
 * Revision 1.30  2007/08/07 19:44:11  perev
 * Gene scalers added
 *
 * Revision 1.29  2004/08/07 02:39:00  perev
 * Traditional Clear added
 *
 * Revision 1.28  2004/03/05 15:38:08  fisyak
 * Add default for getTPCReader (to keep interface unchanged)
 *
 * Revision 1.27  2004/03/04 21:51:27  ward
 * Replaced MERGE_SEQUENCES with a StDAQMaker chain parameter, as suggested by Landgraf and Lauret.
 *
 * Revision 1.26  2004/02/18 20:17:45  ward
 * Access SSD data in makers.
 *
 * Revision 1.25  2004/02/03 03:25:06  jeromel
 * Added counter for ZeroToken events
 *
 * Revision 1.24  2003/12/24 21:30:45  perev
 * Cleanup of DAQ
 *
 * Revision 1.23  2003/09/28 01:57:21  jeromel
 * No change (indent)
 *
 * Revision 1.22  2003/03/24 18:12:10  ward
 * Full support for EEMC from Herbert Ward.
 *
 * Revision 1.21  2003/02/16 16:02:49  perev
 * new method added
 *
 * Revision 1.20  2002/12/19 22:28:27  perev
 * PMD added
 *
 * Revision 1.19  2002/01/17 21:14:38  perev
 * Akio FPD reader
 *
 * Revision 1.18  2001/07/16 21:38:44  perev
 * EMC added
 *
 * Revision 1.17  2001/07/10 18:13:04  jeromel
 * Changes commited for Frank Geurts (TOF) after approval from Herb Ward
 * on Tue, 10 Jul 2001 11:19:48 and review by Victor.
 * Changes implements TOF DAQ Reader.
 *
 * Revision 1.16  2001/06/29 17:08:24  perev
 * Hide DaqLib includes from CINT
 *
 * Revision 1.15  2001/06/19 21:10:49  jeromel
 * Changes for FTPCReader (Janet S.)
 *
 * Revision 1.14  2000/07/13 22:35:10  perev
 * More accurate __CINT__ flags added
 *
 * Revision 1.13  2000/06/30 21:53:40  perev
 * L3 stuff added
 *
 * Revision 1.12  2000/06/12 15:41:19  perev
 * StTPCRead.h added into StDAQReader.h
 *
 * Revision 1.11  2000/06/12 15:04:02  perev
 * SVT + cleanup
 *
 * Revision 1.10  2000/05/25 20:01:58  perev
 * EventSize added
 *
 * Revision 1.9  2000/04/07 15:43:19  perev
 * SetVerbose method added
 *
 * Revision 1.8  2000/01/24 20:35:37  ward
 * Access trigger data.
 *
 * Revision 1.7  2000/01/24 14:39:33  perev
 * FTPC (HolmMade) is added
 *
 * Revision 1.6  1999/12/15 18:40:59  perev
 * Keep undeleted all DAQ wrapper classes
 *
 * Revision 1.5  1999/08/06 16:20:59  perev
 * RICHReader added
 *
 * Revision 1.4  1999/08/01 00:14:50  perev
 * leak removed author added
 *
 * Revision 1.3  1999/08/01 00:10:57  perev
 * leak removed author added
 *
 *
 **************************************************************************/
#ifndef _StDAQReader_
#define _StDAQReader_
//		Forward declarations
struct  EventInfo;
typedef  EventInfo DAQEventInfo;
class EventReader ;  
class StTPCReader ;  
class StSSDReader ;
class StEMCReader ;  
class StEEMCReader;  
class StPMDReader ;  
class StFTPCReader;  
class StTRGReader ;
class StSVTReader ;
class TDataSet    ;

class daqReader;
typedef daqReader evpReader;

#ifndef __CINT__

struct RICH_Reader;
struct L3_Reader;
struct TOF_Reader;
struct FPD_Reader;

typedef RICH_Reader StRICHReader;
typedef L3_Reader   StL3Reader;
typedef TOF_Reader  StTOFReader;
typedef FPD_Reader  StFPDReader;

#endif /*__CINT__*/

#ifdef __CINT__

class StRICHReader;
class StL3Reader;
class StTOFReader;
class StFPDReader;

#endif /*__CINT__*/
class StTrigSummary {
public:
  int L1summary[2];
  int L2summary[2];
  int L3summary[4];
};
class StRtsReaderMaker;
class StStreamFile;
//

 
class StDAQReader 
{
public:
  StDAQReader(const char *file=0, StRtsReaderMaker* rtsMaker=0);
  virtual ~StDAQReader();
          void clear();
  virtual void setVerbose(int ver=1){fVerbose=ver;};
  virtual int open(const char *file);
  virtual int close();
  virtual int isOpened(){ return (fFd != (-1));};
  virtual int Make();
  virtual int skipEvent(int nskip);
  virtual int getRunNumber() const;
  virtual int getEventNumber() const;
  virtual unsigned int getUnixTime() const;
  virtual unsigned int getTrigWord() const;
  virtual unsigned int getTrigInputWord() const;
  
  virtual int TPCPresent () const;  
  virtual int SVTPresent () const; 
  virtual int TOFPresent () const;
  virtual int FPDPresent () const;
  virtual int SSDPresent () const;
  virtual int EMCPresent () const;
  virtual int PMDPresent () const;
  virtual int SMDPresent () const;
  virtual int FTPCPresent() const;
  virtual int RICHPresent() const;
  virtual int TRGPresent () const;
  virtual int L3Present  () const;
 
  virtual void setTPCVersion(const char* vers = "TPCV2P0"); 
  virtual void setFTPCVersion(const char* vers = "FTPV1P0"); 
  virtual const char *getTPCVersion()  const {return fTPCVersion ;} 
  virtual const char *getFTPCVersion() const {return fFTPCVersion;} 
  StTPCReader  *getTPCReader (char mergeSequences=1); 
  StSSDReader  *getSSDReader ();
  StEMCReader  *getEMCReader (); 
  StEEMCReader *getEEMCReader(); 
  StPMDReader  *getPMDReader (); 
  StRICHReader *getRICHReader(); 
  StFTPCReader *getFTPCReader(); 
  StTRGReader  *getTRGReader ();
  StSVTReader  *getSVTReader ();
  StL3Reader   *getL3Reader  ();
  StTOFReader  *getTOFReader ();
  StFPDReader  *getFPDReader ();
  StTrigSummary *getTrigSummary () const {return fTrigSummary;}
  virtual void printEventInfo();
  virtual int  getEventSize() const;
  virtual EventReader  *getEventReader()   const {return fEventReader;  }
  virtual daqReader    *getFileReader()    const {return fDaqFileReader;}
  virtual StStreamFile *getDatFileReader() const {return fDatFileReader;}

protected:
  void nextEvent();
  virtual int readEvent();
  int  fEventStatus;

protected:
  int m_ZeroTokens;   //! number of events with token==0
  int fFd;	      //  File descriptor
  int fVerbose;
  EventReader  *fEventReader;  
  StTPCReader  *fTPCReader;  
  StSSDReader  *fSSDReader;
  StEMCReader  *fEMCReader;  
  StEEMCReader *fEEMCReader;  
  StPMDReader  *fPMDReader;  
  StFTPCReader *fFTPCReader;  
  StRICHReader *fRICHReader;
  StTRGReader  *fTRGReader;
  StSVTReader  *fSVTReader;
  StL3Reader   *fL3Reader;
  StTOFReader  *fTOFReader;
  StFPDReader  *fFPDReader;
  long fOffset;
  DAQEventInfo *fEventInfo;
  char *fFile;
  char fTPCVersion[12];
  char fFTPCVersion[12];
  StTrigSummary *fTrigSummary; //!
  daqReader     *fDaqFileReader;
  StStreamFile  *fDatFileReader;
  char *fDATAP;
  StRtsReaderMaker *fRtsMaker;  //! pointer to the RTS_READER
};
#ifndef __CINT__
#include "StTPCReader.h"
#include "StFTPCReader.h"
#include "StEMCReader.h"
#include "StEEMCReader.h"
#include "StPMDReader.h"
#endif /*__CINT__*/
#endif /*end*/
