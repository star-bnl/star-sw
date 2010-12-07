#ifndef RunStatus_h
#define RunStatus_h

#include <Rtypes.h>
#include <TROOT.h>
#include <iostream>
#include <iomanip>
using namespace std;


class RunStatus : public TObject {
 private:
  int mRun;
  int mEvent;
  int mCount;
  int mEnd;
  int mLive;
  int mToken;
  int mEmpty;
  int mType;
  int mStatus;
  int mTime;
  int mEndOfRunActionPerformed;
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;
  unsigned int mTriggerBitsRun;
  unsigned int mDetectorBitsRun;

  int mNumPlotSets;  // number of plots sets connected

 public:

  RunStatus() :    
    mRun(0),
    mEvent(0),
    mCount(0),
    mEnd(0),
    mLive(0),
    mToken(0),
    mEmpty(0),
    mType(0),
    mStatus(0),
    mTime(0),
    mEndOfRunActionPerformed(0),
    mTriggerBits(0),
    mDetectorBits(0),
    mTriggerBitsRun(0),
    mDetectorBitsRun(0),
    mNumPlotSets(0)
    {}
  
 int getRunNumber() { return mRun;}
 int getEventNumber() { return mEvent;}
 int getEventCounter() { return mCount;}
 int getEndOfRun() { return mEnd;}
 int getLiveSource() { return mLive;}
 int getToken() { return mToken;}
 int getEmpty() { return mEmpty;}
 int getType() { return mType;}
 int getStatus() { return mStatus;}
 int getTime() { return mTime;}
 int getEndOfRunActionPerformed() { return mEndOfRunActionPerformed;}
 unsigned int getTriggerBits() { return mTriggerBits; }
 unsigned int getDetectorBits() { return mDetectorBits; }
 unsigned int getTriggerBitsRun() { return mTriggerBitsRun; }
 unsigned int getDetectorBitsRun() { return mDetectorBitsRun; }
 int getNumPlotSets() { return mNumPlotSets; }

 void setRunNumber(int n) { if (n==0) return; mRun = n;}
 void setEventNumber(int n) { if (n==0) return; mEvent = n;} // endOfRun event has run==0 ; I don't want this event to reset the run#
 void setEventCounter(int n) { mCount = n;}
 void setEndOfRun(int n) { mEnd = n;}
 void setLiveSource(int n) { mLive = n;}
 void setToken(int n) { mToken = n;}
 void setEmpty(int n) { mEmpty = n;}
 void setType(int n) { mType= n;}
 void setStatus(int n) { mStatus= n;}
 void setTime(int n) { mTime= n;}
 void setTriggerBits(unsigned int n) { mTriggerBits= n;}
 void setDetectorBits(unsigned int n) { mDetectorBits= n;}
 void setTriggerBitsRun(unsigned int n) { mTriggerBitsRun= n;}
 void setDetectorBitsRun(unsigned int n) { mDetectorBitsRun= n;}
 void setEndOfRunActionPerformed(int n) { mEndOfRunActionPerformed= n;}
 void setNumPlotSets(int n) { mNumPlotSets = n; }

 void dump();

 friend ostream& operator<<(ostream& o, RunStatus& rs);
 
 ClassDef(RunStatus,1) ;
};

inline ostream& operator<<(ostream& o, RunStatus& rs) {
  o << "run=" << rs.mRun;
  o << "evt=" << rs.mEvent;
  o << "sta=" << rs.mStatus;
  o << "emp=" << rs.mEmpty;
  o << "trg=" << hex << rs.mTriggerBits;
  o << "det=" << hex << rs.mDetectorBits;
  o << "trgRun=" << hex << rs.mTriggerBitsRun;
  o << "detRun=" << hex << rs.mDetectorBitsRun;
  o << "eof=" << rs.mEndOfRunActionPerformed;
  return o;
}


#endif





/***************************************************************************
 *
 * $Id: RunStatus.h,v 1.1 2010/12/07 14:25:22 jml Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: RunStatus.h,v $
 * Revision 1.1  2010/12/07 14:25:22  jml
 * adding Jevp
 *
 * Revision 1.1  2010/03/29 15:16:16  jml
 * more changes..
 *
 * Revision 1.1  2010/03/19 17:58:14  jml
 * modify to scripted version, add hlt
 *
 * Revision 1.1  2009/10/20 20:38:09  jml
 * getting up to date...
 *
 * Revision 1.1  2009/01/23 16:10:58  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.2  2007/05/24 13:15:18  jml
 * blah
 *
 * Revision 1.1  2007/02/27 15:23:39  laue
 * Initial version
 *
 * Revision 1.1  2006/10/04 20:31:16  laue
 * Initial Version
 *
 *
 ***************************************************************************/

