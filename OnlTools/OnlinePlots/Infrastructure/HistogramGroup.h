#ifndef HistogramGroup_h
#define HistogramGroup_h

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TCanvas.h"
#include "TString.h"

#ifdef NEW_DAQ_READER
  class daqReader;
  typedef daqReader evpReader;
  typedef unsigned int UINT32;
#else
  class evpReader   ; // new  2007 DAQ file reader
#endif

class TMapFile;

#include <map>
#include <set>
#include <list>
#include <vector>
#include <string>
using namespace std;

#include "Rtypes.h"

#define PR(x) {cout << __PRETTY_FUNCTION__ << " " << #x "  " << x << endl;}

class HistogramGroup : public TNamed {
 public:
  HistogramGroup(const char* group, const char* subGroup, const char* trigger="any", const char* detector="any");
  //  HistogramGroup(const HistogramGroup&);
  virtual ~HistogramGroup() {  }
  virtual bool fill(evpReader* evp, char* datap) { return true; }
  virtual void draw(TCanvas* cc);
  virtual void reset() {}
  virtual void beginRun(evpReader* evp, char* datap) {}  
  virtual void endRun() {}  
  virtual void finish() {}  

 public:
  const char* groupName() const { return mGroupName.Data(); } 
  const char* subGroupName() const { return mSubGroupName.Data(); } 
  const char* triggerName() const { return mTriggerName.Data(); } 
  const char* detectorName() const { return mDetectorName.Data(); } 
  const char* id() const { return mId.Data(); }
  bool operator<(const HistogramGroup& hg) const;
 protected:
  //char mGroupName[1024];
  //char mSubGroupName[1024];
  //char mTriggerName[1024];
  //char mDetectorName[1024];
  //char mId[1024];
  /*
  string groupName() const { return mGroupName; } 
  string subGroupName() const { return mSubGroupName; } 
  string triggerName() const { return mTriggerName; } 
  string detectorName() const { return mDetectorName; } 
  string id() const { return mId; }
  `*/
 protected:
  TString mGroupName;
  TString mSubGroupName;
  TString mTriggerName;
  TString mDetectorName;
  TString mId;
 public:
  bool testBits(unsigned int trigger, unsigned int detector);
  void setActive(bool b) { mActive = b; }
  bool active() { return mActive; }
 protected:
  char* pre(const char* a);
  void pre(vector<TH1*>& );
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;
  bool mActive;

  ClassDef(HistogramGroup,2) ;
};


#endif
