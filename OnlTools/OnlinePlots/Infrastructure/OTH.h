#ifndef OTH_H
#define OTH_h

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"



#include <map>
#include <set>
using namespace std;

class OTHCondition {
 public:
  OTHCondition(unsigned int trig=0xffff, unsigned int det=0xffff) : mTriggerBits(trig), mDetectorBits(det) {}
 private:
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;
  friend class OTH;
};



class OTH {
  public:
  static OTH* instance();
  void setTriggerBits(unsigned int trig) { mTriggerBits = trig;}
  void setDetectorBits(unsigned int det) { mDetectorBits = det;}
  unsigned int triggerBits() { return mTriggerBits; }
  unsigned int detectorBits() { return mDetectorBits; }
  void add(TH1* histo, unsigned int trig=0xffff, unsigned int det=0xffff);
  bool fill(TH1* histo, double x, double w=1.);
  bool fill(TH2* histo, double x, double y, double w=1.);
  bool fill(TH3* histo, double x, double y, double z, double w=1.);
  bool test(TH1* histo);
 private:
  static OTH* _instance;
  OTH(unsigned int trig=0xffff, unsigned int det=0xffff) : mTriggerBits(trig), mDetectorBits(det) {}
  unsigned int mTriggerBits;
  unsigned int mDetectorBits;
  map<TH1*, OTHCondition> mConditions;
};


inline 
void OTH::add(TH1* h, unsigned int trig, unsigned int det) {
  mConditions[h] = OTHCondition(trig,det);
}

inline 
bool OTH::test(TH1* h) {
  map<TH1*, OTHCondition>::const_iterator iter = mConditions.find(h);
  if ( iter==mConditions.end() ) return false;
  const OTHCondition& cond = (*iter).second;
  return (mTriggerBits|cond.mTriggerBits) && (mDetectorBits|cond.mDetectorBits);
}

inline 
bool OTH::fill(TH1* h, double x, double w) {
  if ( !test(h) ) return false;
  h->Fill(x,w);
  return true;
}

inline 
bool OTH::fill(TH2* h, double x, double y, double w) {
  if ( !test(h) ) return false;
  h->Fill(x,y,w);
  return true;
}

inline 
bool OTH::fill(TH3* h, double x, double y, double z, double w) {
  if ( !test(h) ) return false;
  h->Fill(x,y,z,w);
  return true;
}

#endif
