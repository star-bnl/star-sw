//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef TCU_2009_HH
#define TCU_2009_HH

class DSMLayer_LD301_2009;

#include <string>
#include <vector>
#include <map>

using namespace std;

#ifdef __ROOT__
#include "RTS/trg/include/trgDataDefs_46.h"
#include "RTS/trg/include/trgConfNum.h"
#else
#include "trgDataDefs_46.h"
#include "trgConfNum.h"
#endif

class TCU_2009 {
public:
  TCU_2009();

  unsigned short bits() const { return mDSMInput; }
  void read(const TriggerDataBlk& event);
  void read(const DSMLayer_LD301_2009& layer);
  void update();
  bool isTrigger(int triggerId) const;
  int numberOfTriggers() const;
  string triggerName(int triggerId) const;
  void getTriggerMasks(int triggerId, vector<int>& masks) const;
  int barrelJetPatchBits(int triggerId) const;
  int endcapJetPatchBits(int triggerId) const;
  int emcJetPatchBits(int triggerId) const;
  void dump() const;

private:
  typedef map<int, pair<string, int> >::const_iterator MapIterator;

  void   defineTrigger(const char* name, int id, int mask);
  int    triggerId(const MapIterator& i) const { return i->first; }
  string triggerName(const MapIterator& i) const { return i->second.first; }
  int    triggerMask(const MapIterator& i) const { return i->second.second; }
  bool   isTrigger(const MapIterator& i) const;

  unsigned short mDSMInput;	// from trigger data
  unsigned short mTCUInput;	// from simulation
  multimap<int, pair<string, int> > mTriggers;
};

inline bool TCU_2009::isTrigger(const MapIterator& i) const
{
  int mask = triggerMask(i);
  return (mTCUInput & mask) == mask;
}

#endif	// TCU_2009_HH
