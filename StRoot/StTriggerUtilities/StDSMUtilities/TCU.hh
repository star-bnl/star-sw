//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#ifndef TCU_HH
#define TCU_HH

#include <string>
#include <map>
#include <set>

using namespace std;

struct TriggerDefinition {
  int    idx_trigger;
  string name;
  int    triggerId;
  int    physicsBits;
};

class TCU {
public:
  void defineTrigger(const TriggerDefinition& triggerDef);
  bool isPhysicsBits(int physicsBits) const;
  bool isTrigger(int triggerId) const;
  set<int> triggerIds() const;
  int  input() const { return mInput; }

  void setInput(int input) { mInput = input; }
  void clear() { mTriggers.clear(); }

protected:
  multimap<int, TriggerDefinition> mTriggers; // key=triggerId, value=trigger definition
  int mInput;
};

#endif	// TCU_HH
