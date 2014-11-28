//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "TriggerDefinition.hh"
#include "TCU.hh"

void TCU::defineTrigger(TriggerDefinition& triggerDef)
{
  mTriggers.insert(make_pair(triggerDef.triggerId, triggerDef));
}

bool TCU::isOnBits(int onbits) const
{
  if(onbits != 0){	
    return (mInput & onbits) == onbits;
  }
    return 0;
}

bool TCU::isTrigger(int triggerId) const
{
  typedef multimap<int, TriggerDefinition>::const_iterator MI;
  pair<MI, MI> p = mTriggers.equal_range(triggerId);
  for (MI i = p.first; i != p.second; ++i)
    //onbits comparision for run11 and run12 -- zchang
    {

      if (i->second.onbits&&isOnBits(i->second.onbits)) {
	return true;
      }
    }
  return false;
}

set<int> TCU::triggerIds() const
{
  set<int> s;
  for (multimap<int, TriggerDefinition>::const_iterator i = mTriggers.begin(); i != mTriggers.end(); ++i)
    if (isTrigger(i->first)) s.insert(i->first);
  return s;
}
