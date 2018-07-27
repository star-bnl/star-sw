//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "TriggerDefinition.hh"
#include "TCU.hh"

#include<iostream> //A
using std::cout; //A

//void TCU::defineTrigger(const TriggerDefinition& triggerDef)
void TCU::defineTrigger(TriggerDefinition& triggerDef)
{
  mTriggers.insert(make_pair(triggerDef.triggerId, triggerDef));
}

bool TCU::isOnBits(int onbits) const
{
//  std::cout<<"Input: "<<hex<<mInput<<" Onbits: "<<hex<<onbits<<"\n";
  if(onbits != 0){	
    return (mInput & onbits) == onbits;
  }
    return 0;
}

bool TCU::isTrigger(int triggerId) const
{
  typedef multimap<int, TriggerDefinition>::const_iterator MI;
  pair<MI, MI> p = mTriggers.equal_range(triggerId);
  //cout << "Danny was HERE" << endl;
  for (MI i = p.first; i != p.second; ++i)
    //include onbits1 comparision for run11 and run12
    {

      if (i->second.onbits&&isOnBits(i->second.onbits)) {
//        std::cout<<"TriggerID: "<<dec<<triggerId<<" Onbits: "<<hex<<i->second.onbits<<"\n";
	return true;
      }
      /*
      if(i->second.onbits1&&isOnBits(i->second.onbits1)){
        std::cout<<"TriggerID: "<<dec<<triggerId<<" Onbits: "<<hex<<i->second.onbits1<<"\n";
	return true;
      }
      */
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
