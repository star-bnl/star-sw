//
// Pibero Djawotho <pibero@comp.tamu.edu>
// Texas A&M University Cyclotron Institute
// 7 Jan 2009
//

#include "DSMLayer_LD301_2009.hh"
#include "TCU_2009.hh"

TCU_2009::TCU_2009() : mDSMInput(0), mTCUInput(0)
{
  defineTrigger("bemc-jp1-mb", 137222, 0x4020);
  defineTrigger("eemc-jp1-mb", 137273, 0x4080);
  defineTrigger("bemc-jp0-mb", 137501, 0x4010);
  defineTrigger("bemc-jp0-mb", 137501, 0x4020);
  defineTrigger("eemc-jp0-mb", 137551, 0x4040);
  defineTrigger("eemc-jp0-mb", 137551, 0x4080);
  defineTrigger("bemc-jp2",    137585, 0x0030);
  defineTrigger("eemc-jp2",    137635, 0x00c0);
  defineTrigger("jpsi-mb",     137705, 0x4008);
  defineTrigger("ajp-mb",      999996, 0x4400);
  defineTrigger("emc2-jp0-mb", 999997, 0x4100);
  defineTrigger("emc2-jp0-mb", 999997, 0x4200);
  defineTrigger("emc2-jp1-mb", 999998, 0x4200);
  defineTrigger("emc2-jp2",    999999, 0x0300);
}

void TCU_2009::read(const TriggerDataBlk& event)
{
  EvtDescData* evtDesc = (EvtDescData*)((char*)&event+event.EventDesc_ofl.offset);
  mDSMInput = evtDesc->DSMInput;
}

void TCU_2009::read(const DSMLayer_LD301_2009& layer)
{
  mTCUInput = layer[0].output & 0xffff;
}

void TCU_2009::update()
{
  mTCUInput = mDSMInput;
}

void TCU_2009::defineTrigger(const char* name, int id, int mask)
{
  mTriggers.insert(make_pair(id, make_pair(name, mask)));
}

int TCU_2009::numberOfTriggers() const { return mTriggers.size(); }

bool TCU_2009::isTrigger(int triggerId) const
{
  pair<MapIterator, MapIterator> p = mTriggers.equal_range(triggerId);
  for (MapIterator i = p.first; i != p.second; ++i)
    if (isTrigger(i)) return true;
  return false;
}

string TCU_2009::triggerName(int triggerId) const
{
  MapIterator i = mTriggers.find(triggerId);
  return (i == mTriggers.end()) ? "" : triggerName(i);
}

void TCU_2009::getTriggerMasks(int triggerId, vector<int>& masks) const
{
  pair<MapIterator, MapIterator> p = mTriggers.equal_range(triggerId);
  for (MapIterator i = p.first; i != p.second; ++i)
    masks.push_back(triggerMask(i));
}

int TCU_2009::barrelJetPatchBits(int triggerId) const
{
  vector<int> masks;
  getTriggerMasks(triggerId, masks);
  return masks.empty() ? 0 : *min_element(masks.begin(), masks.end()) >> 4 & 0x3;
}

int TCU_2009::endcapJetPatchBits(int triggerId) const
{
  vector<int> masks;
  getTriggerMasks(triggerId, masks);
  return masks.empty() ? 0 : *min_element(masks.begin(), masks.end()) >> 6 & 0x3;
}

int TCU_2009::emcJetPatchBits(int triggerId) const
{
  vector<int> masks;
  getTriggerMasks(triggerId, masks);
  return masks.empty() ? 0 : *min_element(masks.begin(), masks.end()) >> 8 & 0x3;
}

void TCU_2009::dump() const
{
  printf("TCU: 0x%04x\n", mTCUInput);
  for (MapIterator i = mTriggers.begin(); i != mTriggers.end(); ++i)
    printf("%s\t%d\t0x%04x\t%d\n", triggerName(i).c_str(), triggerId(i), triggerMask(i), isTrigger(i));
}
