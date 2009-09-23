//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 18, 2006
//

#include "JanEvent.h"
#include <cstring>

JanEvent::JanEvent()
{
  memset(mLength, 0, sizeof(mLength));
  memset(mHeader, 0, sizeof(mHeader));
  memset(mTriggerData, 0, sizeof(mTriggerData));
  memset(mBemcData, 0, sizeof(mBemcData));
  memset(mEemcData, 0, sizeof(mEemcData));
}

void JanEvent::setHeader(const char* header)
{
  if (header) {
    mLength[0] = strlen(header);
    strcpy(mHeader, header);
  }
}

void JanEvent::setTriggerData(char* trgData)
{
  if (trgData) {
    mLength[1] = sizeof(mTriggerData);
    memcpy(mTriggerData, trgData, sizeof(mTriggerData));
  }
}

void JanEvent::setBemcData(unsigned short* bemcData)
{
  if (bemcData) {
    mLength[2] = sizeof(mBemcData);
    memcpy(mBemcData, bemcData, sizeof(mBemcData));
  }
}

void JanEvent::setEemcData(unsigned short* eemcData)
{
  if (eemcData) {
    mLength[3] = sizeof(mEemcData);
    memcpy(mEemcData, eemcData, sizeof(mEemcData));
  }
}

istream& operator>>(istream& in, JanEvent& event)
{
  in.read((char*)event.mLength, sizeof(event.mLength));
  in.read(event.mHeader, event.mLength[0]);
  in.read(event.mTriggerData, event.mLength[1]);
  in.read((char*)event.mBemcData, event.mLength[2]);
  in.read((char*)event.mEemcData, event.mLength[3]);
  return in;
}

ostream& operator<<(ostream& out, const JanEvent& event)
{
  out.write((char*)event.mLength, sizeof(event.mLength));
  out.write(event.mHeader, event.mLength[0]);
  out.write(event.mTriggerData, event.mLength[1]);
  out.write((char*)event.mBemcData, event.mLength[2]);
  out.write((char*)event.mEemcData, event.mLength[3]);
  return out;
}
