//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Feb 18, 2006
//

#ifndef JanEvent_hh
#define JanEvent_hh

#include <iostream>
using namespace std;

class JanEvent {
public:
  JanEvent();

  char* header();
  char* triggerData();
  unsigned short* bemcData();
  unsigned short* eemcData();
  unsigned short bemcIn() const;
  unsigned short eemcIn() const;

  void setHeader(const char*);
  void setTriggerData(char*);
  void setBemcData(unsigned short*);
  void setEemcData(unsigned short*);

  friend istream& operator>>(istream& in, JanEvent& event);
  friend ostream& operator<<(ostream& out, const JanEvent& event);

private:
  unsigned int mLength[4];
  char mHeader[100];
  char mTriggerData[20232];
  unsigned short mBemcData[4800];
  unsigned short mEemcData[960];
};

inline char* JanEvent::header() { return mHeader; }
inline char* JanEvent::triggerData() { return mTriggerData; }
inline unsigned short* JanEvent::bemcData() { return mBemcData; }
inline unsigned short* JanEvent::eemcData() { return mEemcData; }
inline unsigned short JanEvent::bemcIn() const { return mLength[2] > 0; }
inline unsigned short JanEvent::eemcIn() const { return mLength[3] > 0; }

#endif
