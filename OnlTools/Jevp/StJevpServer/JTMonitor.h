#ifndef _JTMONITOR_
#define _JTMONITOR_

#include <TSocket.h>
#include <TList.h>

#define _JT_NSOCK_ 100

class JTMonitor {
 public:

  TList *sList;

  JTMonitor();
  virtual ~JTMonitor();

  void Add(TSocket *s);
  void Remove(TSocket *s);
  TSocket *Select(int delay);
};

#endif
