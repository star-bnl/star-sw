#ifndef DAUBACKGROUND_H
#define DAUBACKGROUND_H

#include <TObject.h>

class AnaCuts;

class dAuBackground : public TObject{

 protected:
    AnaCuts *cuts;

 public:
  dAuBackground();
  ~dAuBackground();
  void run(const char*);
  void runSim(const char*);
  ClassDef(dAuBackground,1)
};

#endif
