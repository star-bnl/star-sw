#ifndef EVENTMIXER_H
#define EVENTMIXER_H

#include <TObject.h>
#include <TClonesArray.h>
#include <TH2F.h>

class MyEvent;

class AnaCuts;

class EventMixer : public TObject{
 protected:
  
  unsigned int fNmixed;

  TH2F *h_minvMB_mixed;

  TClonesArray *ev_array;

  AnaCuts *cuts;

 public:
  EventMixer(const char*);
  ~EventMixer();

  void addEvent(MyEvent*);
  void mix();
  TH2F *getMinvMB(){return h_minvMB_mixed;}  
    
  ClassDef(EventMixer,1)
};

#endif
