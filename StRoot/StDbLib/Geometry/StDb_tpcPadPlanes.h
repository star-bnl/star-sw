#ifndef STDB_tpcPadPlanes_HH
#define STDB_tpcPadPlanes_HH

#include "StDbTableComponent.h"
#include "tpcPadPlanes.h"

class typeAcceptor;

class StDb_tpcPadPlanes : public StDbTableComponent {

private:

tpcPadPlanes* mstruct;//!

public: 

  StDb_tpcPadPlanes(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcPadPlanes(StDb_tpcPadPlanes& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcPadPlanes *a = c.getTable();
                                     if(a)mstruct=new tpcPadPlanes(*a);};

  virtual ~StDb_tpcPadPlanes() { if(mstruct)delete mstruct;}

  void addTable(tpcPadPlanes* s){mstruct = s;};
  tpcPadPlanes* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcPadPlanes(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcPadPlanes,1)

};


#endif 
