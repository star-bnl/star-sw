#ifndef STDB_tpcWirePlanes_HH
#define STDB_tpcWirePlanes_HH

#include "StDbTableComponent.h"
#include "tpcWirePlanes.h"

class typeAcceptor;

class StDb_tpcWirePlanes : public StDbTableComponent {

private:

tpcWirePlanes* mstruct;//!

public: 

  StDb_tpcWirePlanes(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcWirePlanes(StDb_tpcWirePlanes& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcWirePlanes *a = c.getTable();
                                     if(a)mstruct=new tpcWirePlanes(*a);};

  virtual ~StDb_tpcWirePlanes() { if(mstruct)delete mstruct;}

  void addTable(tpcWirePlanes* s){mstruct = s;};
  tpcWirePlanes* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcWirePlanes(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcWirePlanes,1)

};


#endif 
