#ifndef STDB_tpcDriftVelocity_HH
#define STDB_tpcDriftVelocity_HH

#include "StDbTableComponent.h"
#include "tpcDriftVelocity.h"

class typeAcceptor;

class StDb_tpcDriftVelocity : public StDbTableComponent {

private:

tpcDriftVelocity* mstruct;//!

public: 

  StDb_tpcDriftVelocity(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcDriftVelocity(StDb_tpcDriftVelocity& c) : StDbTableComponent(c) {
                                     tpcDriftVelocity *a = c.getTable();
                                     if(a)mstruct=new tpcDriftVelocity(*a);};

  virtual ~StDb_tpcDriftVelocity() { if(mstruct)delete mstruct;}

  void addTable(tpcDriftVelocity* s){mstruct = s;};
  tpcDriftVelocity* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcDriftVelocity(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcDriftVelocity,1)

};


#endif 
