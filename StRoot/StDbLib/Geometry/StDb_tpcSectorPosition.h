#ifndef STDB_tpcSectorPosition_HH
#define STDB_tpcSectorPosition_HH

#include "StDbTableComponent.h"
#include "tpcSectorPosition.h"

class typeAcceptor;

class StDb_tpcSectorPosition : public StDbTableComponent {

private:

tpcSectorPosition* mstruct;//!

public: 

  StDb_tpcSectorPosition(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcSectorPosition(StDb_tpcSectorPosition& c) : StDbTableComponent(c) {
                                     tpcSectorPosition *a = c.getTable();
                                     if(a)mstruct=new tpcSectorPosition(*a);};

  virtual ~StDb_tpcSectorPosition() { if(mstruct)delete mstruct;}

  void addTable(tpcSectorPosition* s){mstruct = s;};
  tpcSectorPosition* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcSectorPosition(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcSectorPosition,1)

};


#endif 
