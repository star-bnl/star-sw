#ifndef STDB_tpcDimensions_HH
#define STDB_tpcDimensions_HH

#include "StDbTableComponent.h"
#include "tpcDimensions.h"

class typeAcceptor;

class StDb_tpcDimensions : public StDbTableComponent {

private:

tpcDimensions* mstruct;//!

public: 

  StDb_tpcDimensions(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcDimensions(StDb_tpcDimensions& c) : StDbTableComponent(c) {
                                     tpcDimensions *a = c.getTable();
                                     if(a)mstruct=new tpcDimensions(*a);};

  virtual ~StDb_tpcDimensions() { if(mstruct)delete mstruct;}

  void addTable(tpcDimensions* s){mstruct = s;};
  tpcDimensions* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcDimensions(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcDimensions,1)

};


#endif 
