#ifndef STDB_tpcGas_HH
#define STDB_tpcGas_HH

#include "StDbTableComponent.h"
#include "tpcGas.h"

class typeAcceptor;

class StDb_tpcGas : public StDbTableComponent {

private:

tpcGas* mstruct;//!

public: 

  StDb_tpcGas(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcGas(StDb_tpcGas& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcGas *a = c.getTable();
                                     if(a)mstruct=new tpcGas(*a);};

  virtual ~StDb_tpcGas() { if(mstruct)delete mstruct;}

  void addTable(tpcGas* s){mstruct = s;};
  tpcGas* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcGas(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcGas,1)

};


#endif 
