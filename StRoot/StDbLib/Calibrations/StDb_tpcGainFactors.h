#ifndef STDB_tpcGainFactors_HH
#define STDB_tpcGainFactors_HH

#include "StDbTableComponent.h"
#include "tpcGainFactors.h"

class typeAcceptor;

class StDb_tpcGainFactors : public StDbTableComponent {

private:

tpcGainFactors* mstruct;//!

public: 

  StDb_tpcGainFactors(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcGainFactors(StDb_tpcGainFactors& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcGainFactors *a = c.getTable();
                                     if(a)mstruct=new tpcGainFactors(*a);};

  virtual ~StDb_tpcGainFactors() { if(mstruct)delete mstruct;}

  void addTable(tpcGainFactors* s){mstruct = s;};
  tpcGainFactors* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcGainFactors(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcGainFactors,1)

};


#endif 
