#ifndef STDB_tpcTimeOffsets_HH
#define STDB_tpcTimeOffsets_HH

#include "StDbTableComponent.h"
#include "tpcTimeOffsets.h"

class typeAcceptor;

class StDb_tpcTimeOffsets : public StDbTableComponent {

private:

tpcTimeOffsets* mstruct;//!

public: 

  StDb_tpcTimeOffsets(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcTimeOffsets(StDb_tpcTimeOffsets& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcTimeOffsets *a = c.getTable();
                                     if(a)mstruct=new tpcTimeOffsets(*a);};

  virtual ~StDb_tpcTimeOffsets() { if(mstruct)delete mstruct;}

  void addTable(tpcTimeOffsets* s){mstruct = s;};
  tpcTimeOffsets* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcTimeOffsets(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcTimeOffsets,1)

};


#endif 
