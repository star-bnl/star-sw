#ifndef STDB_dedxPidAmpDb_HH
#define STDB_dedxPidAmpDb_HH

#include "StDbTableComponent.h"
#include "dedxPidAmpDb.h"

class typeAcceptor;

class StDb_dedxPidAmpDb : public StDbTableComponent {

private:

dedxPidAmpDb* mstruct;//!

public: 

  StDb_dedxPidAmpDb(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_dedxPidAmpDb(StDb_dedxPidAmpDb& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     dedxPidAmpDb *a = c.getTable();
                                     if(a)mstruct=new dedxPidAmpDb(*a);};

  virtual ~StDb_dedxPidAmpDb() { if(mstruct)delete mstruct;}

  void addTable(dedxPidAmpDb* s){mstruct = s;};
  dedxPidAmpDb* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_dedxPidAmpDb(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_dedxPidAmpDb,1)

};


#endif 
