#ifndef STDB_tpcVoltages_HH
#define STDB_tpcVoltages_HH

#include "StDbTableComponent.h"
#include "tpcVoltages.h"

class typeAcceptor;

class StDb_tpcVoltages : public StDbTableComponent {

private:

tpcVoltages* mstruct;//!

public: 

  StDb_tpcVoltages(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcVoltages(StDb_tpcVoltages& c) : StDbTableComponent(c) {
                                     tpcVoltages *a = c.getTable();
                                     if(a)mstruct=new tpcVoltages(*a);};

  virtual ~StDb_tpcVoltages() { if(mstruct)delete mstruct;}

  void addTable(tpcVoltages* s){mstruct = s;};
  tpcVoltages* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcVoltages(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcVoltages,1)

};


#endif 
