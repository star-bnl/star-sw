#ifndef STDB_tpcElectronics_HH
#define STDB_tpcElectronics_HH

#include "StDbTableComponent.h"
#include "tpcElectronics.h"

class typeAcceptor;

class StDb_tpcElectronics : public StDbTableComponent {

private:

tpcElectronics* mstruct;//!

public: 

  StDb_tpcElectronics(const char* name): StDbTableComponent(name),mstruct(0) {}; 
  StDb_tpcElectronics(StDb_tpcElectronics& c) : StDbTableComponent(c) {
                                     mstruct=0;
                                     tpcElectronics *a = c.getTable();
                                     if(a)mstruct=new tpcElectronics(*a);};

  virtual ~StDb_tpcElectronics() { if(mstruct)delete mstruct;}

  void addTable(tpcElectronics* s){mstruct = s;};
  tpcElectronics* getTable(){ return mstruct;};

  virtual StDbTableComponent* duplicate() { return new StDb_tpcElectronics(*this);};
  virtual void Streamer(typeAcceptor* accept);    // Stream Data

  ClassDef(StDb_tpcElectronics,1)

};


#endif 
