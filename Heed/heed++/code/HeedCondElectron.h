#ifndef HEEDCONDELECTRON
#define HEEDCONDELECTRON

#include "heed++/code/HeedDeltaElectron.h"
#include "wcpplib/safetl/BlkArr.h"

/*
Conduction electrons deposited in gas. 
Usually these are electron-ion pairs created by the
delta-electron.
But the delta-electron is itself converted in conduction electron
at the end of its route. In this case the ion may be located somewhere else.
To reduce the computer espenses, the position of conduction electron
is determined only in the local coordinate system, that is in the
most deep volume.

To make the coduction electrons generated, the volume
must be derived from class SensitiveVolume.

2003, I. Smirnov

*/





class HeedCondElectron
{public:
  //point pt;    // in the first system from tid system
  point ptloc;    // in the local system, the last system from tid
  // Time
  double time;
  //manip_absvol_treeid tid;
  //PassivePtr< HeedDeltaElectron > parent_de;  // reference to parent  
  HeedCondElectron(void) {;}
  HeedCondElectron(point fptloc, double ftime):
    ptloc(fptloc), time(ftime) {;}
  //HeedCondElectron(point fpt, point fptloc, manip_absvol_treeid ftid,
  //		   PassivePtr< HeedDeltaElectron > fparent_de):
  // pt(fpt), ptloc(fptloc), tid(ftid), parent_de(fparent_de) {;}
  virtual void print(std::ostream& file, int l) const ;
};


//extern AbsList< HeedCondElectron > conduction_electron_bank;  
//extern BlkArr< HeedCondElectron > conduction_electron_bank; 

class SensitiveVolume
{public:
  BlkArr< HeedCondElectron > conduction_electron_bank;
  SensitiveVolume(void) {;}

};


#endif
