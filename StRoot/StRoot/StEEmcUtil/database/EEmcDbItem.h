#ifndef EEMCDBITEM_H
#define EEMCDBITEM_H

#include "StMessMgr.h"
#if !defined(ST_NO_NAMESPACES)
using std::ostream;
#endif


#define StEEmcNameLen 16  // to avoid dependency on "cstructs/eemcConstDB.hh"
//class FILE;

class EEmcDbItem {

 public:
  int key; // unique internal ID in form 0 to EEindexMax-1
  //  of every active EEMC pixel
  //  see StEEmcUtil/EEfeeRaw/EEname2Index.h for definition

  char name[StEEmcNameLen]; ///< ASCII name of the channel, see Readme 
  char tube[StEEmcNameLen]; ///< name of PMT or MAPMT pixel
  // for towers/pre/post use (sec,sub,eta)
  // for SMD use sec,plane,strip)
  int sec,eta; // 1-12, 1-12
  char sub,plane; //A-E, U-V
  int strip; // 1-288 

  int crate, chan; ///< hardware channel
  float gain; 
  float ped,thr,sigPed; // in ADC channals
  unsigned  stat; // bits, see eemcConstDB.hh for definitions
  unsigned  fail; // bits, see eemcConstDB.hh for definitions

  EEmcDbItem();
  void clear();
  void print() const;
  void setName(char *text);
  void setTube(char *text);
  void setDefaultTube(int cr_off);
  int mapmtId()const;  
  bool isEmpty() const;
  bool isSMD() const { return (plane=='U' || plane=='V');}
  bool isTower() const;
  void exportAscii(FILE *fd) const;
  int  importAscii(FILE *fd);

  ostream &print( ostream &out ) const;

};

ostream &operator<<(ostream &out, const EEmcDbItem &item );

#endif 


