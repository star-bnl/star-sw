#ifndef EEMCDBITEM_H
#define EEMCDBITEM_H


#define StEEmcNameLen 16  // to avoid dependency on "cstructs/eemcConstDB.hh"

class EEmcDbItem {

 public:
  char name[StEEmcNameLen]; ///< ASCII name of the channel, see Readme 
  char tube[StEEmcNameLen]; ///< name of PMT or MAPMT pixel
  int crate, chan; ///< hardware channel
  float gain; 
  float ped,thr; // in ADC channals
  int sec,eta;
  char sub;
  int strip; // for SMD instead of sub&eta
  unsigned short stat; // bits, see eemcConstDB.hh for definitions
  unsigned short fail; // bits, see eemcConstDB.hh for definitions

  EEmcDbItem();
  void clear();
  void print() const;
  void setName(char *text);
  void setTube(char *text);
  int isEmpty() const;
};

#endif 


