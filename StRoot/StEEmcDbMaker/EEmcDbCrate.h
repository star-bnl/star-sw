#ifndef EEMCDB_CRATE_H
#define EEMCDB_CRATE_H

#define CrateNameLen 16

class EEmcDbCrate {

 public:

  char name[CrateNameLen]; ///< crT1,... for towers, 06S1,... for mapmt
  int crID ; ///< logical crate ID
  int crIDswitch ; ///<  crate ID set by hardware switch
  int fiber ;///<  position of the crate in the .daq data stream, couting from 0
  int nch ;///< no. of valid channels for the crate
  int nHead ;///< no. of header words

  EEmcDbCrate();
  void clear();
  void print() const;
  void setName(char *text);
  int isEmpty() const;
  //  void exportAscii(FILE *fd) const;
  // int  importAscii(FILE *fd);
};

#endif 


