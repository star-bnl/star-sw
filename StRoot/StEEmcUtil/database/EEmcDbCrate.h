#ifndef EEMCDB_CRATE_H
#define EEMCDB_CRATE_H

#include "StMessMgr.h"

class EEmcDbCrate {
 private:
  enum {CrateNameLen=16 };

 public:

  char name[CrateNameLen]; ///< crT1,... for towers, 06S1,... for mapmt
  int crID ; ///< logical crate ID
  int crIDswitch ; ///<  crate ID set by hardware switch
  int fiber ;///<  position of the crate in the .daq data stream, couting from 0
  int nCh ;///< no. of valid channels for the crate
  int nHead ;///< no. of header words
  char type; ///< is 'T' for towers & 'S' for MAPMT
  int useIt; ///< flag to ignore data from misconfig/broken crate/box

  EEmcDbCrate();
  void clear();
  void print() const;
  void setName(char *text);
  void setAll(char *buff);
  int isEmpty() const;
  void exportAscii(FILE *fd) const;
  //  int importAscii(FILE *fd);
  ostream &print( ostream &out ) const;
};

ostream &operator<<(ostream &out, const EEmcDbCrate &crate );

#endif 


