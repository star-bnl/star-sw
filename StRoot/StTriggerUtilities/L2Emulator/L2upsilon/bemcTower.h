//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// March 15, 2006
//

#ifndef BEMC_TOWER_H
#define BEMC_TOWER_H

struct BemcTower {
  int   softId;
  int   crate;
  int   crateSeq;
  float gain;
  float x;
  float y;
  float z;
  float eta;
  float phi;
  float pedestal;
  int   stat;
  int   fail;
  int   numberOfNeighbors;
  int   neighbor[8];
};

#endif
