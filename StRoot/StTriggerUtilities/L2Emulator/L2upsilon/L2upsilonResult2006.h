//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// March 15, 2006
//
// L2Result must not exceed 6 words
//

#ifndef L2_UPSILON_RESULT_2006_H
#define L2_UPSILON_RESULT_2006_H

struct L2upsilonResult2006 {
  unsigned short numberOfL0SeedTowers;
  unsigned short numberOfL2SeedTowers;
  float invariantMass;
  unsigned short eventsSeen;
  unsigned short eventsAccepted;
  unsigned int processingTime;
  float energyOfL0Cluster;
  float energyOfL2Cluster;
};

#endif
