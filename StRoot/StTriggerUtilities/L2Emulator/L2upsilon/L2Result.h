//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// March 15, 2006
//
// L2Result must not exceed 6 words
//

#ifndef L2_RESULT_H
#define L2_RESULT_H

struct L2Result {
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
