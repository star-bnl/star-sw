#ifndef TPC_DRIFTVELOCITY_H
#define TPC_DRIFTVELOCITY_H
//:Description: 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
struct tpcDriftVelocity {

  //  type varnam;    //Units : Comments

  float laserDriftVelocityEast;     // cm/us : from laser beam analysis
  float laserDriftVelocityWest;     // cm/us : from laser beam analysis
  float cathodeDriftVelocityEast;   // cm/us : from cathode emission
  float cathodeDriftVelocityWest;   // cm/us : from cathode emission
  

};

#endif
