#ifndef TPC_SECTORPOSITION_H
#define TPC_SECTORPOSITION_H
//:Description: position measures of a tpc-sector 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Geometry_tpc/Sector_##/tpcSectorPosition
struct tpcSectorPosition {

  //  type varnam;    //Units : Comments

  float innerSectorLocalxShift;     // cm : shift in local x coord.
  float innerSectorLocalyShift;     // cm : shift in local y coord.
  float innerSectorRotationAngle;   // degrees : rotation
  float innerSectorCovMatrix;       // 0

  float outerSectorLocalxShift;     // cm : shift in local x coord.
  float outerSectorLocalyShift;     // cm : shift in local y coord.
  float outerSectorRotationAngle;   // degrees : rotation
  float outerSectorCovMatrix;       // 0



};
#endif
