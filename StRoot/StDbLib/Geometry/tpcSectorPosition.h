//:Description: 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data:  sectorPosition.time:
struct tpcSectorPosition {

  //  type varnam;    //Units : Comments

  float innerSectorLocalxShift;     // cm : shift in local x coord.
  float innerSectorLocalyShift;     // cm : shift in local y coord.
  float innerSectorRotationAngle;   // degrees : rotation
  float innerSectorCovMatrix;       //

  float outerSectorLocalxShift;     // cm : shift in local x coord.
  float outerSectorLocalyShift;     // cm : shift in local y coord.
  float outerSectorRotationAngle;   // degrees : rotation
  float outerSectorCovMatrix;       //



};
