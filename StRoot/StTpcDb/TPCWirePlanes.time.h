//:Description: 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data:  TPCWires.time:
struct tpc_wireplanes {

  //  type varnam;    //Units : Comments
    float  anodeWireRadius;
    float  frischGridWireRadius;
    float  gatingGridWireRadius;
    
    float  anodeWirePitch;
    float  frischGridWirePitch;
    float  gatingGridWirePitch;
    
    float  innerSectorAnodeWirePadPlaneSeparation;
    float  innerSectorFrischGridPadPlaneSeparation;
    float  innerSectorGatingGridPadPlaneSeparation;

    float  outerSectorAnodeWirePadPlaneSeparation;
    float  outerSectorFrischGridPadPlaneSeparation;
    float  outerSectorGatingGridPadPlaneSeparation;

    int    numberOfInnerSectorAnodeWires;
    int    numberOfInnerSectorFrischGridWires;
    int    numberOfInnerSectorGatingGridWires;
    float  firstInnerSectorAnodeWire;
    float  firstInnerSectorFrischGridWire;
    float  firstInnerSectorGatingGridWire;
    float  lastInnerSectorAnodeWire;

    int    numberOfOuterSectorAnodeWires;
    int    numberOfOuterSectorFrischGridWires;
    int    numberOfOuterSectorGatingGridWires;
    float  firstOuterSectorAnodeWire;
    float  firstOuterSectorFrischGridWire;
    float  firstOuterSectorGatingGridWire;
    float  lastOuterSectorAnodeWire;


};
