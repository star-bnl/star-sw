//:Description: 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data:  TPCDimensions.time:
struct tpc_dimensions {

  //  type varnam;    //Units : Comments

    int   numberOfSectors;

    float tpcInnerRadius;
    float tpcOuterRadius;
    float tpcTotalLength;

    float wheelInnerRadius;
    float wheelOuterRadius;
    float wheelThickness;

    float senseGasOuterRadius;
    float tpeaThickness;
   
    float cathodeInnerRadius;
    float cathodeOuterRadius;
    float cathodeThickness;
};

