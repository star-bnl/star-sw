//:Description: 
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data:  TPCPadplanes.time:
struct tpc_padplanes {
  //  type varnam;    //Units : Comments


    int    padRows;
    int    innerPadRows;
    int    innerPadRows48;
    int    innerPadRows52;
    int    outerPadRows;

    float innerSectorPadWidth;
    float innerSectorPadLength;
    float innerSectorPadPitch;
    float innerSectorRowPitch1;
    float innerSectorRowPitch2;
    float firstPadRow;
    float firstOuterSectorPadRow;
    float lastOuterSectorPadRow;
    float firstRowWidth;
    float lastRowWidth;
    float outerSectorPadWidth;
    float outerSectorPadLength;
    float outerSectorPadPitch;
    float outerSectorRowPitch;
    float outerSectorLength;
    float ioSectorSeparation;
    float innerSectorEdge;
    float outerSectorEdge;
 
    int    innerPadsPerRow[13];
    int    outerPadsPerRow[32];
    float innerRowRadii[13];
    float outerRowRadii[32];

};
