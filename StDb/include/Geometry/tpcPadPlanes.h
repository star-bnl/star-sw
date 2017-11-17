#ifndef TPC_PADPLANES_H
#define TPC_PADPLANES_H
//:Description: geometrical characteristic of tpc's padplanes
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data: Geometry_tpc/tpcPadPlanes
struct tpcPadPlanes {
  //  type varnam;    //Units : Comments


    int    padRows;
    int    innerPadRows;
    int    innerPadRows48;
    int    innerPadRows52;
    int    outerPadRows;
    int    superInnerPadRows;
    int    superOuterPadRows;

    double innerSectorPadWidth;
    double innerSectorPadLength;
    double innerSectorPadPitch;
    double innerSectorRowPitch1;
    double innerSectorRowPitch2;
    double firstPadRow;
    double firstOuterSectorPadRow;
    double lastOuterSectorPadRow;
    double firstRowWidth;
    double lastRowWidth;
    double outerSectorPadWidth;
    double outerSectorPadLength;
    double outerSectorPadPitch;
    double outerSectorRowPitch;
    double outerSectorLength;
    double ioSectorSeparation;
    double innerSectorEdge;
    double outerSectorEdge;
 
    int    innerPadsPerRow[13];
    int    outerPadsPerRow[32];
    double innerRowRadii[13];
    double outerRowRadii[32];

};
#endif
