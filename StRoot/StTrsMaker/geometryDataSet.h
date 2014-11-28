#ifndef geometryDataSet_h
#define geometryDataSet_h
#include "St_DataSet.h"
class geometryDataSet : public St_DataSet {
 public:
  geometryDataSet(const Char_t *Name="Geometry") : St_DataSet(Name) {}
  virtual ~geometryDataSet(){}
    int padRows;
    int innerPadRows;
    int innerPadRows48;
    int innerPadRows52;
    int outerPadRows;
    int timeBuckets;
    int sectors;
    int ifcRadius;
    int ofcRadius;
    int endCapZ;
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
    double frischGrid;
    double maximumDriftDistance;
    double innerSectorzOffSet;
    double outerSectorzOffSet;
    
    double anodeWirePitch;
    double frischGridWirePitch;
    double gateWirePitch;
    double anodeWireRadius;
    double frischGridWireRadius;
    double gateWireRadius;
    double iSAnodeWirePadPlaneSeparation;
    double iSFrischGridPadPlaneSeparation;
    double iSGatingGridPadPlaneSeparation;
    double oSAnodeWirePadPlaneSeparation;
    double oSFrischGridPadPlaneSeparation;
    double oSGatingGridPadPlaneSeparation;
    double firstInnerSectorAnodeWire;
    double lastInnerSectorAnodeWire;
    double numberOfInnerSectorAnodeWires;
    double firstOuterSectorAnodeWire;
    double lastOuterSectorAnodeWire;
    double numberOfOuterSectorAnodeWires;
    double innerSectorEdge;
    double outerSectorEdge;
    int padsInRow[45];
    /* DANGER HARD CODE  88 96 104 112 118 126 134 142 150 158 166 174 182 98 100 102 104 106 106 108 110 112 112 114 116 118 120 122 122 124 126 128 128 130 132 134 136 138 138 140 142 144 144 144 144*/
    ClassDef(geometryDataSet,1) //geometryDataSet class
};
#endif
