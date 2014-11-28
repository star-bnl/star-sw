#ifndef slowcontrolDataSet_h
#define slowcontrolDataSet_h
#include "St_DataSet.h"

class slowcontrolDataSet : public St_DataSet {
 public:
  slowcontrolDataSet(const Char_t *Name="SlowControl") : St_DataSet(Name) {}
  virtual ~slowcontrolDataSet(){}

    double driftVelocity;
    double driftVoltage;
    double innerSectorAnodeVoltage;
    double innerSectorGatingGridV;
    double outerSectorAnodeVoltage;
    double outerSectorGatingGridV;
    double innerSectorGasGain;
    double innerSectorGasGainVzero;
    double innerSectorGasGainb;
    double outerSectorGasGain;
    double outerSectorGasGainVzero;
    double outerSectorGasGainb; 
    double hallPressure;
    double hallTemperature;
    ClassDef(slowcontrolDataSet,1) //
};
#endif
