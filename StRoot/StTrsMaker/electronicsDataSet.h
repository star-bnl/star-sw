#ifndef electronicsDataSet_h
#define electronicsDataSet_h
#include "St_DataSet.h"
class electronicsDataSet : public St_DataSet {
 public:
  electronicsDataSet(const Char_t *Name="Electronics") : St_DataSet(Name) {}
  virtual ~electronicsDataSet(){}

    double nominalGain;
    double samplingFrequency;
    double tZero;
    double adcCharge;
    double adcConversion;
    int numberOfTimeBins;
    double averagePedestal;
    double shapingTime;
    double tau;
    ClassDef(electronicsDataSet,1)
};
#endif

