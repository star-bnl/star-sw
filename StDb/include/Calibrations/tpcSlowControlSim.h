#ifndef TPC_SlowControlSim_H
#define TPC_SlowControlSim_H
//:Description: 
//:Synonyms::::
//:Source: $STAR/StDb/params/tpc/trspars/Trs*.C
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
struct tpcSlowControlSim {

  //  type varnam;    //Units : Comments
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
 
};

#endif
