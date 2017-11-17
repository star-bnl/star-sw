#ifndef TPC_GAS_H
#define TPC_GAS_H
//:Description: Gas measures
//:Synonyms::::
//:Source:
//:Update:
//:Update frequncy:
//:Reminder:
//:Recall frequency:
//:Size of Data:
//:Pointer to data:  Conditions_tpc/tpcGas:
struct tpcGas {

  //  type varnam;    //Units : Comments
  float barometricPressure;        // mbar : TPC-PTB
  float inputTPCGasPressure;       // mbar : TPC-PT8
  float nitrogenPressure;          // mbar : TPC-PI15
  float gasPressureDiff;           // mbar : TPC-PT7

  float inputGasTemperature;       // degrees C
  float outputGasTemperature;      // degrees C

  // Gas input values:
  float flowRateArgon1;            // liters/min : TPC-FM4
  float flowRateArgon2;            // liters/min : TPC-FM4
  float flowRateMethane;           // liters/min : TPC-FM1
  float percentMethaneIn;          // percent : TPC-CH4M3
  float ppmOxygenIn;               // ppm : TPC-O2M1

  // Gas exhaust values:
  float flowRateExhaust;           // liters/min : TPC-PT11
  float percentMethaneOut;         // ppm : TPC-M4
  float ppmWaterOut;               // ppm : TPC-M2
  float ppmOxygenOut;              // scf/hr : TPC-O2M1
  float flowRateRecirculation;     // liters/min : TPC-F17

};
#endif
