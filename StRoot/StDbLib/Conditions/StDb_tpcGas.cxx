#include "StDb_tpcGas.h"

ClassImp(StDb_tpcGas)
//_______________________________________________________
void StDb_tpcGas::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcGas;

accept->pass("barometricPressure",mstruct->barometricPressure,sizeof(mstruct->barometricPressure));
accept->pass("inputTPCGasPressure",mstruct->inputTPCGasPressure,sizeof(mstruct->inputTPCGasPressure));
accept->pass("nitrogenPressure",mstruct->nitrogenPressure,sizeof(mstruct->nitrogenPressure));
accept->pass("gasPressureDiff",mstruct->gasPressureDiff,sizeof(mstruct->gasPressureDiff));
accept->pass("inputGasTemperature",mstruct->inputGasTemperature,sizeof(mstruct->inputGasTemperature));
accept->pass("outputGasTemperature",mstruct->outputGasTemperature,sizeof(mstruct->outputGasTemperature));
accept->pass("flowRateArgon1",mstruct->flowRateArgon1,sizeof(mstruct->flowRateArgon1));
accept->pass("flowRateArgon2",mstruct->flowRateArgon2,sizeof(mstruct->flowRateArgon2));
accept->pass("flowRateMethane",mstruct->flowRateMethane,sizeof(mstruct->flowRateMethane));
accept->pass("percentMethaneIn",mstruct->percentMethaneIn,sizeof(mstruct->percentMethaneIn));
accept->pass("ppmOxygenIn",mstruct->ppmOxygenIn,sizeof(mstruct->ppmOxygenIn));
accept->pass("flowRateExhaust",mstruct->flowRateExhaust,sizeof(mstruct->flowRateExhaust));
accept->pass("percentMethaneOut",mstruct->percentMethaneOut,sizeof(mstruct->percentMethaneOut));
accept->pass("ppmWaterOut",mstruct->ppmWaterOut,sizeof(mstruct->ppmWaterOut));
accept->pass("ppmOxygenOut",mstruct->ppmOxygenOut,sizeof(mstruct->ppmOxygenOut));
accept->pass("flowRateRecirculation",mstruct->flowRateRecirculation,sizeof(mstruct->flowRateRecirculation));

}

