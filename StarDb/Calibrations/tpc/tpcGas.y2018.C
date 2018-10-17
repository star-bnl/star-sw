TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.tpcGas/tpcGas Allocated rows: 1  Used rows: 1  Row size: 64 bytes
//  Table: tpcGas_st[0]--> tpcGas_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcGas")) return 0;
tpcGas_st row;
St_tpcGas *tableSet = new St_tpcGas("tpcGas",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.barometricPressure	 =   1001.605; // ;
    row.inputTPCGasPressure	 =   1.955556; // ;
    row.nitrogenPressure	 =  0.8466667; // ;
    row.gasPressureDiff	 =   1.669444; // ;
    row.inputGasTemperature	 =   297.2567; // ;
    row.outputGasTemperature	 =   297.4806; // ;
    row.flowRateArgon1	 =   16.41778; // ;
    row.flowRateArgon2	 = -0.01833333; // ;
    row.flowRateMethane	 =   1.580556; // ;
    row.percentMethaneIn	 =   10.14333; // ;
    row.ppmOxygenIn	 =   7.242778; // ;
    row.flowRateExhaust	 =   13.72333; // ;
    row.percentMethaneOut	 =   9.961111; // ;
    row.ppmWaterOut	 =       8.86; // ;
    row.ppmOxygenOut	 =       0.01; // ;
    row.flowRateRecirculation	 =   538.7095; // ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
