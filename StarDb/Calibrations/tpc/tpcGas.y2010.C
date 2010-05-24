TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcGas")) return 0;
  St_tpcGas *tableSet = new St_tpcGas("tpcGas",1);
  tpcGas_st row = {
    1018.2, // barometricPressure     
    1.99,   // inputTPCGasPressure    
    1.13,   // nitrogenPressure       
    0.73,   // gasPressureDiff        
    297.66, // inputGasTemperature    
    297.87, // outputGasTemperature   
    14.98,  // flowRateArgon1         
    0.44,   // flowRateArgon2         
    1.36,   // flowRateMethane        
    10.17,  // percentMethaneIn       
    27.39,  // ppmOxygenIn            
    11.52,  // flowRateExhaust        
    10.21,  // percentMethaneOut      
    7.86,   // ppmWaterOut            
    -0.27,  // ppmOxygenOut           
    539.55  // flowRateRecirculation  
  };
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
