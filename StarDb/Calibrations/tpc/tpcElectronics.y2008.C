TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcElectronics")) return 0;
  tpcElectronics_st row;
  St_tpcElectronics *tableSet = new St_tpcElectronics("tpcElectronics",1);
  memset(&row,0,tableSet->GetRowSize());
  row.numberOfTimeBins	 =        512; // ;
  row.nominalGain	 =     82.065; // mV/fC  ;
  row.samplingFrequency	 =    9.43437; // MHz  ;
  row.tZero	                =   0; // us (microseconds)  ;
  row.adcCharge	         =       0.12; // fC/adc count  ;
  row.adcConversion	 =          2; // mV/adc count  ;
  row.averagePedestal	 =         50; // adc counts  ;
  row.shapingTime	 =        180; // ns  ;
  row.tau	         =         55; // ns  ;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
