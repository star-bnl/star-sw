St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcSlowSimPars Allocated rows: 1  Used rows: 1  Row size: 44 bytes
//  Table: ftpcSlowSimPars_st[0]--> ftpcSlowSimPars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcSlowSimPars")) return 0;
ftpcSlowSimPars_st row;
St_ftpcSlowSimPars *tableSet = new St_ftpcSlowSimPars("ftpcSlowSimPars",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.randomNumberGenerator   =       0; // D=  select random number generator;
    row.zeroSuppressThreshold   =       5; // D=  minimum ADC value    ;
    row.numSlowSimGridPoints    =    4000; // D=  number of drift simu grid points    ;
    row.maxAdc                  =     255; // D=  maximum ADC value    ;
    row.numGaussIntSteps        =      21; // D=  number of gauss integration steps    ;
    row.diffusionCoarseness     =      10; // D=  diffusion coarseness    ;
    row.adcConversion           =  2100.0; // D=  electrons/adc value    ;
    row.chamberCathodeVoltage   =  -10000; // D=  drift cathode voltage [V]    ;
    row.sigmaPadResponseFuntion =    1500; // D=  pad-response-function sigma [um]    ;
    row.shaperTime	        =     507; // D=  shaper time FWHM [ns] -> 350ns measured + 45% to match dAu data  ;
    row.slowSimPressure         = 1013.25; // D = atmospheric pressure used;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
