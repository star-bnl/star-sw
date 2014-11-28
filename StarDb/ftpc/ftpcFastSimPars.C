St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcFastSimPars Allocated rows: 1  Used rows: 1  Row size: 44 bytes
//  Table: ftpcFastSimPars_st[0]--> ftpcFastSimPars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcFastSimPars")) return 0;
ftpcFastSimPars_st row;
St_ftpcFastSimPars *tableSet = new St_ftpcFastSimPars("ftpcFastSimPars",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.ftpcWestGeantVolumeId         =       100; // D= GEANT volume id for FTPC west;
    row.ftpcEastGeantVolumeId         =       200; // D= GEANT volume id for FTPC east;
    row.unfoldedClusterFlag           =         1; // D= unfolded cluster bit number;
    row.badShapeClusterFlag           =         8; // D= bad shape cluster bit number;
    row.mergedClusterFlag             =      1000; // D= merged cluster bit number;
    row.numberOfPadsDedxSmearing      =         4; // D= number of pads used for de/dx smearing;
    row.numberOfBinsDedxSmearing      =         3; // D= number of bins used for de/dx smearing;  
    row.radiusTolerance               =      0.25; // D= tolerance used for radius check;  
    row.sigmaSpacingFactor            =       2.5; // D= sigma spacing factor;
    row.adcConversionFactor           =  800000.0; // D= convert GEANT energy loss to adc counts;
    row.clusterChargeConversionFactor =        6.; // D= convert adc counts to charge;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
