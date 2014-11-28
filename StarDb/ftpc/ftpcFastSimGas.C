St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcFastSimGas Allocated rows: 1  Used rows: 1  Row size: 100 bytes
//  Table: ftpcFastSimGas_st[0]--> ftpcFastSimGas_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcFastSimGas")) return 0;
ftpcFastSimGas_st row;
St_ftpcFastSimGas *tableSet = new St_ftpcFastSimGas("ftpcFastSimGas",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.orderOfFastEstimates          =         4; // D= number of parameters;
    row.driftVelocityEstimates[0]     =       4.6; // D= parameters of a P3-fit of drift-velocity;
    row.driftVelocityEstimates[1]     =    -0.418; // D= ;
    row.driftVelocityEstimates[2]     =    0.0149; // D= ;
    row.driftVelocityEstimates[3]     = -0.000182; // D= ;
    row.driftTimeEstimates[0]         =        31; // D= parameters of a P3-fit of drift-time;
    row.driftTimeEstimates[1]         =     0.125; // D= ;
    row.driftTimeEstimates[2]         =   -0.0365; // D= ;
    row.driftTimeEstimates[3]         =  3.82e-05; // D= ;
    row.sigmaRadialEstimates[0]       =       800; // D= parameters of a P3-fit of sigma(rad);
    row.sigmaRadialEstimates[1]       =         0; // D= ;
    row.sigmaRadialEstimates[2]       =         0; // D= ;
    row.sigmaRadialEstimates[3]       =         0; // D= ;
    row.sigmaAzimuthalEstimates[0]    =      2000; // D= parameters of a P3-fit of sigma(azi);
    row.sigmaAzimuthalEstimates[1]    =         0; // D= ;
    row.sigmaAzimuthalEstimates[2]    =         0; // D= ;
    row.sigmaAzimuthalEstimates[3]    =         0; // D= ;
    row.errorRadialEstimates[0]       =        90; // D= parameters of a P3-fit of radial error;
    row.errorRadialEstimates[1]       =         0; // D= ;
    row.errorRadialEstimates[2]       =         0; // D= ;
    row.errorRadialEstimates[3]       =         0; // D= ;
    row.errorAzimuthalEstimates[0]    =        50; // D= parameters of a P3-fit of azimuthal error;
    row.errorAzimuthalEstimates[1]    =         0; // D= ;
    row.errorAzimuthalEstimates[2]    =         0; // D= ;
    row.errorAzimuthalEstimates[3]    =         0; // D= ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
