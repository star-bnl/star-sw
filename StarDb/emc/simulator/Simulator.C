TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// Simulator Allocated rows: 4  Used rows: 4  Row size: 32 bytes
//  Table: controlEmcPmtSimulator_st[0]--> controlEmcPmtSimulator_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_controlEmcPmtSimulator")) return 0;
controlEmcPmtSimulator_st row;
St_controlEmcPmtSimulator *tableSet = new St_controlEmcPmtSimulator("Simulator",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.mode	 =          3; // 0->no conversion; 1->simple conversion; 2-> precise simulation ;
    row.maxAdc	 =       3500; // ADC bits ;
    row.maxEnergy	 =         60; // Max energy in BEMC tower (~60 GeV/c)            ;
    row.sfCoeff[0]	 =      14.69; // ;
    row.sfCoeff[1]	 =    -0.1022;
    row.sfCoeff[2]	 =     0.7484;
    row.depMip	 =     0.0198; // MIP deposit energy in Sc.layers at zero angle   ;
    row.npheMip	 =         63; // number of PHE for MIP at zero angle         ;
    row.typeOfPmt	 =          1; // ;
    row.pedDistribution  = 0;   //  must be 0 for embedding 
    row.pedMean          = 40.; // for 2002 year
    row.pedRMS           = 1.5;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.mode	 =          3; // 0->no conversion; 1->simple conversion; 2-> precise simulation ;
    row.maxAdc	 =        220; // ADC bits ;
    row.maxEnergy	 =          1; // Max energy in BPRS tower (~60 GeV/c)            ;
    row.sfCoeff[0]	 =      14.69; // ;
    row.sfCoeff[1]	 =    -0.1022;
    row.sfCoeff[2]	 =     0.7484;
    row.depMip	 =      0.002; // MIP deposit energy in Sc.layers at zero angle   ;
    row.npheMip	 =          6; // number of PHE for MIP at zero angle         ;
    row.typeOfPmt	 =          1; // ;
    row.pedDistribution  = 0;
    row.pedMean          = 10.; // for 2001 year (the same as for bemc)
    row.pedRMS           = 2.;
tableSet->AddAt(&row,1);
memset(&row,0,tableSet->GetRowSize());
    row.mode	 =          1; // 0->no conversion; 1->simple conversion; 2-> precise simulation ;
    row.maxAdc	 =        900; // ADC bits ;
    row.maxEnergy	 =         25; // Max energy in SMD strip (~25 GeV/c)            ;
    row.sfCoeff[0]	 =     118500; // ;
    row.sfCoeff[1]	 =     -32920;
    row.sfCoeff[2]	 =      31130;
    row.depMip	         = 0; // Three lines unused for SMD
    row.npheMip	         = 0; // ;
    row.typeOfPmt	 = 0; // ;
    row.pedDistribution  = 0;
    row.pedMean          = 40.; // for 2001 year
    row.pedRMS           = 4.;
tableSet->AddAt(&row,2);
memset(&row,0,tableSet->GetRowSize());
    row.mode	 =          1; // 0->no conversion; 1->simple conversion; 2-> precise simulation ;
    row.maxAdc	 =        900; // ADC bits ;
    row.maxEnergy	 =         25; // Max energy in SMD strip (~25 GeV/c);
    row.sfCoeff[0]	 =     126000; // ;
    row.sfCoeff[1]	 =     -13950;
    row.sfCoeff[2]	 =      19710;
    row.depMip	         =  0; // Three lines unused for SMD;
    row.npheMip	         =  0; // ;
    row.typeOfPmt	 =  0; // ;
    row.pedDistribution  = 0;
    row.pedMean          = 40.; // for 2001 year
    row.pedRMS           = 4.;
tableSet->AddAt(&row,3);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
