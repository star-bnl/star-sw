TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// sls_ctrl Allocated rows: 1  Used rows: 1  Row size: 80 bytes
//  Table: sls_ctrl_st[0]--> sls_ctrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sls_ctrl")) return 0;
sls_ctrl_st row;
St_sls_ctrl *tableSet = new St_sls_ctrl("sls_ctrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.NElectronInAMip	 =      22500; // in electron unit;
    row.ADCDynamic	 =         20; // in mip unit;
    row.A128Dynamic	 =         12; // in mip unit;
    row.NBitEncoding	 =         10; // number od adc bit for encoding;
    row.NStripInACluster	 =          4; // ;
    row.PairCreationEnergy	 =    3.6e-09; // energy to create a e+e-pair(GeV);
    row.ParDiffP	 =    0.00123; // ;
    row.ParDiffN	 =    0.00094; // ;
    row.ParIndRightP	 =      0.021; // ;
    row.ParIndRightN	 =      0.026; // ;
    row.ParIndLeftP	 =      0.013; // ;
    row.ParIndLeftN	 =       0.01; // ;
    row.DAQCutValue	 =          3; // in "sigma unit";
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
