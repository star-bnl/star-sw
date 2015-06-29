TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/sst/.sstSlsCtrl/sstSlsCtrl Allocated rows: 1  Used rows: 1  Row size: 84 bytes
//  Table: sstSlsCtrl_st[0]--> sstSlsCtrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_sstSlsCtrl")) return 0;
sstSlsCtrl_st row;
St_sstSlsCtrl *tableSet = new St_sstSlsCtrl("sstSlsCtrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.nElectronInAMip	 =      22500; // 22500 in electron unit ;
    row.adcDynamic	 =         20; // 20 in mip unit ;
    row.a128Dynamic	 =         12; // 12 in mip unit ;
    row.nbitEncoding	 =         10; // 10 number of adc bit for encoding ;
    row.nstripInACluster	 =          4; // 4 ;
    row.daqCutValue	 =          3; // 3 ;
    row.rmsToNoise	 =         16; // 16 ;
    row.pairCreationEnergy	 =    3.6e-09; // 3.6e-09 energy to create a e+e-pair(GeV) ;
    row.parDiffP	 =    0.00123; // 0.00123 ;
    row.parDiffN	 =    0.00094; // 0.00094 ;
    row.parIndRightP	 =      0.021; // 0.021 ;
    row.parIndRightN	 =      0.026; // 0.026 ;
    row.parIndLeftP	 =      0.013; // 0.013 ;
    row.parIndLeftN	 =       0.01; // 0.01 ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
