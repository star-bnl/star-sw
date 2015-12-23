TDataSet *CreateTable() { // Simulation !
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/rhic/.vertexSeed/vertexSeed Allocated rows: 1  Used rows: 1  Row size: 44 bytes
  //  Table: vertexSeed_st[0]--> vertexSeed_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_vertexSeed")) return 0;
  vertexSeed_st row;
  St_vertexSeed *tableSet = new St_vertexSeed("vertexSeed",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.x0	 =      0.000; // cm   : x intercept of x vs z line  ;
  row.dxdz	 =    0.00000; //      :    slope    of x vs z line  ;
  row.y0	 =      0.000; // cm   : y intercept of y vs z line  ;
  row.dydz	 =    0.00000; //      :    slope    of y vs z line  ;
  row.err_x0	 =      0.009; // cm   : error on x0  ;
  row.err_dxdz	 =      3e-05; //      : error on dxdz  ;
  row.err_y0	 =      0.003; // cm   : error on y0  ;
  row.err_dydz	 =      2e-05; //      : error on dydz  ;
  row.chisq_dof	 =       1.68; // chi square / dof of fit  ;
  row.weight	 =        100; // weight of seed to use in vertex-finding  ;
  row.stats	 =          0; // number of events used in seed-finding  ;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
