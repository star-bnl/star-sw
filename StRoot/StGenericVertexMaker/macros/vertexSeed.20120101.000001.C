TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/rhic/.vertexSeed/vertexSeed Allocated rows: 1  Used rows: 1  Row size: 44 bytes
//  Table: vertexSeed_st[0]--> vertexSeed_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_vertexSeed")) return 0;
vertexSeed_st row;
St_vertexSeed *tableSet = new St_vertexSeed("vertexSeed",1);
//  my run is Rrun_fix
memset(&row,0,tableSet->GetRowSize());
    row.x0	 = x0_fix; // cm   : x intercept of x vs z line  ;
    row.dxdz	 = dxdz_fix; // :    slope    of x vs z line  ;
    row.y0	 = y0_fix; // cm   : y intercept of y vs z line  ;
    row.dydz	 = dydz_fix; // :    slope    of y vs z line  ;
    row.err_x0	 = x0Err_fix; // cm   : error on x0  ;
    row.err_dxdz = dxdzErr_fix; // : error on dxdz  ;
    row.err_y0	 = y0Err_fix; // cm   : error on y0  ;
    row.err_dydz = dydz_fix; // : error on dydz  ;
    row.chisq_dof= chisq_fix; // chi square / dof of fit  ;
    row.weight	 =        100; // weight of seed to use in vertex-finding  ;
    row.stats	 =tracks_fix; // number of events used in seed-finding  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
