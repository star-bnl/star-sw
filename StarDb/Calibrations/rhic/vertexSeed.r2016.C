TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/rhic/.vertexSeed/vertexSeed Allocated rows: 1  Used rows: 1  Row size: 44 bytes
//  Table: vertexSeed_st[0]--> vertexSeed_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_vertexSeed")) return 0;
vertexSeed_st row;
St_vertexSeed *tableSet = new St_vertexSeed("vertexSeed",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.x0	 = -2.38206064000000023e-01; // cm   : x intercept of x vs z line  ;
    row.dxdz	 = -4.79999999999999958e-04; // :    slope    of x vs z line  ;
    row.y0	 = -1.73180429999999996e-01; // cm   : y intercept of y vs z line  ;
    row.dydz	 = -9.99999999999999912e-05; // :    slope    of y vs z line  ;
    row.err_x0	 = 0; // cm   : error on x0  ;
    row.err_dxdz = 0; // : error on dxdz  ;
    row.err_y0	 = 0; // cm   : error on y0  ;
    row.err_dydz = 0; // : error on dydz  ;
    row.chisq_dof= 0; // chi square / dof of fit  ;
    row.weight	 = 0; // weight of seed to use in vertex-finding  ;
    row.stats	 = 0; // number of events used in seed-finding  ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
