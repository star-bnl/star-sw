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
  /* AuAu200 
root.exe [8] xZ_1->Fit("pol1","er","",-40,20)
 FCN=51.8992 FROM MINOS     STATUS=SUCCESSFUL     10 CALLS          61 TOTAL
                     EDM=3.13516e-20    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           4.79211e-02   1.56197e-04   1.19044e-11   1.96501e-08
   2  p1          -7.08302e-04   1.25979e-05   1.25979e-05  -1.59045e-02

root.exe [6] yZ_1->Fit("pol1","er","",-40,20)
 FCN=46.7008 FROM MINOS     STATUS=SUCCESSFUL      8 CALLS          69 TOTAL
                     EDM=2.30623e-17    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0          -3.53243e-01   1.62536e-04   0.00000e+00  -7.43715e-04
   2  p1          -4.42782e-04   1.32032e-05   1.32032e-05  -1.54980e-02
   */
  memset(&row,0,tableSet->GetRowSize());
  row.x0	 =      4.79211e-02; // cm   : x intercept of x vs z line  ;
  row.dxdz	 =     -7.08302e-04; //      :    slope    of x vs z line  ;
  row.y0	 =     -3.53243e-01; // cm   : y intercept of y vs z line  ;
  row.dydz	 =     -4.42782e-04; //      :    slope    of y vs z line  ;
  row.err_x0	 =      1.56197e-04; // cm   : error on x0  ;
  row.err_dxdz	 =      1.25979e-05; //      : error on dxdz  ;
  row.err_y0	 =      1.62536e-04; // cm   : error on y0  ;
  row.err_dydz	 =      1.32032e-05; //      : error on dydz  ;
  row.chisq_dof	 =       1.68; // chi square / dof of fit  ;
  row.weight	 =        100; // weight of seed to use in vertex-finding  ;
  row.stats	 =          0; // number of events used in seed-finding  ;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
