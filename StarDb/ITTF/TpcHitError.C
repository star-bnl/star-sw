TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // HitError 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_HitError")) return 0;
  HitError_st row;
  St_HitError *tableSet = 
    new St_HitError("TpcHitError",2); // 0 - Inner, 1 - Outer
  //
  memset(&row,0,tableSet->GetRowSize());
  row.driftCross       = .066;    
  row.tanCross         = 1.2e-04; 
  row.intrinsicCross   = 0.0004;  
  row.driftDip         = .066;    
  row.tanDip   	       = 4.4e-4;  
  row.intrinsicDip     = 2.8e-02;
  tableSet->AddAt(&row);
  row.driftCross       = .02;    
  row.tanCross         = 4.e-3;  
  row.intrinsicCross   = 0.04;   
  row.driftDip         = .02;    
  row.tanDip   	       = 3.2e-3; 
  row.intrinsicDip     = 9.e-2);
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
