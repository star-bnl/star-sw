TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tpc/.tpcGridLeak/tpcGridLeak Allocated rows: 1  Used rows: 1  Row size: 72 bytes
//  Table: tpcGridLeak_st[0]--> tpcGridLeak_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcGridLeak")) return 0;
tpcGridLeak_st row;
St_tpcGridLeak *tableSet = new St_tpcGridLeak("tpcGridLeak",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.InnerGLRadius	 =          0; // Radius of GL around inner sectors           ;
    row.MiddlGLRadius	 =          0; // Radius of GL between inner/outer sectors    ;
    row.OuterGLRadius	 =          0; // Radius of GL around outer sectors           ;
    row.InnerGLWidth	 =          0; // Width of GL around inner sectors            ;
    row.MiddlGLWidth	 =          0; // Width of GL between inner/outer sectors     ;
    row.OuterGLWidth	 =          0; // Width of GL around outer sectors            ;
    row.InnerGLStrength	 =          0; // Strength of GL around inner sectors         ;
    row.MiddlGLStrength	 =          0; // Strength of GL between inner/outer sectors  ;
    row.OuterGLStrength	 =          0; // Strength of GL around outer sectors         ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
