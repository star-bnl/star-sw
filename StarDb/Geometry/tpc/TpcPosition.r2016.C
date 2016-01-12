TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/tpc/.TpcPosition/TpcPosition Allocated rows: 1  Used rows: 1  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("TpcPosition",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          0; // ;
    row.r00	 =          1; // ;
    row.r01	 =   -0.00036; // -gamma ;
    row.r02	 =   -0.00048; // beta  ;
    row.r10	 =    0.00036; // gamma ;
    row.r11	 =          1; // ;
    row.r12	 =    -0.0001; // -alpha ;
    row.r20	 =    0.00048; // -beta  ;
    row.r21	 =     0.0001; // alpha ;
    row.r22	 =          1; // ;
    row.t0	 =    -0.2383; // ;
    row.t1	 =    -0.1732; // ;
    row.t2	 =    -0.1957; // ;
    row.sigmaRotX	 =          0; // ;
    row.sigmaRotY	 =          0; // ;
    row.sigmaRotZ	 =          0; // ;
    row.sigmaTrX	 =          0; // ;
    row.sigmaTrY	 =          0; // ;
    row.sigmaTrZ	 =          0; // ;
 memcpy(&row.comment,"2013\x20Tpc",8);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
