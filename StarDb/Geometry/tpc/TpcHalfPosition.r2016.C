TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/tpc/.TpcHalfPosition/TpcHalfPosition Allocated rows: 2  Used rows: 2  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[1]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("TpcHalfPosition",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          0; // ;
    row.r00	 =          1; // ;
    row.r01	 =     -3e-05; // -gamma ;
    row.r02	 =    -0.0002; // beta  ;
    row.r10	 =      3e-05; // gamma ;
    row.r11	 =          1; // ;
    row.r12	 =          0; // -alpha ;
    row.r20	 =     0.0002; // -beta  ;
    row.r21	 =          0; // alpha ;
    row.r22	 =          1; // ;
    row.t0	 =          0; // ;
    row.t1	 =          0; // ;
    row.t2	 =          0; // ;
    row.sigmaRotX	 =          0; // ;
    row.sigmaRotY	 =          0; // ;
    row.sigmaRotZ	 =          0; // ;
    row.sigmaTrX	 =          0; // ;
    row.sigmaTrY	 =          0; // ;
    row.sigmaTrZ	 =          0; // ;
 memcpy(&row.comment,"2013\x20east\x20+\x20Membrane\x2014\x20+0.2mrad",32);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =          1; // ;
    row.r01	 =    0.00039; // -gamma ;
    row.r02	 =    0.00011; // beta  ;
    row.r10	 =   -0.00039; // gamma ;
    row.r11	 =          1; // ;
    row.r12	 =   -0.00016; // -alpha ;
    row.r20	 =   -0.00011; // -beta  ;
    row.r21	 =    0.00016; // alpha ;
    row.r22	 =          1; // ;
    row.t0	 =     0.0193; // ;
    row.t1	 =    -0.0133; // ;
    row.t2	 =    -0.0156; // ;
    row.sigmaRotX	 =          0; // ;
    row.sigmaRotY	 =          0; // ;
    row.sigmaRotZ	 =          0; // ;
    row.sigmaTrX	 =          0; // ;
    row.sigmaTrY	 =          0; // ;
    row.sigmaTrZ	 =          0; // ;
 memcpy(&row.comment,"2013\x20west",9);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
