TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/pxl/.idsOnTpc/idsOnTpc Allocated rows: 1  Used rows: 1  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("idsOnTpc",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =          1; // ;
    row.r01	 =          0; // -gamma ;
    row.r02	 =          0; // beta  ;
    row.r10	 =          0; // gamma ;
    row.r11	 =          1; // ;
    row.r12	 =          0; // -alpha ;
    row.r20	 =          0; // -beta  ;
    row.r21	 =          0; // alpha ;
    row.r22	 =          1; // ;
    row.t0	 =          0; // ;
    row.t1	 =          0; // ;
    row.t2	 =          0; // ;
    row.sigmaRotX	 =     0.0001; // ;
    row.sigmaRotY	 =     0.0001; // ;
    row.sigmaRotZ	 =     0.0001; // ;
    row.sigmaTrX	 =     0.0001; // ;
    row.sigmaTrY	 =     0.0001; // ;
    row.sigmaTrZ	 =     0.0001; // ;
 memcpy(&row.comment,"ideal\x20geometry",14);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
