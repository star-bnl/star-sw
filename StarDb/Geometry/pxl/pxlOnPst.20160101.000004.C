TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/pxl/.pxlOnPst/pxlOnPst Allocated rows: 1  Used rows: 1  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("pxlOnPst",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =    0.99991; // ;
    row.r01	 = 0.00247503; // -gamma ;
    row.r02	 = -3.35217e-05; // beta  ;
    row.r10	 = -0.00251561; // gamma ;
    row.r11	 =   0.999857; // ;
    row.r12	 = 0.00107501; // -alpha ;
    row.r20	 = 1.9286e-05; // -beta  ;
    row.r21	 = -0.00100373; // alpha ;
    row.r22	 =   0.999895; // ;
    row.t0	 = -0.0150608; // ;
    row.t1	 =  0.0119463; // ;
    row.t2	 = -0.00868523; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
