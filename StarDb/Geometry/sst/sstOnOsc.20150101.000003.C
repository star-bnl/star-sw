TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/ist/.idsOnTpc/idsOnTpc Allocated rows: 1  Used rows: 1  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("idsOnTpc",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =   0.999997; // ;
    row.r01	 = 0.00254671; // -gamma ;
    row.r02	 = 0.000310778; // beta  ;
    row.r10	 = -0.00254692; // gamma ;
    row.r11	 =   0.999997; // ;
    row.r12	 = 0.00100149; // -alpha ;
    row.r20	 = -0.000308298; // -beta  ;
    row.r21	 = -0.00100194; // alpha ;
    row.r22	 =          1; // ;
    row.t0	 =  0.0390069; // ;
    row.t1	 =  0.0606756; // ;
    row.t2	 =   0.164951; // ;
    row.sigmaRotX	 =     0.0001; // ;
    row.sigmaRotY	 =     0.0001; // ;
    row.sigmaRotZ	 =     0.0001; // ;
    row.sigmaTrX	 =     0.0001; // ;
    row.sigmaTrY	 =     0.0001; // ;
    row.sigmaTrZ	 =     0.0001; // ;
 memcpy(&row.comment,"aligned\x20geometry",16);// 
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
