TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Geometry/pxl/.pxlHalfOnPxl/pxlHalfOnPxl Allocated rows: 2  Used rows: 2  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[1]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("pxlHalfOnPxl",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          1; // ;
    row.r00	 =   0.999818; // ;
    row.r01	 = 5.94964e-05; // -gamma ;
    row.r02	 = 0.000171824; // beta  ;
    row.r10	 = -0.000131513; // gamma ;
    row.r11	 =   0.999728; // ;
    row.r12	 = 0.000867831; // -alpha ;
    row.r20	 = -0.000182758; // -beta  ;
    row.r21	 = -0.000730146; // alpha ;
    row.r22	 =   0.999819; // ;
    row.t0	 =  0.0134245; // ;
    row.t1	 =  0.0246484; // ;
    row.t2	 = 0.00595419; // ;
    row.sigmaRotX	 =      0.001; // ;
    row.sigmaRotY	 =      0.001; // ;
    row.sigmaRotZ	 =      0.001; // ;
    row.sigmaTrX	 =      0.001; // ;
    row.sigmaTrY	 =      0.001; // ;
    row.sigmaTrZ	 =      0.001; // ;
 memcpy(&row.comment,"\x00",1);// 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
    row.Id	 =          2; // ;
    row.r00	 =   0.999836; // ;
    row.r01	 = -0.00014043; // -gamma ;
    row.r02	 = -0.000205703; // beta  ;
    row.r10	 = 5.05797e-05; // gamma ;
    row.r11	 =   0.999715; // ;
    row.r12	 = -0.000725518; // -alpha ;
    row.r20	 = 0.00014888; // -beta  ;
    row.r21	 = 0.000872458; // alpha ;
    row.r22	 =   0.999764; // ;
    row.t0	 = -0.0134245; // ;
    row.t1	 = -0.0246485; // ;
    row.t2	 = -0.00595427; // ;
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
