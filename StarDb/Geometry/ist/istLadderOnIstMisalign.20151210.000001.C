TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/ist/.istLadderOnIst/istLadderOnIstMisalign Allocated rows: 24  Used rows: 24  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[23]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("istLadderOnIstMisalign",24);
//
 memset(&row,0,tableSet->GetRowSize());
 for ( int i=1; i<=24; i++ ) {
   row.Id	 =          1; // ;
   row.r00	 =  1.0000000; // ;
   row.r01	 =  0.0000000; // -gamma ;
   row.r02	 =  0.0000000; // beta  ;
   row.r10	 =  0.0000000; // gamma ;
   row.r11	 =  1.0000000; // ;
   row.r12	 =  0.0000000; // -alpha ;
   row.r20	 =  0.0000000; // -beta  ;
   row.r21	 =  0.0000000; // alpha ;
   row.r22	 =  1.0000000; // ;
   row.t0	 =  0.0000000; // ;
   row.t1	 =  0.0000000; // ;
   row.t2	 =  0.0000000; // ;
   row.sigmaRotX	 =      0.001; // ;
   row.sigmaRotY	 =      0.001; // ;
   row.sigmaRotZ	 =      0.001; // ;
   row.sigmaTrX	 =      0.001; // ;
   row.sigmaTrY	 =      0.001; // ;
   row.sigmaTrZ	 =      0.001; // ;
   memcpy(&row.comment,"\x00",1);// 
   tableSet->AddAt(&row);
 }
 // ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
