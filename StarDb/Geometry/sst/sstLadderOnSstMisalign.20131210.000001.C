TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Geometry/sst/.sstLadderOnSst/sstLadderOnSstMisalign Allocated rows: 20  Used rows: 20  Row size: 180 bytes
//  Table: Survey_st[0]--> Survey_st[19]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_Survey")) return 0;
Survey_st row;
St_Survey *tableSet = new St_Survey("sstLadderOnSstMisalign",20);
//

 for ( int i=101; i<=120; i++ ) {

   memset(&row,0,tableSet->GetRowSize());
    row.Id	 =        i; // ;
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
    row.sigmaRotX	 =     0.0001; // ;
    row.sigmaRotY	 =     0.0001; // ;
    row.sigmaRotZ	 =     0.0001; // ;
    row.sigmaTrX	 =     0.0001; // ;
    row.sigmaTrY	 =     0.0001; // ;
    row.sigmaTrZ	 =     0.0001; // ;
    memcpy(&row.comment,"\x00",1);// 
    tableSet->AddAt(&row);

 }
 return tableSet;
}
