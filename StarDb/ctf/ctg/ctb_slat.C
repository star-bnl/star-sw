St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// params/ctf/ctg/ctb_slat Allocated rows: 240  Used rows: 240  Row size: 32 bytes
//  Table: ctg_slat_st[0]--> ctg_slat_st[239]
// ====================================================================
ctg_slat_st row;
St_ctg_slat *tableSet = new St_ctg_slat("ctb_slat",240);
//
 if (!gROOT->GetClass("St_ctg_slat")) return 0;
 memset(&row,0,tableSet->GetRowSize());
 row.cc_adc           =      0.024; // ;
 row.cc_tdc           =    2.5e-11; // ;
 row.offset_adc       =          5; // ;
 row.offset_tdc       =          5; // ;
 row.ods_adc          =        0.5; // ;
 row.ods_tdc          =        0.5; // ;
 Int_t i, j, k=0; 
 for (j=1;j<=60;j++){
   row.i_phi          =          j; // ;
   for  (i=1;i<=4;i++){
     row.i_eta        =          i; // ;
     tableSet->AddAt(&row,k); k++;
   }
 }
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
