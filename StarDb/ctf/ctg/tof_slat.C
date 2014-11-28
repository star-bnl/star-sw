St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// params/ctf/ctg/tof_slat Allocated rows: 5400  Used rows: 5400  Row size: 32 bytes
//  Table: ctg_slat_st[0]--> ctg_slat_st[5399]
// ====================================================================
ctg_slat_st row;
St_ctg_slat *tableSet = new St_ctg_slat("tof_slat",5400);
//
 if (!gROOT->GetClass("St_ctg_slat")) return 0;
 memset(&row,0,tableSet->GetRowSize());
 row.cc_adc           =      0.024; // ;
 row.cc_tdc           =    2.5e-11; // ;
 row.offset_adc       =          0; // ;
 row.offset_tdc       =          0; // ;
 row.ods_adc          =          0; // ;
 row.ods_tdc          =          0; // ;
 Int_t i,j,k=0;
 for (j=1;j<=300;j++){
   row.i_phi          =          j; // ;
   for (i=1;i<=18;i++){ 
     row.i_eta        =          i; // ;
     tableSet->AddAt(&row,k); k++;
   }
 }
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
