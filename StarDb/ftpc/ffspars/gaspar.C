St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ffspars/gaspar Allocated rows: 1  Used rows: 1  Row size: 96 bytes
//  Table: ffs_gaspar_st[0]--> ffs_gaspar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ffs_gaspar")) return 0;
ffs_gaspar_st row;
St_ffs_gaspar *tableSet = new St_ffs_gaspar("gaspar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.vdrift[0]	 =        4.6; // parameters of a P3-fit of drift-vel. ;
    row.vdrift[1]	 =     -0.418;
    row.vdrift[2]	 =     0.0149;
    row.vdrift[3]	 =  -0.000182;
    row.tdrift[0]	 =         31; // parameters of a P3-fit of drift-time ;
    row.tdrift[1]	 =      0.125;
    row.tdrift[2]	 =    -0.0365;
    row.tdrift[3]	 =   3.82e-05;
    row.sig_rad[0]	 =        800; // sigma(rad) ;
    row.sig_rad[1]	 =          0;
    row.sig_rad[2]	 =          0; // 
    row.sig_rad[3]	 =          0;
    row.sig_azi[0]	 =       2000; // sigma(azi) ;
    row.sig_azi[1]	 =          0;
    row.sig_azi[2]	 =          0;
    row.sig_azi[3]	 =          0;
    row.err_rad[0]	 =         90; // error(rad) ;
    row.err_rad[1]	 =          0;
    row.err_rad[2]	 =          0; // 
    row.err_rad[3]	 =          0;
    row.err_azi[0]	 =         50; // error(azi) ;
    row.err_azi[1]	 =          0;
    row.err_azi[2]	 =          0;
    row.err_azi[3]	 =          0;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
