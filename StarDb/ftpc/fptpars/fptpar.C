St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fptpars/fptpar Allocated rows: 1  Used rows: 1  Row size: 36 bytes
//  Table: fpt_fptpar_st[0]--> fpt_fptpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fpt_fptpar")) return 0;
fpt_fptpar_st row;
St_fpt_fptpar *tableSet = new St_fpt_fptpar("fptpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.padrows	 =         10; // # of padrows in one TPC ;
    row.sea_circ	 =        0.5; // Search circle radius ;
    row.merge_x	 =          0; // Merge Criterion (x-dir) ;
    row.merge_y	 =          0; // Merge criterion (y-dir) ;
    row.fan_angle	 =         45; // ang.range, in which a last pl. hit is sear. ;
    row.one_or_two	 =          2; // track only in one (=1) or both (=2) FTPCs ;
    row.inner_r	 =       7.73; // inner radius of the sensitive volume ;
    row.outer_r	 =      30.05; // outer radius of the sensitive volume ;
    row.cord_crit	 =        0.2; // tolerance by comparing the two cord lengths ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
