St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/srspars/srs_direct Allocated rows: 2  Used rows: 2  Row size: 8 bytes
//  Table: srs_direct_st[0]--> srs_direct_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_srs_direct")) return 0;
srs_direct_st row;
St_srs_direct *tableSet = new St_srs_direct("srs_direct",2);
//
memset(&row,0,tableSet->GetRowSize());
    row.sd	 =      0.002; // sigma in drift direction ;
    row.st	 =      0.002; // sigma transverse direction ;
tableSet->AddAt(&row,0);
memset(&row,0,tableSet->GetRowSize());
    row.sd	 =      0.002; // sigma of SSD in the trans. direction ;
    row.st	 =      0.080; // sigma of SSD in the strip direction ;
tableSet->AddAt(&row,1);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
