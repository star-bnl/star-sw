St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ftpcdEdxPars Allocated rows: 1  Used rows: 1  Row size: 40 bytes
//  Table: ftpcdEdxPars_st[0]--> ftpcdEdxPars_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ftpcdEdxPars")) return 0;
ftpcdEdxPars_st row;
St_ftpcdEdxPars *tableSet = new St_ftpcdEdxPars("ftpcdEdxPars",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.debug_level	 =         10; // level < 11: print output results ;
    row.id_method	 =          0; // default=0, truncated mean ;
    row.no_angle	 =          0; // 0 to consider dip/cross angles ;
    row.max_hit	 =         10; // default=20 ;
    row.min_hit	 =          4; // default=10 ;
    row.max_track	 =     100000; // max. tracks to process ;
    row.pad_length	 =          2; // pad length in cm ;
    row.frac_trun	 =        0.8; // default=0.8, 80% truncated mean ;
    row.a_i_p	 =         26; // default=30. eV ;
    row.a_large_number	 =      1e+10; // a large number ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
