St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fdepars/fdepar Allocated rows: 1  Used rows: 1  Row size: 56 bytes
//  Table: fde_fdepar_st[0]--> fde_fdepar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fde_fdepar")) return 0;
fde_fdepar_st row;
St_fde_fdepar *tableSet = new St_fde_fdepar("fdepar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.debug_level	 =         10; // level < 11: print output results ;
    row.id_method	 =          0; // default=0, truncated mean ;
    row.no_angle	 =          0; // 0 to consider dip/cross angles ;
    row.max_track	 =     100000; // max. tracks to process ;
    row.nok_track	 =          0; // no. tracks accepted ;
    row.max_hit	 =         10; // default=20 ;
    row.min_hit	 =          4; // default=10 ;
    row.n_row	 =         10; // number of padrows ;
    row.a_large_number	 =      1e+10; // a large number ;
    row.pad_length	 =          2; // pad length in cm ;
    row.frac_trun	 =        0.8; // default=0.8, 80% truncated mean ;
    row.a_i_p	 =         26; // default=30. eV ;
    row.min_dedx	 = 0.000394161; // default=1. ;
    row.total_charge	 =          0; // total charge (energy loss/a_i_p) ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
