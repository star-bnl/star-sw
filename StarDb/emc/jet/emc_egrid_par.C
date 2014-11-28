St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/jet/emc_egrid_par Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: emc_egrid_par_st[0]--> emc_egrid_par_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_emc_egrid_par")) return 0;
emc_egrid_par_st row;
St_emc_egrid_par *tableSet = new St_emc_egrid_par("emc_egrid_par",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.chrg_eta_min	 =         -2; // Cut for eta minimum for charged track ;
    row.chrg_eta_max	 =          2; // Cut for eta maximum for charged track ;
    row.ideal_eta_nbin	 =        100; // Size of energy matrix for ideal egrid ;
    row.ideal_phi_nbin	 =        100; // Size of energy matrix for ideal egrid ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
