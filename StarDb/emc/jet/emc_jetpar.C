St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/jet/emc_jetpar Allocated rows: 1  Used rows: 1  Row size: 76 bytes
//  Table: emc_jetpar_st[0]--> emc_jetpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_emc_jetpar")) return 0;
emc_jetpar_st row;
St_emc_jetpar *tableSet = new St_emc_jetpar("emc_jetpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.init	 =          1; // initialization flag ;
    row.jetmethod	 =          1; // jet finding algorithm (only =1 works right now) ;
    row.jetweighting	 =          1; // =1 to use eta,phi weighting ;
    row.cone_rad	 =        0.7; // jet cone radius in cluster algorithm ;
    row.ej_min	 =         10; // energy cut for post-algorithm analysis ;
    row.et_min	 =       0.05; // min energy deposition to be collected ;
    row.et_seed	 =          3; // init. transverse energy in algorithm GeV ;
    row.par[0]	 =          0; // possible extra params in algorithm ;
    row.par[1]	 =          0;
    row.par[2]	 =          0;
    row.par[3]	 =          0;
    row.par[4]	 =          0;
    row.phi_dev	 =        100; // back-to-back phi deviations for 2 jets ;
    row.min_cone_move	 =       0.05; // stop moving cone when it moves less than this ;
    row.max_cone_move	 =       0.35; // cone will not move more than this away from initiator;
    row.mode_bg	 =          1; // background subtraction mode off=0/on=1;
    row.prec_bg	 =      0.035; // stop iteration when bg level doesn's change this much;
    row.eta_max	 =          2; // minimum eta for jet ;
    row.eta_min	 =         -2; // maxinum eta for jet ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
