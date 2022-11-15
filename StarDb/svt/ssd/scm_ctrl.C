TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// scm_ctrl Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: scm_ctrl_st[0]--> scm_ctrl_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_scm_ctrl")) return 0;
scm_ctrl_st row;
St_scm_ctrl *tableSet = new St_scm_ctrl("scm_ctrl",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.clusterTreat	 =         13; // max number of cluster in a matched ;
    row.AdcTolerance	 =        0.2; // set to 20% ;
    row.matchMean	 =          0; // matching mean ;
    row.matchSigma	 =          8; // matching sigma ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
