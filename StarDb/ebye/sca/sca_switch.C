St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/sca/sca_switch Allocated rows: 1  Used rows: 1  Row size: 16 bytes
//  Table: sca_switch_st[0]--> sca_switch_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sca_switch")) return 0;
sca_switch_st row;
St_sca_switch *tableSet = new St_sca_switch("sca_switch",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.makePrior	 =          0; // ;
    row.makeEnsembleAve	 =          0; // ;
    row.doAnalysis	 =          0; // ensemble average                                      ;
    row.nEvents	 =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
