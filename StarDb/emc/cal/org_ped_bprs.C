St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/cal/org_ped_bprs Allocated rows: 4800  Used rows: 4800  Row size: 4 bytes
//  Table: emc_pedestal_st[0]--> emc_pedestal_st[4799]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_emc_pedestal")) return 0;
  emc_pedestal_st row;
  St_emc_pedestal *tableSet = new St_emc_pedestal("org_ped_bprs",4800);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.ped	 =          0; // pedestal for ADC channel ;
  for (Int_t i=0;i<4800;i++) tableSet->AddAt(&row,i);
  // ----------------- end of code ---------------
  return (St_DataSet *)tableSet;
}
