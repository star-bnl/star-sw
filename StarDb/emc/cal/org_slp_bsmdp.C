St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/cal/org_slp_bsmdp Allocated rows: 18000  Used rows: 18000  Row size: 4 bytes
//  Table: emc_adcslope_st[0]--> emc_adcslope_st[17999]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_emc_adcslope")) return 0;
  emc_adcslope_st row;
  St_emc_adcslope *tableSet = new St_emc_adcslope("org_slp_bsmdp",18000);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.p0	 =          1; // adc slope for ADC channel ;
  for (Int_t i=0;i<18000;i++) tableSet->AddAt(&row,i);
  // ----------------- end of code ---------------
  return (St_DataSet *)tableSet;
}
