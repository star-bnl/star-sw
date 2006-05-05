TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/svt/svtDriftCorrection
//  Table: svtCorrection_st[0]--> svtCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_svtCorrection")) return 0;
  svtCorrection_st row;
  St_svtCorrection *tableSet = new St_svtCorrection("svtDriftCorrection",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
