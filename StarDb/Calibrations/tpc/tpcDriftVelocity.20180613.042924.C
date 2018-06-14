TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 164001
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54819; // +/- 6.28458e-06 cm/us All: East = 0.48113 +/- 1.04104
  row.laserDriftVelocityWest	 =   5.54819; // +/- 6.28458e-06 cm/us All: West = 0.184865 +/- 0.00110595
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54819 +/- 6.28458e-06
  return (TDataSet *)tableSet;// West = 5.54819 +/- 6.2846e-06 East = 5.54978 +/- 0.00242622
};
