TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 136041
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53845; // +/- 4.53946e-06 cm/us All: East = 0.134499 +/- 0.00163993
  row.laserDriftVelocityWest	 =   5.53845; // +/- 4.53946e-06 cm/us All: West = 0.158975 +/- 0.000924206
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53845 +/- 4.53946e-06
  return (TDataSet *)tableSet;// West = 5.53842 +/- 5.2148e-06 East = 5.53857 +/- 9.22326e-06
};
