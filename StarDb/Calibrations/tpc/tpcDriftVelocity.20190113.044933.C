TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 12062
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54053; // +/- 1.36401e-05 cm/us All: East = 0.80529 +/- 0.00369597
  row.laserDriftVelocityWest	 =   5.54053; // +/- 1.36401e-05 cm/us All: West = 1.48828 +/- 0.00325901
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54053 +/- 1.36401e-05
  return (TDataSet *)tableSet;// West = 5.53885 +/- 1.82678e-05 East = 5.54265 +/- 2.05056e-05
};
