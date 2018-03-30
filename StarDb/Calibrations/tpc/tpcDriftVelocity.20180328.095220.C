TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 87042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52804; // +/- 8.51844e-06 cm/us All: East = 1.52902 +/- 0.00809895
  row.laserDriftVelocityWest	 =   5.52804; // +/- 8.51844e-06 cm/us All: West = 2.55616 +/- 0.00154752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52804 +/- 8.51844e-06
  return (TDataSet *)tableSet;// West = 5.52785 +/- 8.66199e-06 East = 5.53348 +/- 4.6985e-05
};
