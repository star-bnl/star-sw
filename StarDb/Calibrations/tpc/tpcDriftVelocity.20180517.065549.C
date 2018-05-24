TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 137009
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53823; // +/- 1.20278e-05 cm/us All: East = 0.328562 +/- 0.018425
  row.laserDriftVelocityWest	 =   5.53823; // +/- 1.20278e-05 cm/us All: West = 0.164115 +/- 0.00218973
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53823 +/- 1.20278e-05
  return (TDataSet *)tableSet;// West = 5.53828 +/- 1.21979e-05 East = 5.5364 +/- 7.22822e-05
};
