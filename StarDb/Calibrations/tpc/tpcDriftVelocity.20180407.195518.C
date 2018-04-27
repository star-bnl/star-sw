TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 97052
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53629; // +/- 9.03497e-06 cm/us All: East = 0.147039 +/- 0.0022593
  row.laserDriftVelocityWest	 =   5.53629; // +/- 9.03497e-06 cm/us All: West = 0.216866 +/- 0.00231476
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53629 +/- 9.03497e-06
  return (TDataSet *)tableSet;// West = 5.53609 +/- 1.29177e-05 East = 5.53648 +/- 1.26416e-05
};
