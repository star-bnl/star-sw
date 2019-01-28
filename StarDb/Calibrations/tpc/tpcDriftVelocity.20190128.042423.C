TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 27081
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52879; // +/- 8.09769e-06 cm/us All: East = 0.89571 +/- 0.00314519
  row.laserDriftVelocityWest	 =   5.52879; // +/- 8.09769e-06 cm/us All: West = 1.21159 +/- 0.00160856
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52879 +/- 8.09769e-06
  return (TDataSet *)tableSet;// West = 5.52847 +/- 9.05836e-06 East = 5.53009 +/- 1.80681e-05
};
