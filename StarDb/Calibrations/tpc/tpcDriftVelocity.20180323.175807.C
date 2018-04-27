TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82031
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52741; // +/- 1.04516e-05 cm/us All: East = 1.27085 +/- 0.00571796
  row.laserDriftVelocityWest	 =   5.52741; // +/- 1.04516e-05 cm/us All: West = 2.0557 +/- 0.00198752
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52741 +/- 1.04516e-05
  return (TDataSet *)tableSet;// West = 5.52696 +/- 1.10692e-05 East = 5.53117 +/- 3.1731e-05
};
