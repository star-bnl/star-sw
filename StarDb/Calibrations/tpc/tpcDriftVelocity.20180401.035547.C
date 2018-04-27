TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 90055
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51826; // +/- 1.14033e-05 cm/us All: East = -0.594088 +/- 0.00880644
  row.laserDriftVelocityWest	 =   5.51826; // +/- 1.14033e-05 cm/us All: West = 0.207664 +/- 0.00208334
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51826 +/- 1.14033e-05
  return (TDataSet *)tableSet;// West = 5.51805 +/- 1.16906e-05 East = 5.52245 +/- 5.17532e-05
};
