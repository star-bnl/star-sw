TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 89008
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.51705; // +/- 1.34881e-05 cm/us All: East = -0.679922 +/- 0.00748285
  row.laserDriftVelocityWest	 =   5.51705; // +/- 1.34881e-05 cm/us All: West = 0.188759 +/- 0.00256816
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.51705 +/- 1.34881e-05
  return (TDataSet *)tableSet;// West = 5.51653 +/- 1.42861e-05 East = 5.52133 +/- 4.09293e-05
};
