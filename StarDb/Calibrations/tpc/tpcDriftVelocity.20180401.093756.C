TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 91017
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.52883; // +/- 1.09198e-05 cm/us All: East = -0.437817 +/- 0.00583342
  row.laserDriftVelocityWest	 =   5.52883; // +/- 1.09198e-05 cm/us All: West = 0.274553 +/- 0.00209761
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.52883 +/- 1.09198e-05
  return (TDataSet *)tableSet;// West = 5.52839 +/- 1.15857e-05 East = 5.53235 +/- 3.26813e-05
};
