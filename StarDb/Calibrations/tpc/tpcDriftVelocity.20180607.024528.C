TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 157042
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55205; // +/- 4.91799e-06 cm/us All: East = 0.131384 +/- 0.00168737
  row.laserDriftVelocityWest	 =   5.55205; // +/- 4.91799e-06 cm/us All: West = 0.197531 +/- 0.00103091
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55205 +/- 4.91799e-06
  return (TDataSet *)tableSet;// West = 5.55194 +/- 5.78416e-06 East = 5.55233 +/- 9.34306e-06
};
