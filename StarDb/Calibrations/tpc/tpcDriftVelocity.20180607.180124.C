TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 158050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55095; // +/- 4.55691e-06 cm/us All: East = 0.568166 +/- 0.00141254
  row.laserDriftVelocityWest	 =   5.55095; // +/- 4.55691e-06 cm/us All: West = 0.568907 +/- 0.000979045
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55095 +/- 4.55691e-06
  return (TDataSet *)tableSet;// West = 5.55096 +/- 5.52347e-06 East = 5.55091 +/- 8.06359e-06
};
