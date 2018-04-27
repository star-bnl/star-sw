TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 76007
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53719; // +/- 9.64832e-06 cm/us All: East = 12.4297 +/- 17.3915
  row.laserDriftVelocityWest	 =   5.53719; // +/- 9.64832e-06 cm/us All: West = 2.06694 +/- 0.00173492
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53719 +/- 9.64832e-06
  return (TDataSet *)tableSet;// West = 5.53719 +/- 9.64832e-06 East = 5.51748 +/- 0.0364351
};
