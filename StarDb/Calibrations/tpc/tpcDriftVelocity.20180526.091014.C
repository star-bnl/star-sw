TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 146013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55145; // +/- 0.000100841 cm/us All: East = 0.144317 +/- 0.165837
  row.laserDriftVelocityWest	 =   5.55145; // +/- 0.000100841 cm/us All: West = 0.2283 +/- 0.0716029
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55145 +/- 0.000100841
  return (TDataSet *)tableSet;// West = 5.55127 +/- 0.000133569 East = 5.55169 +/- 0.000153778
};
