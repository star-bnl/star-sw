TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 15054
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55941; // +/- 2.41696e-05 cm/us All: East = -1.60678 +/- 0.0043285
  row.laserDriftVelocityWest	 =   5.55941; // +/- 2.41696e-05 cm/us All: West = -999 +/- 999
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55941 +/- 2.41696e-05
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.55941 +/- 2.41696e-05
};
