TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 130071
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55205; // +/- 1.84233e-05 cm/us All: East = 0.0550447 +/- 0.0145227
  row.laserDriftVelocityWest	 =   5.55205; // +/- 1.84233e-05 cm/us All: West = 0.08195 +/- 0.00385431
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55205 +/- 1.84233e-05
  return (TDataSet *)tableSet;// West = 5.55202 +/- 1.94265e-05 East = 5.55233 +/- 5.80791e-05
};
