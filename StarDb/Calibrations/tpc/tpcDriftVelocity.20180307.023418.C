TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 65065
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53047; // +/- 7.74761e-06 cm/us All: East = -0.133209 +/- 0.0019365
  row.laserDriftVelocityWest	 =   5.53047; // +/- 7.74761e-06 cm/us All: West = 0.454482 +/- 0.0019907
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53047 +/- 7.74761e-06
  return (TDataSet *)tableSet;// West = 5.52885 +/- 1.10236e-05 East = 5.53205 +/- 1.08911e-05
};
