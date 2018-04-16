TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 101011
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55192; // +/- 1.07582e-05 cm/us All: East = -5.59764 +/- 0.203838
  row.laserDriftVelocityWest	 =   5.55192; // +/- 1.07582e-05 cm/us All: West = -5.273 +/- 0.00189289
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55192 +/- 1.07582e-05
  return (TDataSet *)tableSet;// West = 5.55189 +/- 1.08178e-05 East = 5.55463 +/- 0.000102606
};
