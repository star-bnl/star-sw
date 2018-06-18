TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165012
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54745; // +/- 0.000204919 cm/us All: East = -1.72347 +/- 4.84735
  row.laserDriftVelocityWest	 =   5.54745; // +/- 0.000204919 cm/us All: West = 0.236847 +/- 0.314732
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54745 +/- 0.000204919
  return (TDataSet *)tableSet;// West = 5.54728 +/- 0.000210805 East = 5.55044 +/- 0.000873241
};
