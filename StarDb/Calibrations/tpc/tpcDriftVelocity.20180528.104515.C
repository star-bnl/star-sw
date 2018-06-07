TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 148016
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55204; // +/- 0.000332729 cm/us All: East = -0.0878754 +/- 0.342994
  row.laserDriftVelocityWest	 =   5.55204; // +/- 0.000332729 cm/us All: West = 0.172623 +/- 0.0213157
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55204 +/- 0.000332729
  return (TDataSet *)tableSet;// West = 5.54831 +/- 0.003845 East = 5.55207 +/- 0.000333982
};
