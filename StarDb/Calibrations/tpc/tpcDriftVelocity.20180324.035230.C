TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 82047
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.53501; // +/- 9.58515e-06 cm/us All: East = -0.450175 +/- 0.00702991
  row.laserDriftVelocityWest	 =   5.53501; // +/- 9.58515e-06 cm/us All: West = 0.208727 +/- 0.00177636
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.53501 +/- 9.58515e-06
  return (TDataSet *)tableSet;// West = 5.53479 +/- 9.87609e-06 East = 5.53844 +/- 3.97827e-05
};
