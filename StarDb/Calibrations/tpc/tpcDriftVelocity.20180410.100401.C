TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 100013
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55386; // +/- 2.22842e-05 cm/us All: East = 0.145874 +/- 0.00412859
  row.laserDriftVelocityWest	 =   5.55386; // +/- 2.22842e-05 cm/us All: West = 1.34347 +/- 0.108647
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55386 +/- 2.22842e-05
  return (TDataSet *)tableSet;// West = 5.54898 +/- 0.000135758 East = 5.55399 +/- 2.25906e-05
};
