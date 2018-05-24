TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 138026
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54601; // +/- 6.51613e-06 cm/us All: East = 0.359845 +/- 0.00734718
  row.laserDriftVelocityWest	 =   5.54601; // +/- 6.51613e-06 cm/us All: West = 0.175186 +/- 0.00116932
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54601 +/- 6.51613e-06
  return (TDataSet *)tableSet;// West = 5.54604 +/- 6.59741e-06 East = 5.54478 +/- 4.16391e-05
};
