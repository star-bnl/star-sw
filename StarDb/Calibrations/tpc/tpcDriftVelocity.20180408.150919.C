TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 98028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.5493; // +/- 1.32582e-05 cm/us All: East = -4.96077 +/- 0.00344315
  row.laserDriftVelocityWest	 =   5.5493; // +/- 1.32582e-05 cm/us All: West = -4.69543 +/- 0.00322529
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.5493 +/- 1.32582e-05
  return (TDataSet *)tableSet;// West = 5.54867 +/- 1.80163e-05 East = 5.55004 +/- 1.95812e-05
};
