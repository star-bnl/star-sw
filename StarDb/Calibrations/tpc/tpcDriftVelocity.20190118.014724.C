TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 17086
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55789; // +/- 1.00358e-05 cm/us All: East = -0.148981 +/- 0.00373318
  row.laserDriftVelocityWest	 =   5.55789; // +/- 1.00358e-05 cm/us All: West = 0.0845082 +/- 0.00195621
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55789 +/- 1.00358e-05
  return (TDataSet *)tableSet;// West = 5.55767 +/- 1.12294e-05 East = 5.55875 +/- 2.23684e-05
};
