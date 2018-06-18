TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 165028
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54763; // +/- 5.75173e-06 cm/us All: East = -0.0678201 +/- 0.00350765
  row.laserDriftVelocityWest	 =   5.54763; // +/- 5.75173e-06 cm/us All: West = 0.231321 +/- 0.00106975
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54763 +/- 5.75173e-06
  return (TDataSet *)tableSet;// West = 5.5475 +/- 6.02888e-06 East = 5.54898 +/- 1.91907e-05
};
