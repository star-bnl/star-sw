TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 81050
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54427; // +/- 1.3136e-05 cm/us All: East = -0.437212 +/- 0.0100919
  row.laserDriftVelocityWest	 =   5.54427; // +/- 1.3136e-05 cm/us All: West = -0.410658 +/- 0.00242329
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54427 +/- 1.3136e-05
  return (TDataSet *)tableSet;// West = 5.54426 +/- 1.3601e-05 East = 5.54443 +/- 5.06669e-05
};
