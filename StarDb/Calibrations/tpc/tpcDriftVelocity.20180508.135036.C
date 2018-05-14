TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 128021
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56187; // +/- 1.168e-05 cm/us All: East = -0.239462 +/- 0.00395521
  row.laserDriftVelocityWest	 =   5.56187; // +/- 1.168e-05 cm/us All: West = 0.437733 +/- 0.00243607
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56187 +/- 1.168e-05
  return (TDataSet *)tableSet;// West = 5.56083 +/- 1.37356e-05 East = 5.5646 +/- 2.21956e-05
};
