TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 20015
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55604; // +/- 8.90971e-06 cm/us All: East = 0.544857 +/- 0.00292409
  row.laserDriftVelocityWest	 =   5.55604; // +/- 8.90971e-06 cm/us All: West = 0.653491 +/- 0.00188224
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55604 +/- 8.90971e-06
  return (TDataSet *)tableSet;// West = 5.5559 +/- 1.06036e-05 East = 5.55638 +/- 1.64327e-05
};
