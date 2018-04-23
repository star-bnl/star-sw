TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 78023
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55335; // +/- 1.16472e-05 cm/us All: East = 0.0690328 +/- 0.00395297
  row.laserDriftVelocityWest	 =   5.55335; // +/- 1.16472e-05 cm/us All: West = 0.188771 +/- 0.0025242
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55335 +/- 1.16472e-05
  return (TDataSet *)tableSet;// West = 5.55321 +/- 1.37647e-05 East = 5.55371 +/- 2.1855e-05
};
