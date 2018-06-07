TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 150010
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.54949; // +/- 6.88196e-06 cm/us All: East = 0.122353 +/- 0.00466817
  row.laserDriftVelocityWest	 =   5.54949; // +/- 6.88196e-06 cm/us All: West = 0.303746 +/- 0.00127442
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.54949 +/- 6.88196e-06
  return (TDataSet *)tableSet;// West = 5.54943 +/- 7.15385e-06 East = 5.55031 +/- 2.52024e-05
};
