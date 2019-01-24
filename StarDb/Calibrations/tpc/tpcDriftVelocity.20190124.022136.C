TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 23061
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.56389; // +/- 1.30774e-05 cm/us All: East = -0.271942 +/- 0.00568898
  row.laserDriftVelocityWest	 =   5.56389; // +/- 1.30774e-05 cm/us All: West = 0.412942 +/- 0.00261408
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.56389 +/- 1.30774e-05
  return (TDataSet *)tableSet;// West = 5.56317 +/- 1.4539e-05 East = 5.56694 +/- 2.99266e-05
};
