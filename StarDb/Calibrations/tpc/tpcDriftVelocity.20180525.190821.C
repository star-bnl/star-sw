TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 145039
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55147; // +/- 0.000358655 cm/us All: East = 0.222255 +/- 0.189691
  row.laserDriftVelocityWest	 =   5.55147; // +/- 0.000358655 cm/us All: West = 0.0974919 +/- 0.0570825
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55147 +/- 0.000358655
  return (TDataSet *)tableSet;// West = -999 +/- 999 East = 5.55147 +/- 0.000358655
};
