TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcDriftVelocity")) return 0;
  St_tpcDriftVelocity *tableSet = new St_tpcDriftVelocity("tpcDriftVelocity",1);
  tpcDriftVelocity_st row;// Laser Run 139056
  memset(&row, 0, tableSet->GetRowSize());
  row.laserDriftVelocityEast	 =   5.55682; // +/- 0.000212715 cm/us All: East = 0.0464423 +/- 4.51666
  row.laserDriftVelocityWest	 =   5.55682; // +/- 0.000212715 cm/us All: West = 0.479464 +/- 0.0944045
  tableSet->AddAt(&row);// 1e3*Delta: All = 5.55682 +/- 0.000212715
  return (TDataSet *)tableSet;// West = 5.55683 +/- 0.000213804 East = 5.55557 +/- 0.00211026
};
