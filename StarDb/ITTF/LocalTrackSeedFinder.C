TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // LocalTrackSeedFinder 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_LocalTrackSeedFinder")) return 0;
  LocalTrackSeedFinder_st row;
  St_LocalTrackSeedFinder *tableSet = 
    new St_LocalTrackSeedFinder("LocalTrackSeedFinder",1);
  //
  memset(&row,0,tableSet->GetRowSize());
    row.DeltaY           =  5.;
    row.DeltaZ           = 15.;
    row.SeedLength       =   2;
    row.ExtrapDeltaY     =  1.;
    row.ExtrapDeltaZ     =  2.;
    row.MaxSkipped       =   4;
    row.ExtrapMinLength  =   2;
    row.ExtrapMaxLength  =   5;
    row.UseOrigin        =   1; // true
    row.DoHelixFit       =   1; // true
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
