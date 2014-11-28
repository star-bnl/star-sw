TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.LocalTrackSeedFinder/LocalTrackSeedFinder Allocated rows: 4  Used rows: 4  Row size: 52 bytes
//  Table: LocalTrackSeedFinder_st[0]--> LocalTrackSeedFinder_st[3]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_LocalTrackSeedFinder")) return 0;
LocalTrackSeedFinder_st row;
St_LocalTrackSeedFinder *tableSet = new St_LocalTrackSeedFinder("LocalTrackSeedFinder",4);
//
memset(&row,0,tableSet->GetRowSize());
    row.deltaY	 =          5.; // search window in the next layer when connecting two points           ;
    row.deltaZ	 =         15.; // search window in the next layer when connecting two points           ;
    row.mExtrapDeltaY	 =  1.; // search window in the next layer when extending a conection of points ;
    row.mExtrapDeltaZ	 =  2.; // search window in the next layer when extending a conection of points ;
    row.seedLength	 =  2.; // minimum number of points to connect                                  ;
    row.maxSkipped	 =  4.; // max number of layers one can skip                                    ;
    row.extrapMaxLength	 =  5.; // define the Max number of points to extrapolate                       ;
    row.extrapMinLength	 =  2.; // define the Min number of points to extrapolate                       ;
    row.useOrigin	 =  1;  // Use the origin to calculate helix                                    ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
