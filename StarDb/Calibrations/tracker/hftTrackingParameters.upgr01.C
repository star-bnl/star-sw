TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.ssdTrackingParameters/ssdTrackingParameters Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: TrackingParameters_st[0]--> TrackingParameters_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TrackingParameters")) return 0;
TrackingParameters_st row;
St_TrackingParameters *tableSet = new St_TrackingParameters("hftTrackingParameters",1);
//
memset(&row,0,tableSet->GetRowSize());
//                                        hit errors are squared !
    row.minSearch =  .05; // Minimum Search Window ;
    row.maxSearch =  .1; // Maximum Search Window ;
    row.scaling	  =  4.; // Search Window Scaling ;
    row.maxChi2	  = 10.; // Max Chi2 ;
tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
