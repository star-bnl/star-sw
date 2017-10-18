TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.data/StarDb/Calibrations/tracker/.ssdTrackingParameters/ssdTrackingParameters Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: TrackingParameters_st[0]--> TrackingParameters_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TrackingParameters")) return 0;
TrackingParameters_st row;
St_TrackingParameters *tableSet = new St_TrackingParameters("PixelTrackingParameters",1);
//
memset(&row,0,tableSet->GetRowSize());
// Jan Kapitan's parameters :/star/u/kapitan/HFT/simu/hpss/debug/DEV_small_old/StarDb/Calibrations/tracker/PixelTrackingParameters.upgr01.C
 row.minSearch =  .01; // Minimum Search Window ;
 row.maxSearch =  .5; // Maximum Search Window ;
 row.scaling   =  10.; // Search Window Scaling ;
 row.maxChi2   = 100.; // Max Chi2 ;
 
 tableSet->AddAt(&row);
 // ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
