// $Id: TpcResponseSimulator.y2009.C,v 1.3 2010/04/04 23:14:33 fisyak Exp $
// $Log: TpcResponseSimulator.y2009.C,v $
// Revision 1.3  2010/04/04 23:14:33  fisyak
// Add Row Correction
//
// Revision 1.2  2010/04/01 22:17:57  fisyak
// Freeze version W
//
TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.TpcResponseSimulator/TpcResponseSimulator Allocated rows: 1  Used rows: 1  Row size: 124 bytes
  //  Table: TpcResponseSimulator_st[0]--> TpcResponseSimulator_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_TpcResponseSimulator")) return 0;
  TpcResponseSimulator_st row;
  St_TpcResponseSimulator *tableSet = new St_TpcResponseSimulator("TpcResponseSimulator",1);
#if defined(  __Version_V__ )
  //y2009
  //SecRow3CGFdaq_2009_pp200N.root
  const Double_t SecRowDaq[8] = {
    //    0.0717067,-0.000702456,0.0266199,-0.000707799, // WE	
    0.06643  ,-0.0003326  ,0.0290264,-0.000839816, // W	
    0.0775855,-0.00113683 ,0.0241001,-0.000575163};// E	
  //SecRow3CGFpp200.U.root
  const Double_t SecRowTpcRS[8] = {
    //  -0.0410968,-9.93038e-06,0.246003,-0.000405891, // WE	
    -0.022317 ,0.00113934  ,0.235795,-0.000485105, // W	
    -0.0559222,-0.00114563 ,0.254177,-0.000292327}; // E	
#else /* __Version_X__ */
/* SecRow3CGFRunIX66DEV_calib */
  const Double_t SecRowDaq[12] = {
/* WE	*/ 0.00178543,7.59862e-05,0.00359473,1.63784e-05,
/* W	*/ 0.00213429,5.44094e-05,0.00394723,-9.59276e-06,
/* E	*/ 0.00163108,8.36518e-05,0.0030858,4.26142e-05};
#if 0
/* SecRow3CGFdaq_2009_pp200N */
/* WE	*/ 0.0670893,-0.00020545,0.0327326,-0.00083462,
/* W	*/ 0.0557396,0.00117627,0.0363049,-0.000927954,
/* E	*/ 0.0740859,-0.000378832,0.0322403,-0.000862608,
#endif
/* SecRow3CGFpp200.W */
  const Double_t SecRowTpcRS[12] = {
/* WE	*/ -0.0348841,-0.000377749,0.249109,-0.000362636,
/* W	*/ -0.0167217,0.000942187,0.237763,-0.000397571,
/* E	*/ -0.0498431,-0.00153771,0.258119,-0.000285127};


#endif
  memset(&row,0,tableSet->GetRowSize());
  row.I0                    = 13.1;// eV, CH4 		       
  row.Cluster    	    = 3.2; // average no. of electrons per primary  			       
  row.W          	    = 26.2;// eV 								       
  row.OmegaTau   	    = 3.02;// fit of data 							       
  row.K3IP       	    = 0.68;//(pads) for a/s = 2.5e-3 and h/s = 0.5 
  row.K3IR       	    = 0.89;//(row)  for a/s = 2.5e-3 and h/s = 0.5 
  row.K3OP       	    = 0.55;//(pads) for a/s = 2.5e-3 and h/s = 1.0 
  row.K3OR       	    = 0.61;//(row)  for a/s = 2.5e-3 and h/s = 1.0 
  row.FanoFactor 	    = 0.3; //                                                                        
  row.AveragePedestal       = 50.0;// 
  row.AveragePedestalRMS    = 1.4; // Old Tpc electronics 
  row.AveragePedestalRMSX   = 0.7; // New Tpx electronics 
  row.tauIntegration        = 2.5*74.6e-9;//   secs 
  row.tauF                  = 394.0e-9;// secs Tpc 
  row.tauP                  = 775.0e-9;// secs Tpc 
  row.tauXI                 =  60.0e-9;// secs Tpx Inner integration time 
  row.tauXO                 =  74.6e-9;// secs Tpx Outer integration time 
  row.tauCI                 =   0;  
  row.tauCO                 =   0;  
#if defined( __Version_O__ )
  row.SigmaJitterTI         = 0.0;//  for Tpx inner 
  row.SigmaJitterTO         = 0.0;//  for Tpx outer
#elif defined( __Version_P__ )
  row.SigmaJitterTI         = 0.2;//  for Tpx inner 
  row.SigmaJitterTO         = 0.2;//  for Tpx outer 
#else
  row.SigmaJitterTI         = 0.2;//  for Tpx inner 
  row.SigmaJitterTO         = 0.2;//  for Tpx outer 
#endif
#if defined( __Version_O__ ) || defined( __Version_P__ ) || defined( __Version_Q__ ) 
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
#elif  defined( __Version_S__ ) 
  row.longitudinalDiffusion = 0.0540;//
#elif defined(  __Version_U__ ) ||  defined(  __Version_V__ )
  row.longitudinalDiffusion = 0.0445;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
#else
  row.longitudinalDiffusion = 0.0370; // W
#endif
#if defined( __Version_N__ )
  row.transverseDiffusion   = 0.0640; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
#else
#if defined( __Version_Q__ )
  row.transverseDiffusion   = 0.0775; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
#else
  row.transverseDiffusion   = 0.0725; //  cm/sqrt(cm)  
#endif
#endif
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
#if defined( __Version_N__ )
  row.OmegaTauScaleI        = 2.145;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8;    // effective reduction of OmegaTau near Outer sector anode wire
#elif defined (__Version_O__ )
  row.OmegaTauScaleI        = 2.145*0.75;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8*0.95;    // effective reduction of OmegaTau near Outer sector anode wire
#elif defined (__Version_P__ )
  row.OmegaTauScaleI        = 2.145*1.30;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.07;    // effective reduction of OmegaTau near Outer sector anode wire
#else
  row.OmegaTauScaleI        = 2.145*1.30*1.30;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.07*1.25;    // effective reduction of OmegaTau near Outer sector anode wire
#endif
#if defined(  __Version_V__ )
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] = SecRowTpcRS[i] - SecRowDaq[i];
  }
#else /* __Version_X__ */
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] = SecRowTpcRS[i+4] - SecRowDaq[i+4];
  }
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
