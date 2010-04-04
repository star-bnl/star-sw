// $Id: TpcResponseSimulator.y2010.C,v 1.2 2010/04/04 23:14:33 fisyak Exp $
// $Log: TpcResponseSimulator.y2010.C,v $
// Revision 1.2  2010/04/04 23:14:33  fisyak
// Add Row Correction
//
// Revision 1.1  2010/04/01 22:17:57  fisyak
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
  //y2010
  //SecRow3CGFdaq_2010_AuAu200N.root
  const Double_t SecRowDaq[8] = {
    //    0.25159 ,0.00541165,0.00525316 ,-0.000159434, // WE	
    0.269851,0.00478697,0.000342209,-0.000143101, // W	
    0.238935,0.00573471,0.0106868  ,-0.000211026}; // E	
  const Double_t SecRowTpcRS[8] = {
    //SecRow3CGFauau200.U.root
    //    0.30308 ,-5.90499e-05 ,0.22098 ,-0.00029274, // WE	
    0.292102,-0.00117005  ,0.215507,-0.000314978, //W	
    0.313958, 0.000952929 ,0.22641 ,-0.000300687}; //E	
#else /* __Version_X__ */
#if 0
/* SecRow3CGFdaq_2010_AuAu200N */
/* WE	*/ 0.267023,0.007755,-0.000153647,3.7975e-05,
/* W	*/ 0.291137,0.00274119,-0.00925822,0.000263286,
/* E	*/ 0.284247,0.00888761,0.00822456,-0.000112362,
#endif
/* SecRow3CGFRunX03DEV_calib */
    const Double_t SecRowDaq[12] = {
/* WE	*/ 0.296029,0.00427073,-0.0113142,0.000258612,
/* W	*/ 0.26388,0.00407954,-0.0143448,0.000266297,
/* E	*/ 0.329723,0.00391894,-0.00964907,0.000286288};
/* SecRow3CGFauau200.W */
  const Double_t SecRowTpcRS[12] = {
/* WE	*/ 0.314961,-0.000310742,0.227639,-0.000341098,
/* W	*/ 0.305046,-0.00166713,0.221307,-0.00033598,
/* E	*/ 0.325477,0.000719157,0.230824,-0.000288251}
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
