// $Id: TpcResponseSimulator.C,v 1.3 2010/04/04 23:14:33 fisyak Exp $
// $Log: TpcResponseSimulator.C,v $
// Revision 1.3  2010/04/04 23:14:33  fisyak
// Add Row Correction
//
// Revision 1.2  2010/04/01 22:18:07  fisyak
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
  //y2005
  //SecRow3CGFdaq_2005_CuCu22N.root
  const Double_t SecRowDaq[8] = {
    //    0.0600245,-0.00125623, 0.00582222,-0.000315716,// WE	
    0.0458957,-0.00087154, 0.0136403, -0.000563829,//	W	
    0.0744207,-0.00126576,-0.00268546,-4.91927e-05}; //	E	
  const Double_t SecRowTpcRS[8] = {
    //SecRow3CGFcucu22.U.root
    //0.213151,0.000240478,0.316155,-0.000106692, //WE	
    0.213732,0.000197346,0.317265,-0.000111175, //W	
    0.21252 ,0.000285084,0.314919,-9.87074e-05};//E	
  
#else /* __Version_X__ */
/* SecRow3CGFdaq_2005_CuCu22N */
  const Double_t SecRowDaq[12] = {
/* WE	*/ 0.0562349,-0.00071352,0.00899116,-0.00041805,
/* W	*/ 0.0468808,-0.000681293,0.0139564,-0.000545008,
/* E	*/ 0.0739047,-0.00140537,0.00633511,-0.000324046};
/* SecRow3CGFcucu22.W */
  const Double_t SecRowTpcRS[12] = {
/* WE	*/ 0.21711,0.000170942,0.318614,-6.66108e-05,
/* W	*/ 0.21719,0.000157678,0.316109,4.18145e-05,
/* E	*/ 0.216267,0.000258353,0.318745,-0.000125219};

#endif
  //
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
#if defined( __Version_N__ )
  row.SigmaJitterTI         = 0.2;//  for Tpx inner 
  row.SigmaJitterTO         = 0.2;//  for Tpx outer
#else
  row.SigmaJitterTI         = 0.0;//  for Tpx inner 
  row.SigmaJitterTO         = 0.0;//  for Tpx outer 
#endif
#if defined(__Version_O__)
  row.longitudinalDiffusion = 0.0370*1.64;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
#elif defined(  __Version_N__ ) || defined(__Version_P__) || defined(__Version_Q__)
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
#elif defined(  __Version_S__ )
  row.longitudinalDiffusion = 0.0540;
#elif defined(  __Version_U__ ) ||  defined(  __Version_V__ )
  row.longitudinalDiffusion = 0.0445;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
#else
  row.longitudinalDiffusion = 0.0370; // W
#endif
  row.transverseDiffusion   = 0.0775; //  cm/sqrt(cm)  
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
#if defined( __Version_N__ )
  row.OmegaTauScaleI        = 2.145;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8;    // effective reduction of OmegaTau near Outer sector anode wire
#elif defined(__Version_O__) 
  row.OmegaTauScaleI        = 2.145*0.304/0.354;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8;    // effective reduction of OmegaTau near Outer sector anode wire
#elif defined(__Version_P__)
  row.OmegaTauScaleI        = 2.145*1.275;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8*1.03;    // effective reduction of OmegaTau near Outer sector anode wire
#else
  row.OmegaTauScaleI        = 2.145*1.275*1.275;  // effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8*1.05;    // effective reduction of OmegaTau near Outer sector anode wire
#endif
#if defined(  __Version_V__ )
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] = SecRowTpcRS[i] - SecRowDaq[i];
  }
#else /* __Version_X__ */
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    Int_t j = i%4;
    a[i] = SecRowTpcRS[j] - SecRowDaq[j];
  }
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
