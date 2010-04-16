// $Id: TpcResponseSimulator.y2009.C,v 1.4 2010/04/16 19:31:19 fisyak Exp $
// $Log: TpcResponseSimulator.y2009.C,v $
// Revision 1.4  2010/04/16 19:31:19  fisyak
// Intermidiate version
//
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
/* SecRow3CGFRunIX66DEV_calib */
  const Double_t SecRowDaq[12] = {
/* WE	*/ 0.00178543,7.59862e-05,0.00359473,1.63784e-05,
/* W	*/ 0.00213429,5.44094e-05,0.00394723,-9.59276e-06,
/* E	*/ 0.00163108,8.36518e-05,0.0030858,4.26142e-05};
/* SecRow3CGFpp200.W */
  const Double_t SecRowTpcRS[12] = {
/* WE	*/ -0.0348841,-0.000377749,0.249109,-0.000362636,
/* W	*/ -0.0167217,0.000942187,0.237763,-0.000397571,
/* E	*/ -0.0498431,-0.00153771,0.258119,-0.000285127};
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
  row.SigmaJitterTI         = 0.0; //f  0.2;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0.0; //f  0.2;//ad  0.0;// b for Tpx outer 
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
  row.transverseDiffusion   = 0.06336*1.24;//f 0.06336; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        = 2.145*1.45;//f  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.06;//f    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
#if 0 /* d */
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = - TMath::Log(0.533*0.843485);
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = - TMath::Log(0.512*0.725267);
#else /* e */
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = 7.45056915017355381e-01;
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = 1.05699247000493290e+00;
#endif
#if 0
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] += SecRowTpcRS[i+4] - SecRowDaq[i+4];
  }
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
