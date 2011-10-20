// $Id: TpcResponseSimulator.y2010.C,v 1.8 2010/10/28 23:41:54 fisyak Exp $
// $Log: TpcResponseSimulator.y2010.C,v $
// Revision 1.8  2010/10/28 23:41:54  fisyak
// extra t0 off set for Altro chip
//
// Revision 1.7  2010/06/14 23:36:08  fisyak
// Freeze version V
//
// Revision 1.6  2010/05/24 21:39:53  fisyak
// Fix bracket
//
// Revision 1.5  2010/05/24 16:07:20  fisyak
// Add default dE/dx calibration tables, replace TpcAltroParameters and asic_thresholds_tpx by tpcAltroParams
//
// Revision 1.4  2010/04/19 15:05:58  fisyak
// Final (2010_i) parameters for Run X
//
// Revision 1.3  2010/04/16 19:31:19  fisyak
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
  row.SigmaJitterTI         = 0.25;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0.25;//ad  0.0;// b for Tpx outer 
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
  row.transverseDiffusion   = 0.06336*1.1585; //i; 0.06336*1.17; //h 0.06336; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        = 2.145*1.515;  //i; 2.145*1.4;  //h 2.145;  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.201;  //i 1.8  *1.1;    //h 1.8;    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = - TMath::Log(0.533*0.843485);
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = - TMath::Log(0.512*0.725267);
  const Double_t SecRowTpcRS[12] = {
/* 	SecRow3CGFy2010_Q.root */
/* WE */	0.326678,	-0.00249766,	0.0617192,	-0.000377025,
/* W  */	0.32259,	-0.0025717,	0.0582606,	-0.000529371,
/* E  */	0.335552,	-0.00149708,	0.0672868,	-0.000257523};
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] += SecRowTpcRS[i+4];
  }
  //                                           SecRow3CGFRunX09P10i_AuAu SecRow3CGFy2010_R 
  // SecRow3CGFy2010_R : Inner = 2.73752e-01; Outer = 2.51855e-01;
  // SecRow3CGFRunX09P10i_AuAu
  //   FitP->Draw("sqrt(sigma**2-2.73752e-01**2):y>>DIi","(i&&j&&j<14)/(dsigma**2)","profwsame")
  //   FitP->Draw("sqrt(sigma**2-2.51855e-01**2):y>>DOo","(i&&j&&j>=14)/(dsigma**2)","profwsame")
  const Double_t RowSigmaTrs[4] = {//  2.88735e-01, 2.49370e-01 
    /* WE */  2.19714e-01, -3.33329e-03,  // Inner
    /* WE */  1.39679e-01,  7.26005e-04}; // Outer
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i%4];
  }
  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
  row.T0offset   = 0.50; // From  Lokesh Kumar for Run X
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
