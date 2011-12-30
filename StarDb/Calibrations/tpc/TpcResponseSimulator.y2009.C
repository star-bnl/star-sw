// $Id: TpcResponseSimulator.y2009.C,v 1.10 2011/12/30 16:11:12 fisyak Exp $
// $Log: TpcResponseSimulator.y2009.C,v $
// Revision 1.10  2011/12/30 16:11:12  fisyak
// Fix parameters from TpcRS_2009_pp200_P
//
// Revision 1.8  2011/10/11 19:09:23  fisyak
// Add Yi Guo's tables for Run XI AuAu200 RFF dE/dx calibration
//
// Revision 1.7  2010/10/29 16:04:05  fisyak
// Set proper t0 offset for Run IX
//
// Revision 1.6  2010/06/14 23:36:08  fisyak
// Freeze version V
//
// Revision 1.5  2010/05/24 16:07:20  fisyak
// Add default dE/dx calibration tables, replace TpcAltroParameters and asic_thresholds_tpx by tpcAltroParams
//
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
  row.SigmaJitterTI         = 0.25; // O: 0.0; //C: 0.5129;//0.0;//i 0.2; //g 0.0; //f  0.2;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0.25; // O: 0.2; //C: 0.4446;// 0.2;//i 0.2; //g 0.0; //f  0.2;//ad  0.0;// b for Tpx outer 
  row.SigmaJitterXI         = 0.1505; //N: 0.1105;// M: 0.0; //K: 0.1998;
  row.SigmaJitterXO         = 0.1343; //N: 0.126; // M: 0.0; //K: 0.0890;
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
  row.transverseDiffusion   = 0.06336;//*1.1585; // R:0.06336; //i 0.06336*1.24;//f 0.06336; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        =  7.9;//i 2.145*1.45*1.62;//g 2.145*1.45;//f  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        =  2.275;//i 1.8  *1.06*1.09;//g  1.8  *1.06;//f    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 ) E                F            M
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = - TMath::Log(0.533*0.843485) -3.87357e-01 + 2.86104e-01-2.60277e-01; // 
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = - TMath::Log(0.512*0.725267) -1.87626e-01 + 5.58456e-02-5.31300e-02;
  const Double_t SecRowTpcRS[12] = {
/* 	SecRow3CGFy2009_P.root */
/* WE */        0.319332-6.67437e-02,	-0.00187954,	0.267808+1.503243-02,	-0.000311204,
/* W  */	0.343838-6.67437e-02,	-0.000454393,	0.260619+1.503243-02,	-0.000636036,
/* E  */	0.298714-6.67437e-02,	-0.0026277,	0.281434+1.503243-02,	-0.000255795};
  const Double_t SecRowTpcRS2[4] = {
    /* SecRow3CGFTpcRS_2009_pp200_C */
    /* WE */ 1.27868e-01, 3.11811e-03, 4.91363e-01, 4.96008e-04
  };
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] += SecRowTpcRS[i+4] + SecRowTpcRS2[i%4];
  }
  // SecRow3CGFy2009_S: Inner  2.88735e-01, Outer 2.49370e-01;
  // FitP->Draw("sqrt(sigma**2-2.88735e-01**2):y>>IW","(i&&j&&j<14&&i<=12)/(dsigma**2)","profw")
  // FitP->Draw("sqrt(sigma**2-2.88735e-01**2):y>>IE","(i&&j&&j<14&&i>12)/(dsigma**2)","profwsame")
  // FitP->Draw("sqrt(sigma**2-2.49370e-01**2):y>>OW","(i&&j&&j>=14&&i<=12)/(dsigma**2)","profwsame")
  // FitP->Draw("sqrt(sigma**2-2.49370e-01**2):y>>OE","(i&&j&&j>=14&&i>12)/(dsigma**2)","profwsame")
  // FitP->Draw("sqrt(sigma**2-2.49370e-01**2):y>>O","(i&&j&&j>=14)/(dsigma**2)","profwsame")
  const Double_t RowSigmaTrs[8] = {//  2.88735e-01, 2.49370e-01 
    /* W  */  1.94548e-01 + 1.39259e-01,  0.         ,  // Inner
    /* WE */  1.34204e-01 + 8.03318e-02,  3.83899e-04,  // Outer
    /* E  */  7.77511e-02 + 1.39259e-01,  3.94192e-03,  // Inner
    /* WE */  1.34204e-01 + 8.03318e-02,  3.83899e-04}; // Outer
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i];
  }
  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
  //  row.T0offset   = 0.25; // From Xianglei Zhu for Run IX
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // TpcT->Draw("fMcHit.mPosition.mX3-fRcHit.mPosition.mX3:fMcHit.mPosition.mX3>>Z(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // The corection has to be added               M                  N
  row.T0offset   =  0.25+4.62419e-01 - 1.17813 + 0.226594 +  0.0306463; 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
