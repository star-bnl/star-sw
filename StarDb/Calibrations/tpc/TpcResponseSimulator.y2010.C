// $Id: TpcResponseSimulator.y2010.C,v 1.11 2011/12/30 17:13:05 fisyak Exp $
// $Log: TpcResponseSimulator.y2010.C,v $
// Revision 1.11  2011/12/30 17:13:05  fisyak
// Fix parameters from TpcRS_2010_AuAu200_Q
//
// Revision 1.9  2011/10/11 19:09:23  fisyak
// Add Yi Guo's tables for Run XI AuAu200 RFF dE/dx calibration
//
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
  row.SigmaJitterTI         = 0.43; //M: 0.25;// K: 0.5*(0.25+0.57) + 0.56;//E: C: 0.57;// 0.25;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0.43; //M: 0.25;// K: 0.8*0.51 + 0.46;       //E: C: 0.51;// 0.25;//ad  0.0;// b for Tpx outer 
  row.SigmaJitterXI         = 0.;
  row.SigmaJitterXO         = 0.;
  row.longitudinalDiffusion = 0.0370;// 0.0232;//K  0.0370;J // cm/sqrt(cm)   ; // 0.0232; from Laser Fit
  row.transverseDiffusion   = 0.06336*1.1585;// K: 0.06336/1.05; //E : *1.1585; // C: 0.06336;// *1.1585; //i; 0.06336*1.17; //h 0.06336; //  cm/sqrt(cm)  ; from Field data fit with OmegaTau = 3.02 // 0.0633
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        = 2.145*1.515;  //i; 2.145*1.4;  //h 2.145;  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.201;  //i 1.8  *1.1;    //h 1.8;    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = - TMath::Log(0.533*0.843485);
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = - TMath::Log(0.512*0.725267);
#if 0
  const Double_t SecRowTpcRS[12] = {
/* 	SecRow3CGFy2010_Q.root */
/* WE */	0.326678,	-0.00249766,	0.0617192,	-0.000377025,
/* W  */	0.32259  -2.60391e-02,	-0.0025717 +6.62806e-03,	0.0582606 +9.95772e-03,	-0.000529371+2.33701e-04,
/* E  */	0.335552 -1.28454e-02,	-0.00149708+3.78585e-03,	0.0672868 -2.91205e-03,	-0.000257523+1.86841e-04};
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] += SecRowTpcRS[i+4];
  }
#else
  const Double_t SecRowTpcRS[4] = {
    3.27541e-01, 2.52307e-03,
    6.13475e-02, 0
  };
  Float_t *a = &row.SecRowCorIW[0];
  for (Int_t i = 0; i < 8; i++) {
    a[i] += SecRowTpcRS[i%4];
  }
#endif
#if 0
  //                                           SecRow3CGFRunX09P10i_AuAu SecRow3CGFy2010_R 
  // SecRow3CGFy2010_R : Inner = 2.73752e-01; Outer = 2.51855e-01;
  // SecRow3CGFRunX09P10i_AuAu
  //   FitP->Draw("sqrt(sigma**2-2.73752e-01**2):y>>DIi","(i&&j&&j<14)/(dsigma**2)","profwsame")
  //   FitP->Draw("sqrt(sigma**2-2.51855e-01**2):y>>DOo","(i&&j&&j>=14)/(dsigma**2)","profwsame")
  const Double_t RowSigmaTrs[4] = {//  2.88735e-01, 2.49370e-01 
    /* WE */  2.19714e-01 + 1.63694e-01 -1.11309e-01 -1.13876e-01, -3.33329e-03,  // Inner
    /* WE */  1.39679e-01 + 0.88994e-01 -1.16375e-01 +6.27260e-02,  7.26005e-04}; // Outer
  // Sigma => SecRow3CGFTpcRS_2010_AuAu200 : Inner = 3.30597e-01; Outer = 2.85255e-01;
  //          SecRow3CGFdaq_2010_AuAu200:    Inner = 3.68904e-01; Outer = 2.98815e-01;
  // D        SecRow3CGFTpcRS_2010_AuAu200_D:Inner = 3.85331e-01;         3.20677e-01;
  // diff                                           -1.11309e-01         -1.16375e-01
  // E                                               3.49660e-01          2.78273e-01
  // dif                                            -1.13876e-01          6.27260e-02
  //________________________________________________________________________________
  // FitP->Draw("sigma**2:y>>D","(i&&j&&y<13.5)/(2*sigma*dsigma)**2","profg")
  // FitP->Draw("sigma**2:y>>DO","(i&&j&&y>13.5)/(2*sigma*dsigma)**2","profg")
  // TF1 *f = new TF1("f","([0]+[1]*x)**2",0.5,13.5)
  // TF1 *fO = new TF1("fO","([0]+[1]*x)**2",13.5,45.5)
  // sigma**2 = ([0]+[1]*x)**2             Inner   [0]          [1]      Outer [0]           [1]  
  //   SecRow3CGFdaq_2011_pp500LowLum      3.26431e-01 -5.49697e-04    2.66196e-01   1.77955e-04
  //   SecRow3CGFTpcRS_2011_pp500LowLum_K  
#else
  // sigma**2 = ([0]+[1]*x)**2             Inner   [0]          [1]      Outer [0]           [1]  
  //   SecRow3CGFdaq_2011_pp500LowLum      3.26431e-01 -5.49697e-04    2.66196e-01   1.77955e-04
  //   SecRow3CGFTpcRS_2011_pp500LowLum_K  
  /* FitP->Draw("sigma**2:y>>D","(i&&j)/(2*sigma*dsigma)**2","profg")
root.exe [73] D->Fit("pol2","er","",0.5,13.5)
 FCN=1.52206e+06 FROM MINOS     STATUS=SUCCESSFUL     16 CALLS         147 TOTAL
                     EDM=7.91103e-13    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.46662e-01   2.96885e-06   0.00000e+00   2.63078e-01
   2  p1          -2.35295e-03   8.10244e-07  -0.00000e+00   1.14003e+02
   3  p2           1.02061e-04   5.14867e-08   5.14867e-08   1.57589e-01
root.exe [74] D->Fit("pol1","er+","",13.5,45.5)
 FCN=2.91377e+07 FROM MINOS     STATUS=SUCCESSFUL     12 CALLS          73 TOTAL
                     EDM=3.96657e-11    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           8.81584e-02   3.32454e-07  -2.15080e-13  -7.12965e-03
   2  p1           3.60368e-05   1.11022e-08   1.11022e-08  -3.71486e+01
cd TpcRS_2010_AuAu200_N/SecRow3CGFTpcRS_2010_AuAu200_N.root
root.exe [76] FitP->Draw("sigma:y>>s","i&&j","prof")
(Long64_t)1001
root.exe [77] s->Fit("pol0","er","",0.5,13.5)
 FCN=22.7733 FROM MINOS     STATUS=SUCCESSFUL      2 CALLS          20 TOTAL
                     EDM=3.72763e-13    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           3.03427e-01   3.92322e-04   3.92322e-04  -2.20084e-03
(class TFitResultPtr)175982136
root.exe [78] s->Fit("pol0","er+","",13.5,45.5)
 FCN=39.7332 FROM MINOS     STATUS=SUCCESSFUL      2 CALLS          22 TOTAL
                     EDM=1.75953e-13    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           2.58487e-01   2.03883e-04   2.03883e-04   2.90960e-03
root.exe [79] FitP->Draw("sqrt(sigma**2-(1.46662e-01+-2.35295e-03*y+1.02061e-04*y**2)):y>>sy","i&&j","prof")
root.exe [81] sy->Fit("pol1","er","",0.5,13.5)
 FCN=39.0095 FROM MINOS     STATUS=SUCCESSFUL     10 CALLS          64 TOTAL
                     EDM=5.4693e-20    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           2.29427e-01   1.11897e-03  -0.00000e+00   5.63017e-05
   2  p1          -2.57249e-03   1.42529e-04   1.42529e-04   2.84905e-07

  */     
  const Double_t RowSigmaTrs[4] = {
    // P:    2.29427e-01, -2.57249e-03, // Inner 
    // P: 4.45339e-02, 0};           // Outer
    2.7318e-01, -2.57249e-03, // Inner 
    1.5301e-01, 0};           // Outer
#endif
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i%4];
  }
  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
  // row.T0offset   = 0.50; // From  Lokesh Kumar for Run X
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // TpcT->Draw("fMcHit.mPosition.mX3-fRcHit.mPosition.mX3:fMcHit.mPosition.mX3>>Z(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // The corection has to be added
  row.T0offset   = 0.3 + 0.0305453;// M //K :  0.50+1.65131e-01-3.39101e-01-1.54608e+00+1.54587e+00 +  -0.0256972; // K
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
