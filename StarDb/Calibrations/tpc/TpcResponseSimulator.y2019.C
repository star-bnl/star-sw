// $Id: TpcResponseSimulator.y2019.C,v 1.4 2020/04/03 23:47:14 fisyak Exp $
// $Log: TpcResponseSimulator.y2019.C,v $
// Revision 1.4  2020/04/03 23:47:14  fisyak
// rename tpcSectorT0offset.20190223.080000.C to tpcSectorT0offset.20190101.000000.C, remove intermidiate tpcSectorT0offset.20190*.C
//
// Revision 1.3  2019/05/23 11:50:01  fisyak
// Add default TpcAdcCorrectionMDF, 2019 version of TpcResponseSimulator
//
// Revision 1.2  2019/04/16 19:29:34  fisyak
// Run XIX preliminary dE/dx calibration
//
// Revision 1.1  2018/02/16 20:56:50  perev
// iTPC
//
// Revision 1.1  2017/02/07 16:58:36  fisyak
// Clean up
//
// Revision 1.1  2012/09/13 21:06:27  fisyak
// Default tables for devT
//
// Revision 1.1  2012/04/27 00:31:31  perev
// All defE tables
//
// Revision 1.8  2012/04/11 14:21:55  fisyak
// Fix T0offset from comparison with AuAu27
//
// Revision 1.7  2012/04/03 14:06:55  fisyak
// Speed up using  GetSaveL (__PAD_BLOCK__), sluggish shape histograms, Heed electron generation
//
// Revision 1.6  2012/01/18 13:57:57  fisyak
// Adjust T0offset : Xianglei Zhu from Run 11 AuAu 27 & 19.6 GeV embedding
//
// Revision 1.5  2011/12/30 00:04:01  fisyak
// Freeze parameters for y2011 base on TpcRS_2011_pp500LowLum_Q
//
// Revision 1.3  2011/10/11 19:09:23  fisyak
// Add Yi Guo's tables for Run XI AuAu200 RFF dE/dx calibration
//
// Revision 1.2  2010/10/28 23:41:54  fisyak
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
  row.AveragePedestalRMS    = -1.0; // Tpx, Use St_TpcPadPedRMSC chair
  row.AveragePedestalRMSX   = -1.0; // iTPC, -"- 
  row.tauIntegration        = 2.5*74.6e-9;//   secs 
  row.tauF                  = 394.0e-9;// secs Tpc 
  row.tauP                  = 775.0e-9;// secs Tpc 
  row.tauXI                 =  60.0e-9;// secs Tpx Inner integration time 
  row.tauXO                 =  74.6e-9;// secs Tpx Outer integration time 
  row.tauCI                 =   0;  
  row.tauCO                 =   0;  
  row.SigmaJitterTI         = 0.45;
  row.SigmaJitterTO         = 0.45; 
  row.SigmaJitterXI         = 0.07;
  row.SigmaJitterXO         = 0.00; //0.107525/2.; // 0.107525;  // P: 0.1472*1.05/1.03; //O: 0.1472*1.05;// N: 0.1472; // C:0.;
  row.longitudinalDiffusion = 0.03624; // Magboltz // HD 0.03624*1.5; //HC 0.03624; // Magboltz 
  row.longitudinalDiffusionI= row.longitudinalDiffusion;// *0.63;
  row.transverseDiffusion   = 0.02218*TMath::Sqrt(1 + row.OmegaTau*row.OmegaTau) ; // Magboltz
  row.transverseDiffusionI  = 0.91*row.transverseDiffusion;// 0.66  *row.transverseDiffusion;
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        =  2.145*1.515;// HC 1.;// 2.145*1.515;  //i; 2.145*1.4;  //h 2.145;  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.201;  //HC 1.;// 1.8  *1.201;  //i 1.8  *1.1;    //h 1.8;    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
  row.SecRowCorIW[0] = row.SecRowCorIE[0] =  6.21777e-01 -4.23291e-02 + 1.31883e-02 -6.37730e-02;
  row.SecRowCorIW[1] = row.SecRowCorIE[1] = -1.54623e-03 +1.26327e-03 + 4.57111e-04;
  row.SecRowCorOW[0] = row.SecRowCorOE[0] =  9.59474e-01 +2.50905e-02 + 1.21909e-02 +8.57133e-04;
  row.SecRowCorOW[1] = row.SecRowCorOE[1] =  1.72672e-03 -1.97991e-03; -1.25414e-04;
  // SecRow3CGFdaq_2011_pp500LowLum => Inner: 3.26428e-01 - -5.01720e-04*y; Outer: 2.68883e-01 + 1.23403e-04*y
  //                                          3.22907e-01                          2.72715e-01
  // SecRow3CGFTpcRS_2011_pp500LowLum_f     : 3.09711e-01                          2.65342e-01
  // diff                                   : 9.13675e-02                          6.29849e-02
  // SecRow3CGFTpcRS_2011_pp500LowLum_g     : 3.12857e-01                          2.67379e-01
  const Double_t RowSigmaTrs[4] = {
    0.09, 0,  // 0.2313675,   0,  // 9.13675e-02, 0,  // Inner // 9.13675e-02+0.14
    0.11, 0}; //6.29849e-02, 0}; // Outer
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i%4];
  }
  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
  //  row.T0offset   = 0.50; // From  Lokesh Kumar for Run X
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // TpcT->Draw("fMcHit.mPosition.mX3-fRcHit.mPosition.mX3:fMcHit.mPosition.mX3>>Z(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // The corection has to be added                                                                    M             P
  //row.T0offset   = 0.50 + 1.65431e-01 -  3.45247e-01 -1.54583e+00 -2.90686e-03+ 1.54353e+00 + 0.0191135  -1.20938e-03 ; //E
  // row.T0offset   = 0.50 -1.43663e-01 -0.00932877;//g // 01/18/12 Xianglei Zhu from Run 11 AuAu 27 & 19.6 GeV embedding 
  row.T0offset   = 0.50 -1.43663e-01 -0.00932877 + 0.0416;//g // 12/18/19 from Run 19 AuAu19.6 GeV simulation
  row.T0offsetI  = -1.58786167960479896e-01;
  row.T0offsetO  = -1.94071983062808762e-01-1.76429075511644329e-02;

  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
