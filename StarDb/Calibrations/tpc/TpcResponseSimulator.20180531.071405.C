// $Id: TpcResponseSimulator.20180531.071405.C,v 1.2 2020/05/23 20:57:26 fisyak Exp $
// $Log: TpcResponseSimulator.20180531.071405.C,v $
// Revision 1.2  2020/05/23 20:57:26  fisyak
// Guannan's checked version
//
// Revision 1.1  2020/05/17 15:46:34  fisyak
// 3p85GeV_fixedTarget_2018
//
// Revision 1.2  2020/03/18 20:10:35  iraklic
// new Run17 params and Run18 params after separating sector20 from the rest
//
// Revision 1.3  2018/10/17 20:45:17  fisyak
// Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
//
// Revision 1.1  2018/08/06 15:41:44  fisyak
// RunXVIII isobar dE/dx calibration
//
// Revision 1.1  2017/02/07 16:58:37  fisyak
// Clean up
//
// Revision 1.1  2016/09/19 12:25:07  fisyak
// Set/ReSet default values
//
// Revision 1.1  2014/01/27 14:35:11  fisyak
// Add y2014 simulation (ideal) tables for y2014
//
// Revision 1.1  2014/01/09 23:15:10  fisyak
// Run XIII, pp500p1
//
// Revision 1.3  2013/11/08 16:18:25  fisyak
// T0 instruction
//
// Revision 1.2  2013/02/01 15:58:51  fisyak
// Add handle for separate Inner and Outer sector time off set
//
// Revision 1.1  2012/04/03 14:06:55  fisyak
// Speed up using  GetSaveL (__PAD_BLOCK__), sluggish shape histograms, Heed electron generation
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
  row.AveragePedestalRMS    = 1.4; // Old Tpc electronics or iTPC
  row.AveragePedestalRMSX   = 0.7; // New Tpx electronics 
  row.tauIntegration        = 2.5*74.6e-9;//   secs 
  row.tauF                  = 394.0e-9;// secs Tpc 
  row.tauP                  = 775.0e-9;// secs Tpc 
  row.tauXI                 =  60.0e-9;// secs Tpx Inner integration time 
  row.tauXO                 =  74.6e-9;// secs Tpx Outer integration time 
  row.tauCI                 =   0;  
  row.tauCO                 =   0;  
  row.SigmaJitterTI         = 0;// 0.4317;// 0.25;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0;// 0.4300;// E: 0.4801;//0.25;//ad  0.0;// b for Tpx outer 
  row.SigmaJitterXI         = 0;// 0.1027785; // P: 0.1353*1.05/1.10; //O: 0.1353*1.05;// N: 0.1353; // C:0.;
  row.SigmaJitterXO         = 0;// 0.107525;  // P: 0.1472*1.05/1.03; //O: 0.1472*1.05;// N: 0.1472; // C:0.;
  row.longitudinalDiffusion = 0.03624; // Magboltz // HD 0.03624*1.5; //HC 0.03624; // Magboltz 
  row.transverseDiffusion   = 0.02218*TMath::Sqrt(1 + row.OmegaTau*row.OmegaTau) ; // Magboltz
  row.NoElPerAdcI           = 335./TMath::Exp(-0.81625);   // No. of electrons per 1 ADC count for outer TPX
  row.NoElPerAdcO           = 335./TMath::Exp(-1.09801);   // No. of electrons per 1 ADC count for outer TPX
  row.NoElPerAdcX           = 335./TMath::Exp(-0.49463);;  // No. of electrons per 1 ADC count for iTPC 
  row.OmegaTauScaleI        =  2.145*1.515;// HC 1.;// 2.145*1.515;  //i; 2.145*1.4;  //h 2.145;  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        = 1.8  *1.201;  //HC 1.;// 1.8  *1.201;  //i 1.8  *1.1;    //h 1.8;    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
#if 1
  row.SecRowCorIW[0] = 0.039 -0.032165 ;//- TMath::Log(0.533*0.843485) -5.84129e-01 + 4.52885e-01 + 3.09117e-02; // IRAKLI
  row.SecRowCorIE[0] = 0.039 -0.073489 ;//- TMath::Log(0.533*0.843485) -5.84129e-01 + 4.52885e-01 + 3.09117e-02; // IRAKLI
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = 0.105 -0.0986937;//- TMath::Log(0.512*0.725267) -5.47141e-01 + 5.23937e-01 + 1.19154e-02; // IRAKLI
#endif
  // SecRow3CGFdaq_2011_pp500LowLum => Inner: 3.26428e-01 - -5.01720e-04*y; Outer: 2.68883e-01 + 1.23403e-04*y
  //                                          3.22907e-01                          2.72715e-01
  // SecRow3CGFTpcRS_2011_pp500LowLum_f     : 3.09711e-01                          2.65342e-01
  // diff                                   : 9.13675e-02                          6.29849e-02
  // SecRow3CGFTpcRS_2011_pp500LowLum_g     : 3.12857e-01                          2.67379e-01
#if 0
  const Double_t RowSigmaTrs[4] = {
    9.13675e-02, 0,  // Inner
    6.29849e-02, 0}; // Outer
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i%4];
  }

  /* Sigmas 
     Tpx inner = 0.395
     Tpx outer = 0.314
     iTpc      = 0.322 Outer  0.316

  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
#endif
#if 0
  //  row.T0offset   = 0.50; // From  Lokesh Kumar for Run X
  // TpcT->Draw("fMcHit.mMcl_t+0.165*Frequency-fRcHit.mMcl_t/64:fMcHit.mPosition.mX3>>T(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // TpcT->Draw("fMcHit.mPosition.mX3-fRcHit.mPosition.mX3:fMcHit.mPosition.mX3>>Z(210,-210,210,100,-2,3)","fNoMcHit==1&&fNoRcHit==1&&fRcHit.mQuality>90","colz")
  // The corection has to be added                                                                    M             P
  //row.T0offset   = 0.50 + 1.65431e-01 -  3.45247e-01 -1.54583e+00 -2.90686e-03+ 1.54353e+00 + 0.0191135  -1.20938e-03 ; //E
  row.T0offset   = 0.50 -1.43663e-01;//g // 01/18/12 Xianglei Zhu from Run 11 AuAu 27 & 19.6 GeV embedding 
  // root.exe T0offset.C
  // TI->FitSlicesY(); TI_1->Fit("pol2","er","",-100,100);
  row.T0offsetI =  1.17437e-01 + 8.43584e-03; 
  // TO->FitSlicesY(); TO_1->Fit("pol2","er","",-100,100);
  row.T0offsetO = -9.36725e-03 + 5.74947e-03;
#else
// You have to add -0.73383303057411764 (average offset from Irakli’s correction)
  // TI->FitSlicesY(); TI_1->Fit("pol2","er","",-100,100);
  row.T0offsetI =  1.17437e-01 + 8.43584e-03 - 0.73383303057411764; 
  // TO->FitSlicesY(); TO_1->Fit("pol2","er","",-100,100);
  row.T0offsetO = -9.36725e-03 + 5.74947e-03 - 0.73383303057411764;
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
