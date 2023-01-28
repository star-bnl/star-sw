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
  row.AveragePedestalRMS    = -1.0; // Old Tpc electronics or iTPC  1.4 => 1.0; Tonko 12/12/2019
  row.AveragePedestalRMSX   = -1.0; // New Tpx electronics 
  row.tauIntegration        = 2.5*74.6e-9;//   secs 
  row.tauF                  = 394.0e-9;// secs Tpc 
  row.tauP                  = 775.0e-9;// secs Tpc 
  row.tauXI                 =  60.0e-9;// secs Tpx Inner integration time 
  row.tauXO                 =  74.6e-9;// secs Tpx Outer integration time 
  row.tauCI                 =   0;  
  row.tauCO                 =   0;  
  row.SigmaJitterTI         = 0.00000;// 0.4317;// 0.25;//ad  0.0;// b for Tpx inner 
  row.SigmaJitterTO         = 0.00000;// 0.4300;// E: 0.4801;//0.25;//ad  0.0;// b for Tpx outer 
  row.SigmaJitterXI         = 0.03426;// 0.1027785; // P: 0.1353*1.05/1.10; //O: 0.1353*1.05;// N: 0.1353; // C:0.;
  row.SigmaJitterXO         = 0.03426;// 0.107525;  // P: 0.1472*1.05/1.03; //O: 0.1472*1.05;// N: 0.1472; // C:0.;
  row.longitudinalDiffusion = 0.03624*1.3; // Magboltz // HD 0.03624*1.5; //HC 0.03624; // Magboltz 
  row.longitudinalDiffusionI= row.longitudinalDiffusion;
  row.transverseDiffusion   = 0.02218*TMath::Sqrt(1 + row.OmegaTau*row.OmegaTau) ; // Magboltz 87% Ar + 10% CH4 + 3%CF4
  row.transverseDiffusionI  = 0.97*row.transverseDiffusion;
  row.NoElPerAdc            = 335.;   // No. of electrons per 1 ADC count
  row.OmegaTauScaleI        =   0;//2.145*1.515;// HC 1.;// 2.145*1.515;  //i; 2.145*1.4;  //h 2.145;  //ad 2.145*1.25;  //b effective reduction of OmegaTau near Inner sector anode wire
  row.OmegaTauScaleO        =   0;//1.8  *1.201;  //HC 1.;// 1.8  *1.201;  //i 1.8  *1.1;    //h 1.8;    //ad 1.8  *1.25;  //b effective reduction of OmegaTau near Outer sector anode wire
  // Inner_wire_to_plane_coupling ( 0.533 ) * Inner_wire_to_plane_couplingScale ( 0.843485 )
  // Outer_wire_to_plane_coupling ( 0.512 ) * Outer_wire_to_plane_couplingScale ( 0.725267 )
  row.SecRowCorIW[0] = row.SecRowCorIE[0] = 0.57692996501735538 + 5.13440e-02 - 0.23; 
  row.SecRowCorIW[1] = row.SecRowCorIE[1] = -5.40702e-04;
  row.SecRowCorOW[0] = row.SecRowCorOE[0] = 1.11982875000493309-1.27992e-01 + 2.52297e-02 - 0.135;
  row.SecRowCorOW[1] = row.SecRowCorOE[1] = -3.73511e-04;
  // SecRow3CGF7p7GeV_2021.root: FitP->Draw("sigma:y","i&&j","prof")
  //                                     Inner            3.10477e-01, Outer   2.70452e-01
  // RunXXI/Hijing.2021AuAu200.VMCF
  //                                     Inner            2.93912e-01  Iuter   2.76624e-01    
  const Double_t RowSigmaTrs[4] = {
    9.13675e-02, 0,  // Inner
    6.29849e-02, 0}; // Outer
  Float_t *b = &row.SecRowSigIW[0];
  for (Int_t i = 0; i < 8; i++) {
    b[i] = RowSigmaTrs[i%4];
  }
  row.PolyaInner = 1.38;
  row.PolyaOuter = 1.38;
  row.T0offset   = 0.50 -1.43663e-01 -0.00932877 + 0.0416;//g // 01/18/12 Xianglei Zhu from Run 11 AuAu 27 & 19.6 GeV embedding 
  // row.T0offsetI  = -1.58786167960479896e-01;
  // row.T0offsetO  = -1.94071983062808762e-01-1.76429075511644329e-02;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
