TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXCorrection",7);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar        =      5; // Outer SecRowMipFitpHist295P02gh1.hold.root
  row.a[0]	 =  1.87835e+00;//  +/-   1.25105e-03
  row.a[1]	 = -4.85849e+00;//  +/-   1.35624e-03
  row.a[2]	 =  4.78784e+00;//   +/-   1.00622e-03
  row.a[3]	 = -2.07671e+00;//   +/-   6.80132e-04
  row.a[4]	 =  3.21299e-01;//   +/-   3.31870e-04
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar        =      5;     // Inner SecRowMipFitpHist295P02gh1.hold.root
  row.a[0]	 =  1.42636e-01;//   +/-   2.65872e-03 
  row.a[1]	 = -4.69133e-01;//   +/-   1.16550e-02 
  row.a[2]	 =  6.56511e-01;//   +/-   1.90594e-02 
  row.a[3]	 = -4.08199e-01;//   +/-   2.69925e-02 
  row.a[4]	 =  2.17271e-03;//   +/-   2.00504e-02 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // Additional correction for Inner and Outer
  row.npar       =  3;  // SecRowMipFitpHist298P02gh1.root  correction to most probable value vs log2(dx)
  row.a[0]	 = -3.58584e-02;
  row.a[1]	 =  4.16084e-02;
  row.a[2]	 = -1.45163e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =  2;  // SecRowMipFitpHist298P02gh1.root  correction to sigma (Outer) vs log2(dx)
  row.min        =  0;
  row.max        =  2;
  row.a[0]	 = -2.31862e-01;//   +/-    1.50778e-03
  row.a[1]	 =  9.89624e-02;//   +/-    1.19805e-03
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =  3;  // SecRowMipFitpHist298P02gh1.root  correction to sigma (Inner) vs log2(dx)
  row.min        =  0;
  row.max        =  1;
  row.a[0]	 = -1.42232e-01;//   +/-   7.74809e-03
  row.a[1]	 =  2.08279e-01;//   +/-   3.27172e-02
  row.a[2]	 = -1.43445e-01;//   +/-   3.08671e-02
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =  2;  // SecRowMipFitpHist302P02gh1.root correction to mu (Outer) vs log2(dx)
  row.a[0]	 =  1.84010e-02;//   +/-   1.26864e-03
  row.a[1]	 = -1.99563e-02;//   +/-   9.56681e-04
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =  2;  // SecRowMipFitpHist302P02gh1.root  correction to mu (Inner) vs log2(dx)
  row.a[0]	 = -1.85573e-03;//   +/-    1.68953e-03 
  row.a[1]	 = -1.46755e-02;//   +/-    3.65158e-03 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
