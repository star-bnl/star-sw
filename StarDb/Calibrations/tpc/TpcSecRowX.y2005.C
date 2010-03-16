TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // TpcSecRow Allocated rows: 24  Used rows: 24  Row size: 360 bytes
  //  Table: TpcSecRowCor_st[0]--> TpcSecRowCor_st[23]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  //  Correction for simulation with respect to data
  if (!gROOT->GetClass("St_TpcSecRowCor")) return 0;
  TpcSecRowCor_st row;
  St_TpcSecRowCor *tableSet = new St_TpcSecRowCor("TpcSecRowX",24);
  //
  const Double_t SecRowCor[8] = {
    -7.89396e-02, 2.28200e-03, // w->Fit("pol1","er","",0,13.5)
     1.89021e-02,-2.83586e-04, // w->Fit("pol1","er+","",13.5,45.5)
    -9.98014e-02,-1.95242e-03, // e->Fit("pol1","er","",0,13.5)
     4.96322e-02,-4.82296e-04  // e->Fit("pol1","er+","",13.5,45.5)
  };
  const Double_t SecRowCor2[8] = {
    2.39200e-01, -1.59649e-03, // w->Fit("pol1","er","",0,13.5)
    7.47663e-02,  1.60879e-04, // w->Fit("pol1","er+","",13.5,45.5)
    2.59956e-01,  2.37269e-03, // e->Fit("pol1","er","",0,13.5)
    3.79400e-02,  4.37099e-04  // e->Fit("pol1","er+","",13.5,45.5)
  };
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t sec = 1; sec <= 24; sec++) {
    for (Int_t r = 1; r <= 45; r++) { 
      Int_t indx = 0;
      if (sec > 12) indx  = 4;
      if (r   > 13) indx += 2;
      Double_t SecRowCorGain = (SecRowCor[indx] + SecRowCor2[indx]) + (SecRowCor[indx+1]+SecRowCor2[indx+1])*r;
      row.GainScale[r-1]     = TMath::Exp(-SecRowCorGain);
    }
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
