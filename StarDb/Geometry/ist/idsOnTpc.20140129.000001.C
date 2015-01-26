TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st row;
  St_Survey *tableSet = new St_Survey("idsOnTpc",1);
  memset(&row,0,tableSet->GetRowSize());
  row.Id	 =          1;
  row.r00	 =       1;
  row.r01	 =       4.69412e-05;
  row.r02	 =       -1.38009e-05;
  row.r10	 =       -4.69107e-05;
  row.r11	 =       0.999998;
  row.r12	 =       0.00215842;
  row.r20	 =       1.39021e-05;
  row.r21	 =       -0.00215843;
  row.r22	 =       0.999998;
  row.t0	 =         -0.041854;
  row.t1	 =         0.0403392;
  row.t2	 =         0.109516;
  row.sigmaRotX	 =      0.001;
  row.sigmaRotY	 =      0.001;
  row.sigmaRotZ	 =      0.001;
  row.sigmaTrX	 =      0.001;
  row.sigmaTrY	 =      0.001;
  row.sigmaTrZ	 =      0.001;
  memcpy(&row.comment,"alignment",9);
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
