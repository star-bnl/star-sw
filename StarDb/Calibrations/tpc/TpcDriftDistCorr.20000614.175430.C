TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// StarDb/.make/StarDb/.data/StarDb/Calibration/tpc//TpcDriftDistCorr 
//  Table: TpcDriftDistCorr_st[0]--> TpcDriftDistCorr_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcDriftDistCorr")) return 0;
  St_TpcDriftDistCorr *tableSet = new St_TpcDriftDistCorr("TpcDriftDistCorr",48);
  TpcDriftDistCorr_st row;
  int i = 0;
    row.a0 = -0.037788; row.a1 = 0.000869; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.022816; row.a1 = 0.000702; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.020484; row.a1 = 0.000534; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.029742; row.a1 = 0.000550; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.024068; row.a1 = 0.000645; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.027231; row.a1 = 0.000578; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.022447; row.a1 = 0.000594; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.027390; row.a1 = 0.000650; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 =  0.027573; row.a1 = 0.000511; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.025293; row.a1 = 0.000646; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.011729; row.a1 = 0.000639; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.019621; row.a1 = 0.000594; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.027432; row.a1 = 0.000745; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.024144; row.a1 = 0.000551; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.025532; row.a1 = 0.000672; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.023314; row.a1 = 0.000601; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.034223; row.a1 = 0.000903; row.a2 = -0.000004; tableSet->AddAt(&row, i++);
    row.a0 = -0.016366; row.a1 = 0.000633; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.023732; row.a1 = 0.000724; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.028884; row.a1 = 0.000622; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.025180; row.a1 = 0.000707; row.a2 = -0.000003; tableSet->AddAt(&row, i++);
    row.a0 = -0.029355; row.a1 = 0.000585; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.023333; row.a1 = 0.000641; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.022745; row.a1 = 0.000601; row.a2 = -0.000002; tableSet->AddAt(&row, i++);
    row.a0 = -0.045882; row.a1 = 0.001056; row.a2 = -0.000004; tableSet->AddAt(&row, i++);
    row.a0 = -0.000427; row.a1 = 0.000183; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.001999; row.a1 = 0.000170; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.002645; row.a1 = 0.000208; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.002202; row.a1 = 0.000157; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.001416; row.a1 = 0.000173; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.001380; row.a1 = 0.000160; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.002135; row.a1 = 0.000174; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.002181; row.a1 = 0.000195; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.002683; row.a1 = 0.000190; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.000303; row.a1 = 0.000176; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.001794; row.a1 = 0.000223; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.006403; row.a1 = 0.000265; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.003742; row.a1 = 0.000180; row.a2 = -0.000000; tableSet->AddAt(&row, i++);
    row.a0 = -0.003978; row.a1 = 0.000218; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.004055; row.a1 = 0.000219; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.014831; row.a1 = 0.000302; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.004603; row.a1 = 0.000246; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.003762; row.a1 = 0.000244; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.006403; row.a1 = 0.000219; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.007579; row.a1 = 0.000253; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.008916; row.a1 = 0.000235; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
    row.a0 = -0.006696; row.a1 = 0.000235; row.a2 = -0.000001; tableSet->AddAt(&row, i++); 
    row.a0 = -0.005769; row.a1 = 0.000249; row.a2 = -0.000001; tableSet->AddAt(&row, i++);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
