TH1F *h[10];
h[0] = 0;
h[1] = 0;
h[2] = 0;
h[3] = 0;
h[4] = 0;
h[5] = 0;
h[6] = 0;
h[7] = 0;
h[8] = 0;
h[9] = 0;
TCanvas *CANVAS = 0;
void clear()
{
  for(int i=0;i<10;i++) 
  {
    if(h[i]) delete h[i];
    h[i] = 0;
  }
}

TCanvas* canvas(int nx, int ny, int sx=600, int sy =900)
{
  if(CANVAS) {delete CANVAS; CANVAS=0;}
  CANVAS = new TCanvas();
  //CANVAS->SetWindowSize(sx,sy);
  //CANVAS->Update();
  CANVAS->Divide(nx,ny);
  return CANVAS;
}
void draw(emcCalib_st* t1)
{
  clear();
  h[0] = new TH1F("emcCalib_0","",4800,0.5,4800.5);
  h[1] = new TH1F("emcCalib_1","",4800,0.5,4800.5);
  h[2] = new TH1F("emcCalib_2","",4800,0.5,4800.5);
  h[3] = new TH1F("emcCalib_3","",4800,0.5,4800.5);
  h[4] = new TH1F("emcCalib_4","",4800,0.5,4800.5);
  h[5] = new TH1F("emcCalib_S","",4800,0.5,4800.5);
  for(int i = 0; i<4800;i++) 
  {
    for(int j=0;j<5;j++) h[j]->Fill(i+1,t1->AdcToE[i][j]);
    h[5]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(1,6);
  for(int j=0;j<6;j++) 
  {
    c->cd(j+1);
    h[j]->Draw();
  }
  c->Update();
} 
void draw(smdCalib_st* t1)
{
  clear();
  h[0] = new TH1F("smdCalib_0","",18000,0.5,18000.5);
  h[1] = new TH1F("smdCalib_1","",18000,0.5,18000.5);
  h[2] = new TH1F("smdCalib_2","",18000,0.5,18000.5);
  h[3] = new TH1F("smdCalib_3","",18000,0.5,18000.5);
  h[4] = new TH1F("smdCalib_4","",18000,0.5,18000.5);
  h[5] = new TH1F("smdCalib_S","",18000,0.5,18000.5);
  for(int i = 0; i<18000;i++) 
  {
    for(int j=0;j<5;j++) h[j]->Fill(i+1,t1->AdcToE[i][j]);
    h[5]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(1,6);
  for(int j=0;j<6;j++) 
  {
    c->cd(j+1);
    h[j]->Draw();
  }
  c->Update();
}
void draw(emcStatus_st* t1)
{
  clear();
  h[0] = new TH1F("emcStatus_S","",4800,0.5,4800.5);
  for(int i = 0; i<4800;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(1,1,600,300);
  h[0]->Draw();
  c->Update();
  return;
}
void draw(smdStatus_st* t1)
{
  clear();
  h[0] = new TH1F("smdStatus_S","",18000,0.5,18000.5);
  for(int i = 0; i<18000;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(1,1,600,300);
  h[0]->Draw();
  c->Update();
  return;
}
void draw(emcGain_st* t1)
{
  clear();
  h[0] = new TH1F("emcGain_S","",4800,0.5,4800.5);
  for(int i = 0; i<4800;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->Gain[i]);
  }
  TCanvas* c = canvas(1,1,600,300);
  h[0]->Draw();
  c->Update();
  return;
}
void draw(smdGain_st* t1)
{
  clear();
  h[0] = new TH1F("smdGain_S","",18000,0.5,18000.5);
  for(int i = 0; i<18000;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->Gain[i]);
  }
  TCanvas* c = canvas(1,1,600,300);
  h[0]->Draw();
  c->Update();
  return;
}
void draw(smdPed_st* t1)
{
  clear();
  h[0] = new TH1F("smdPed_ADC0","",18000,0.5,18000.5);
  h[1] = new TH1F("smdPed_ADC1","",18000,0.5,18000.5);
  h[2] = new TH1F("smdPed_ADC2","",18000,0.5,18000.5);
  h[3] = new TH1F("smdPed_RMS0","",18000,0.5,18000.5);
  h[4] = new TH1F("smdPed_RMS1","",18000,0.5,18000.5);
  h[5] = new TH1F("smdPed_RMS2","",18000,0.5,18000.5);
  h[6] = new TH1F("smdPed_STATUS","",18000,0.5,18000.5);
  for(int i = 0; i<18000;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->AdcPedestal[i][0]/100);
    h[1]->Fill(i+1,(float)t1->AdcPedestal[i][1]/100);
    h[2]->Fill(i+1,(float)t1->AdcPedestal[i][2]/100);
    h[3]->Fill(i+1,(float)t1->AdcPedestalRMS[i][0]/100);
    h[4]->Fill(i+1,(float)t1->AdcPedestalRMS[i][1]/100);
    h[5]->Fill(i+1,(float)t1->AdcPedestalRMS[i][2]/100);
    h[6]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(3,3,800,500);
  for(int j=0;j<7;j++) 
  {
    c->cd(j+1);
    h[j]->Draw();
  }
  c->Update();
  return;
}
void draw(emcPed_st* t1)
{
  clear();
  h[0] = new TH1F("emcPed_ADC","",4800,0.5,4800.5);
  h[1] = new TH1F("emcPed_RMS","",4800,0.5,4800.5);
  h[2] = new TH1F("emcPed_STATUS","",4800,0.5,4800.5);
  for(int i = 0; i<4800;i++) 
  { 
    h[0]->Fill(i+1,(float)t1->AdcPedestal[i]/100);
    h[1]->Fill(i+1,(float)t1->AdcPedestalRMS[i]/100);
    h[2]->Fill(i+1,(float)t1->Status[i]);
  }
  TCanvas* c = canvas(1,3);
  for(int j=0;j<3;j++) 
  {
    c->cd(j+1);
    h[j]->Draw();
  }
  c->Update();
  return;
}
void draw(emcTriggerPed_st* t1)
{
  clear();
  h[0] = new TH1F("emcTriggerPed_BITCONV","",300,0,300);
  h[1] = new TH1F("emcTriggerPed_PED","",4800,0,4800);
  for(int c=0;c<30;c++) 
  {
    for(int p=0;p<10;p++) 
      h[0]->Fill(c*10+p, (float)t1->BitConversionMode[c][p]);
    for(int p=0;p<160;p++) 
      h[1]->Fill(c*160+p,(float)t1->Ped[c][p]/100) ;
  }
  TCanvas* q = canvas(1,2);
  for(int j=0;j<2;j++) 
  {
    q->cd(j+1);
    h[j]->Draw();
  }
  q->Update();
  return;
}
void draw(emcTriggerStatus_st* t1)
{
  clear();
  h[0] = new TH1F("emcTriggerStatus_Patch","",300,0,300);
  h[1] = new TH1F("emcTriggerStatus_HighTower","",300,0,300);
  h[2] = new TH1F("emcTriggerStatus_Tower","",4800,0,4800);
  for(int c=0;c<30;c++) 
  {
    for(int p=0;p<10;p++)
    { 
      h[0]->Fill(c*10+p,t1->PatchStatus[c*10+p]) ;
      h[1]->Fill(c*10+p,t1->HighTowerStatus[c*10+p]) ;
    }
    for(int p=0;p<160;p++) 
      h[2]->Fill(c*160+p,t1->TowerStatus[c][p]);
  }
  TCanvas* q = canvas(1,3);
  for(int j=0;j<3;j++) 
  {
    q->cd(j+1);
    h[j]->Draw();
  }
  q->Update();
  return;
}
void draw(emcTriggerLUT_st* t1)
{
  clear();
  h[0] = new TH1F("emcTriggerStatus_Tag","",300,0,300);
  h[1] = new TH1F("emcTriggerStatus_Par0","",300,0,300);
  h[2] = new TH1F("emcTriggerStatus_Par1","",300,0,300);
  h[3] = new TH1F("emcTriggerStatus_Par2","",300,0,300);
  h[4] = new TH1F("emcTriggerStatus_Par3","",300,0,300);
  h[5] = new TH1F("emcTriggerStatus_Par4","",300,0,300);
  h[6] = new TH1F("emcTriggerStatus_Par5","",300,0,300);
  for(int c=0;c<30;c++) 
  {
    for(int p=0;p<10;p++)
    { 
      h[0]->Fill(c*30+p,t1->FormulaTag[c][p]) ;
      h[1]->Fill(c*30+p,t1->FormulaParameter0[c][p]);
      h[2]->Fill(c*30+p,t1->FormulaParameter1[c][p]);
      h[3]->Fill(c*30+p,t1->FormulaParameter2[c][p]);
      h[4]->Fill(c*30+p,t1->FormulaParameter3[c][p]);
      h[5]->Fill(c*30+p,t1->FormulaParameter4[c][p]);
      h[6]->Fill(c*30+p,t1->FormulaParameter5[c][p]);
    }
  }
  TCanvas* q = canvas(1,7);
  for(int j=0;j<7;j++) 
  {
    q->cd(j+1);
    h[j]->Draw();
  }
  q->Update();
  return;
}

