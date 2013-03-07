#include <iomanip>
#include "fgtPedestal.h"   

static const int MAX=2*6*20*128;
static const float markerSize=0.38;

makepedplot(int run, int statonly=0, int comp=0){
  char filename[100];
  float ped[MAX],rms[MAX],frac[MAX],p,r,f;
  float ped2[MAX],rms2[MAX];
  int eleId[MAX],status[MAX],eid,s,t,i; 
  TString statread;
  
  int day=run/1000;
  if(run>0 && statonly==0){
    sprintf(filename,"%d/ped/ped.%d.txt",day,run);
  }else{
    sprintf(filename,"ped.txt");
  }
  cout<<"Reading File "<<filename<<endl;
  std::ifstream in(filename);
  if (!in.is_open()) {
    cout << "Can't find file!\n"; 
    exit(0); 
  }   
  while (!in.eof()){
    //  in >> eid >> t >> p >> r >> f;
    in >> eid >> t >> p >> r;
    eleId[eid]=eid;
    ped[eid]=p;
    rms[eid]=r;
    //frac[eid]=f;
    //printf("%5d %6.1f %6.1f\n",eid,ped[eid],rms[eid]);
  }  
  in.close();

  if(run>0){
    sprintf(filename,"%d/status/status.%d.txt",day,run);
  }else{
    sprintf(filename,"status.txt");
  }
  cout<<"Reading File "<<filename<<endl;
  std::ifstream in2(filename);
  if (!in2.is_open()) {
    cout << "Can't find file!\n"; 
    exit(0); 
  }   
  while (!in2.eof()){
    in2 >> eid >> statread;
    statread.Remove(0,2);
    status[eid] = statread.Atoi();
    //printf("%5d %4d\n",eid,status[eid]);
  }  
  in2.close();

  if(comp==2){
    memset(ped2,0,sizeof(ped2));
    memset(rms2,0,sizeof(rms2));
    TH1F * hd = new TH1F("PED Offline-Tonko","PED Offline-Tonko",50,-30,30);
    TH1F * he = new TH1F("RMS Offline-Tonko","RMS Offline-Tonko",50,-30,30);
    TGraph * de=new TGraphErrors();
    TGraph * dp=new TGraphErrors();
    cout<<"Reading File fgt_pedestals.txt"<<endl;
    std::ifstream in3("fgt_pedestals.txt");
    if (!in3.is_open()) {
      cout << "Can't find file!\n";
      exit(0);
    }
    int rdo,arm,apv,ch,t;
    while (!in3.eof()){
      in3 >> rdo >> arm >> apv >> ch >> t >> p >> r;
      if(apv>9) apv-=2;
      eid=(rdo-1)*6*20*128 + arm*20*128 + apv*128 + ch;
      ped2[eid]+=p/15.0;
      rms2[eid]+=r/15.0;
    }
    in3.close();
    int j=0;
    for(int i=0; i<MAX; i++){
      float d=ped[i]-ped2[i]; hd->Fill(d);
      float e=rms[i]-rms2[i]; he->Fill(e);
      if(fabs(d)<50){
	de->SetPoint(j,float(i),d);
	dp->SetPoint(j,ped[i],d);
	j++;
      }
      // printf("%6d %8.2f %8.2f %8.2f   %8.2f %8.2f%8.2f\n",i,ped[i],ped2[i],d,rms[i],rms2[i],e);      
    }
    TCanvas * c1 = new TCanvas("ped","ped",50,0,800,800);
    c1->Divide(2,2);
    gStyle->SetOptStat(1111110);
    c1->cd(1); hd->Draw();
    c1->cd(2); he->Draw();
    de->GetHistogram()->SetTitle("PED:Offline-Tonko:Id");
    dp->GetHistogram()->SetTitle("PED:Offline-Tonko:Ped");
    c1->cd(3); de->SetMarkerStyle(22); de->SetMarkerSize(markerSize); de->Draw("AP");
    c1->cd(4); dp->SetMarkerStyle(22); dp->SetMarkerSize(markerSize); dp->Draw("AP");
    return;
  }

  TGraph * gped=new TGraphErrors();
  TGraph * grms=new TGraphErrors();
  TGraph * bped=new TGraphErrors();
  TGraph * brms=new TGraphErrors();
  TGraph * gcor=new TGraphErrors();
  TGraph * bcor=new TGraphErrors();
  TH1F * hgped = new TH1F("PED" , "PED",  100,0,2000);
  TH1F * hbped = new TH1F("PEDb", "PEDb", 100,0,2000);
  TH1F * hgrms = new TH1F("RMS",  "RMS",  100,0,120);
  TH1F * hbrms = new TH1F("RNSb", "RMSb", 100,0,120);
  //TH1F * hgfrac= new TH1F("Frac", "Frac", 100,0,1);
  //TH1F * hbfrac= new TH1F("Fracb","Fracb",100,0,1);
  int g=0, b=0, a=0;
  for(eid=0; eid<MAX; eid++){
    if(status[eid]==0) { 
      gped->SetPoint(g,float(eid),ped[eid]); 
      grms->SetPoint(g,float(eid),rms[eid]); 
      gcor->SetPoint(g,ped[eid],rms[eid]);
      hgped->Fill(ped[eid]);
      hgrms->Fill(rms[eid]);
      //hgfrac->Fill(frac[eid]);
      g++;
    }else{ 
      bped->SetPoint(b,float(eid),ped[eid]); 
      brms->SetPoint(b,float(eid),rms[eid]); 
      bcor->SetPoint(b,ped[eid],rms[eid]);
      hbped->Fill(ped[eid]);
      hbrms->Fill(rms[eid]);
      //hbfrac->Fill(frac[eid]);
      b++;
    }
    a++;
  }
  printf("Status Good=%5d Bad=%5d",g,b);

  TCanvas * c1 = new TCanvas("ped","ped",0,0,1200,800);
  c1->Divide(1,3);

  c1->cd(1);
  gped->GetHistogram()->SetTitle("Pedestals;ElecId;ADC");
  gped->SetMinimum(0); gped->SetMaximum(1500); gped->GetXaxis()->SetRangeUser(0,float(MAX+1));
  gped->SetMarkerStyle(21); gped->SetMarkerSize(markerSize); gped->SetMarkerColor(kBlue); gped->SetLineColor(kBlue);
  gped->Draw("AP");
  bped->SetMarkerStyle(22); bped->SetMarkerSize(markerSize); bped->SetMarkerColor(kRed); bped->SetLineColor(kRed);
  bped->Draw("P");

  c1->cd(2);
  grms->GetHistogram()->SetTitle("PedRMS;ElecId;ADC");
  grms->SetMinimum(0); grms->SetMaximum(200); grms->GetXaxis()->SetRangeUser(0,float(MAX+1));
  grms->SetMarkerStyle(21); grms->SetMarkerSize(markerSize); grms->SetMarkerColor(kBlue); grms->SetLineColor(kBlue);
  grms->Draw("AP");
  brms->SetMarkerStyle(22); brms->SetMarkerSize(markerSize); brms->SetMarkerColor(kRed); brms->SetLineColor(kRed);
  brms->Draw("P");

  const char c[200]; Double_t x=500, y=160;
  sprintf(c,"Status Good=%d   Bad=%d   R(Bad/Total)=%5.2f",g,b,float(b)/float(a));
  TText* t1=new TText(x,y,c); t1->SetTextSize(0.1); t1->Draw();

  c1->cd(3); gPad->Divide(3,1);

  c1->cd(3); gPad->cd(1);
  gcor->GetHistogram()->SetTitle("PEDvsRMS;PED;RMS");
  gcor->SetMinimum(0); gcor->SetMaximum(150); gcor->GetXaxis()->SetRangeUser(0,1200);
  gcor->SetMarkerStyle(21); gcor->SetMarkerSize(markerSize); gcor->SetMarkerColor(kBlue); gcor->SetLineColor(kBlue);
  gcor->Draw("AP");
  bcor->SetMarkerStyle(22); bcor->SetMarkerSize(markerSize); bcor->SetMarkerColor(kRed); bcor->SetLineColor(kRed);
  bcor->Draw("P");

  c1->cd(3); gPad->cd(2); gPad->SetLogy(); hgped->SetFillColor(kBlue);  hgped->Draw();  hbped->SetFillColor(kRed);  hbped->Draw("same");  
  c1->cd(3); gPad->cd(3); gPad->SetLogy(); hgrms->SetFillColor(kBlue);  hgrms->Draw();  hbrms->SetFillColor(kRed);  hbrms->Draw("same");  
  //c1->cd(3); gPad->cd(4); gPad->SetLogy(); hgfrac->SetFillColor(kBlue); hgfrac->Draw(); hbfrac->SetFillColor(kRed); hbfrac->Draw("same"); 

  c1->Update();
  
  sprintf(filename,"%d/%d_ped.png",day,run);  
  c1->SaveAs(filename);
}
