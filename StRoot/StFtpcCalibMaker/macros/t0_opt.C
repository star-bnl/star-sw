// $Id: t0_opt.C,v 1.3 2008/05/16 18:36:57 jcs Exp $
//
// $Log: t0_opt.C,v $
// Revision 1.3  2008/05/16 18:36:57  jcs
// update FTPC calibration macros
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

float calc_min(float b1,float b2)
{
  return -b1/(2*b2);
}

//  bf = 0 or bf = 1
void t0_opt(int bf)
{
  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  //c1->Divide(2,2);
  TStyle *plain  = new TStyle("Plain","Plain Style (no colors/fill areas)");
  plain->SetTitleOffset(1.25);
  plain->SetCanvasBorderMode(0);
  plain->SetPadBorderMode(0);
  plain->SetPadColor(0);
  plain->SetCanvasColor(0);
  plain->SetTitleColor(0);
  plain->SetStatColor(0);
  plain->SetPalette(1);
  plain->SetOptStat(0000000);
  plain->SetOptFit(00000000);

  //hvrad->DrawCopy();
  TH2F *hr = new TH2F("hr","",10,-0.1,0.7,10,0.005,0.03);
  hr->SetOptStat(0);
  hr->SetTitle("#sigma_{res} vs. t_{0}");
  hr->GetYaxis()->SetTitleOffset(1.2);
  hr->GetYaxis()->SetTitle("#sigma_{res}");
  hr->GetXaxis()->SetTitleOffset(1.2);
  hr->GetXaxis()->SetTitle("t_{0}");
  hr->DrawCopy(); 
  //

  
  // B=0 !!!
  if ( bf==0)
    {
      Double_t t0[6] = { 0.1, 0.2,0.3,0.4,0.5,0.6};
      Double_t resphi[6] = {0.01383,0.01184,0.009826,0.01083,0.01498,0.01891 };
      Double_t resrad[6] = {0.01922,0.01802,0.01607,0.01665,0.02196,0.02338};
    }
  else
    {
      // B=1 !!!!
      Double_t t0[6] = { 0, 0.1, 0.2,0.3,0.4,0.5};
      Double_t resphi[6] = {0.0239,0.0223,0.021,0.02033,0.01999,0.02347};
      Double_t resrad[6] = {0.021,0.0178,0.01645,0.01656,0.01895,0.0211};
    }

  //TF1 *g=new TF1("g","[0]*1/x^[1]",0,30);
  //gerad->Fit("g","R");

  //TGraph *gerad = new TGraph(6,t0,resrad);
  TF1 *g=new TF1("g","pol2",-0.1,0.7);
  g->SetLineColor(2);

  TGraph *gerad = new TGraph(6,t0,resphi);
  gerad->SetMarkerStyle(22);
  gerad->SetMarkerColor(2);
  gerad->SetMarkerSize(1);
  gerad->Draw("P"); 
  gerad->Fit(g,"RQ");

  cout<<"Min phi = "<<calc_min(g->GetParameter(1),g->GetParameter(2))<<endl;

  TGraph *gerad1 = new TGraph(6,t0,resrad);
  gerad1->SetMarkerStyle(23);
  gerad1->SetMarkerColor(3);
  gerad1->SetMarkerSize(1);
  gerad1->Draw("P"); 
   
  g->SetLineColor(3);
  gerad1->Fit(g,"RQ");

  cout<<"Min rad = "<<calc_min(g->GetParameter(1),g->GetParameter(2))<<endl;

  leg = new TLegend(0.46,0.72,0.72,0.86); 
  leg->SetTextSize(0.05);
  leg->AddEntry(gerad,"#sigma_{res,#phi}","P");
  leg->AddEntry(gerad1,"#sigma_{res,r}","P");
  //leg->AddEntry(gerad2,"scope","P");
  //leg->AddEntry(gerad1,"52.25/47.75 halffield","P");
  //leg->AddEntry(gerad2,"53/47 halffield","P");
  //leg->AddEntry(steppurgegasno,"purge modus fieldoff 53%/47%","l");
  leg->SetFillColor(0);
  leg->Draw();

  c1->Update();

} 
