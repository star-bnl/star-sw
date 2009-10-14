// $Id: res_laser.C,v 1.6 2009/10/14 15:58:43 jcs Exp $
//
// $Log: res_laser.C,v $
// Revision 1.6  2009/10/14 15:58:43  jcs
// change and add macros so that in addition to varying t0 and the gas compostion,
// the gas temperature can be varied
//
// Revision 1.5  2008/05/15 21:00:06  jcs
// change histogram limits
// add improved comments
//
// Revision 1.4  2008/04/23 20:28:56  jcs
// clean up code
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

// plots the rms width of the residual distribution in radius and in phi vs. change in gas composition delta Ar 
// from  the *_res.log files produced by lasertest.C macro
// to use create the res.log file with the following 2 commands
//       touch res.log
//       cat *_res.log >> res.log

 
#include "Gtypes.h"
#include "iostream.h"

float calc_min(float b1,float b2)
{
  return -b1/(2*b2);
}

void res_laser()
{
  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
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

  TH2F *hr = new TH2F("hr","",10,-0.5,0.5,10,0.0,0.1);
  hr->SetTitle("#sigma_{res} vs. #Delta Argon");
  hr->GetYaxis()->SetTitleOffset(1.2);
  hr->GetYaxis()->SetTitle("#sigma_{res}");
  hr->GetXaxis()->SetTitleOffset(1.2);
  hr->GetXaxis()->SetTitle("#Delta Argon");
  hr->SetStats(0);
  hr->DrawCopy(); 
  //

  FILE *file1=fopen ("res.log","r");
  Int_t datab1,datab0;

  const int nhits=160;

  float resx[nhits],resy[nhits],resrad[nhits],resphi[nhits], t0[nhits], gas[nhits];
  
  float gas_temp, t0_temp, T, c2,rad, err, resphi_temp, resrad_temp;

  int ihits==0;
  while(!feof(file1))
    {

      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&resx,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&resy,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&resrad_temp,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&resphi_temp,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&rad,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0_temp,&gas_temp,&T,&rad);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&rad,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0_temp,&gas_temp,&T,&rad);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0_temp,&gas_temp,&T,&rad,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0_temp,&gas_temp,&T,&rad);
      
      //cout<<t0_temp<<endl;
      //if (t0_temp!=0)
	{
	  t0[ihits]=t0_temp;
	  gas[ihits]=gas_temp;
	  resphi[ihits]=resphi_temp;
	  resrad[ihits]=resrad_temp;
	  cout<<ihits<<" t0["<<ihits<<"] = "<<t0_temp<<" gas["<<ihits<<"] = "<<gas_temp<<" resphi["<<ihits<<"] = "<<resphi_temp<<" resrad["<<ihits<<"] = "<<resrad_temp<<endl;
	  ihits++;
	}
    }

  TF1 *g=new TF1("g","[0]+[1]*x^2",0,0.6);
  g->SetParameters(0.01,0.001);
  g->SetLineColor(kRed);

  TGraph *gerad = new TGraph(ihits,gas,resphi);
  gerad->SetMarkerStyle(22);
  gerad->SetMarkerColor(kRed);
  gerad->SetMarkerSize(1);
  gerad->Draw("P"); 
  //gerad->Fit(g,"R");

  //cout<<"Min phi = "<<calc_min(g->GetParameter(0),g->GetParameter(1))<<endl;

  TGraph *gerad1 = new TGraph(ihits,gas,resrad);
  gerad1->SetMarkerStyle(23);
  gerad1->SetMarkerColor(kGreen);
  gerad1->SetMarkerSize(1);
  gerad1->Draw("P"); 
   
  //g->SetLineColor(kGreen);
  //gerad1->Fit(g,"R");

  //cout<<"Min rad = "<<calc_min(g->GetParameter(0),g->GetParameter(1))<<endl;

  leg = new TLegend(0.66,0.82,0.92,0.96); 
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
