// $Id: t0_ana.C,v 1.4 2009/12/09 14:41:49 jcs Exp $
//
// $Log: t0_ana.C,v $
// Revision 1.4  2009/12/09 14:41:49  jcs
// delta_t0 and delta_gas can now both = 0
//
// Revision 1.3  2008/05/16 18:36:56  jcs
// update FTPC calibration macros
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

void t0_ana(TString input)
{
  
  // plain Style
  gROOT->Reset();
  gStyle->SetTitleOffset(1.25);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetPadBorderMode(0);
  gStyle->SetPadColor(0);
  gStyle->SetCanvasColor(0);
  gStyle->SetTitleColor(0);
  gStyle->SetStatColor(0);
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  gStyle->SetOptFit();

  float delta=0.5;
  float minrad=5;

  TCanvas *c1 = new TCanvas("c1","RadialStep West",200,10,700,500);
  TCanvas *c2 = new TCanvas("c2","RadialStep East",230,40,730,530);
  TCanvas *c3 = new TCanvas("c3","RadialStep East/West (fit)",260,70,760,560);
  TCanvas *c4 = new TCanvas("c4","Diff. RadialStep East/West (fit)",290,100,790,590);
  //TPostScript *fps=new TPostScript("central.ps",112);

  cout<<endl;
  cout<<"T0 summary..."<<endl;
  //cout<<endl;

  TLegend *leg;
  leg = new TLegend(0.126,0.5,0.3,0.9); 
  leg->SetTextSize(0.03);

  int mindt=-8;
  int maxdt=1;
  //int anz=abs(maxdt-mindt);
  
  const int anz=10;
  const Float_t rinner=7.73;

  Float_t stepw[anz],dt[anz],stepe[anz];
  Float_t dstepw[anz], dstepe[anz];

  int zaehl=0;

  for (int i=mindt;i<=maxdt;i++)
    {

      TString filename=input;

      float step=i/10.0;

      dt[zaehl]=step;

      char t[3];
        
      if (i==0)
	{
	  filename +="_";
	  filename += "0";
	  filename +="_0_t0.root";
	  
	  sprintf(t,"%.2f",step);
	} 
      else
	{
	  sprintf(t,"%.2f",step);

	  filename +="_";
	  filename += t;
	  filename +="_0_t0.root";
	}
        
      cout<<"Process file "<<filename<<endl;

      TFile *f = new TFile(filename);

      f->ls();
      //rstep[i]=new TH1F();

      
      TH1F *rstep=new TH1F();
      TH1F *rstep2=new TH1F();

      TString title="#Delta_{t0} = ";
      title +=t;
      //cout<<title<<endl;

      rstep=(TH1F*) f->Get("rad_west");
      rstep->SetName(title);
      

      //cout<<"Nach Hist get ..."<<endl;

      rstep2=(TH1F*) f->Get("rad_east");
      rstep2->SetName(title);

      //cout<<"Nach Hist get ..."<<endl;

      //TString title2=rstep->GetTitle();
      //title2 += t;
      
      //rstep->SetTitle(title2);

      rstep->SetMarkerStyle(22);
      rstep2->SetMarkerStyle(22);
      if (i<0)
	{
	  //rstep->SetMarkerColor(abs(i)+1);
	  //rstep->SetLineColor(abs(i)+1);
	  //rstep2->SetMarkerColor(abs(i)+1);
	  //rstep2->SetLineColor(abs(i)+1);
          rstep->SetMarkerColor((i)+10);
	  rstep->SetLineColor((i)+10);
	  rstep2->SetMarkerColor((i)+10);
	  rstep2->SetLineColor((i)+10);
	}
      else
	{
	  //rstep->SetMarkerColor(abs(i)+40);
	  //rstep->SetLineColor(abs(i)+40);
	  //rstep2->SetMarkerColor(abs(i)+40);
	  //rstep2->SetLineColor(abs(i)+40);
          rstep->SetMarkerColor((i)+40);
	  rstep->SetLineColor((i)+40);
	  rstep2->SetMarkerColor((i)+40);
	  rstep2->SetLineColor((i)+40);
          
	}


      //rstep->Scale(1/rstep->GetEntries());

      if (i==mindt) 
	{
	  c1->cd();
	  rstep->DrawCopy("h");
	  c2->cd();
	  rstep2->DrawCopy("h");
	}
      else
	{
	  c1->cd();
	  rstep->DrawCopy("hsame");
	  c2->cd();
	  rstep2->DrawCopy("hsame");
	}
      
      leg->AddEntry(title,title,"l");

      // fit radial step (east/west);

      Float_t maxrad=rstep->GetXaxis()->GetBinCenter(rstep->GetMaximumBin());
      Float_t maxrad2=rstep->GetXaxis()->GetBinCenter(rstep2->GetMaximumBin());

      /*
      TF1 *gfit;

      if (maxrad+delta<12)
	gfit=new TF1("gfit","gaus",minrad,maxrad+delta);
      else
	gfit=new TF1("gfit","gaus",minrad,12);
      */

      TF1 *gfit=new TF1("gfit","gaus",minrad,maxrad+delta);      
      //if (i<5)
      rstep->Fit(gfit,"NQR");
      //cout<<t<<" "<<maxrad<<" "<<gfit->GetParameter(1)-gfit->GetParameter(2)<<endl;
      stepw[zaehl]=(gfit->GetParameter(1)-gfit->GetParameter(2));
      dstepw[zaehl]=fabs(rinner-(gfit->GetParameter(1)-gfit->GetParameter(2)));
      delete gfit;
      
      TF1 *gfit2=new TF1("gfit2","gaus",minrad,maxrad2+delta);      
      //if (i<5)
      rstep2->Fit(gfit2,"NQR");
      //cout<<t<<" "<<maxrad<<" "<<gfit2->GetParameter(1)-gfit2->GetParameter(2)<<endl;
      stepe[zaehl]=(gfit2->GetParameter(1)-gfit2->GetParameter(2));
      dstepe[zaehl]=fabs(rinner-(gfit2->GetParameter(1)-gfit2->GetParameter(2)));
      delete gfit2;
      

      //delete rstep; delete rstep2;

      zaehl++;
      
      f->Close();
    }

  c1->cd();
  leg->Draw();
  c2->cd();
  leg->Draw();
  
  c3->cd();

  TH2F *hr=new TH2F("hr","RadiusStep vs. #Delta_{t0} (read=east/ black=west)",100,(mindt/10.0)-0.25,int (maxdt/10)+0.25,100,5,10);
  hr->DrawCopy();
      
  TLine *inner=new TLine(mindt/10.0,7.73,((maxdt)/10.0),7.73);inner->SetLineColor(3);inner->Draw("same");

  TGraph *gw=new TGraph(anz,dt,stepw);
  gw->SetMarkerStyle(21);
  gw->SetMarkerSize(1);
  gw->SetMarkerColor(1);
  gw->SetLineColor(1);

  TGraph *ge=new TGraph(anz,dt,stepe);
  ge->SetMarkerStyle(21);
  ge->SetMarkerSize(1);
  ge->SetMarkerColor(2);
  ge->SetLineColor(2);

  gw->Draw("P");
  ge->Draw("P");

  c4->cd();

  TH2F *hr1=new TH2F("hr1","abs(Radius_{inner}-RadiusStep) vs. #Delta_{t0} (read=east/ black=west)",100,(mindt/10.0)-0.25,int (maxdt/10)+0.25,100,-0.25,1.25);
  hr1->DrawCopy();
      
  TGraph *gwd=new TGraph(anz,dt,dstepw);
  gwd->SetMarkerStyle(21);
  gwd->SetMarkerSize(1);
  gwd->SetMarkerColor(1);
  gwd->SetLineColor(1);

  TGraph *ged=new TGraph(anz,dt,dstepe);
  ged->SetMarkerStyle(21);
  ged->SetMarkerSize(1);
  ged->SetMarkerColor(2);
  ged->SetLineColor(2);

  gwd->Draw("P");
  ged->Draw("P");

  cout<<endl;
 
}

