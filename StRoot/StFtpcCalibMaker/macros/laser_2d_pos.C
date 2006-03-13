//======================================================================
//
//   laser_2d_pos reads the file produced in StFtpcLaserCalib::MakePs()
//   and draws the reconstructed position histograms
//
//   Inputs to macro:
//        a    - select value(s) to be histogrammed
//               a  Laser I - straight track at lpos1
//               b  Laser II - straight track at lpos2
//               c  Laser III - straight track at lpos3
//               d  all 3 straight lasers
//      opt    - set Draw Options
//               (see ftp://root.cern.ch/root/doc/chapter3.pdf, pages 5-6)
//
//======================================================================

void laser_2d_pos(TString a,char *opt)
{
  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);
/*
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetPadBorderMode(0);
  gStyle->SetPadColor(0);
  gStyle->SetCanvasColor(0);
  gStyle->SetTitleColor(0);
  gStyle->SetStatColor(0);
*/

  TH2F *hr0 = new TH2F("hr","",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  TH2F *hr = new TH2F("hr","Laser I: nominal (11.91cm) - reconstructed position",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  TH2F *hr2 = new TH2F("hr2","Laser II: nominal (19.55cm) - reconstructed position",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  TH2F *hr3 = new TH2F("hr3","Laser III: nominal (28.56cm) - reconstructed position",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr0->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr0->GetXaxis()->SetTitle("#Delta t_{0}");
  hr->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr->GetXaxis()->SetTitle("#Delta t_{0}");
  hr2->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr2->GetXaxis()->SetTitle("#Delta t_{0}");
  hr3->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr3->GetXaxis()->SetTitle("#Delta t_{0}");
  //

  //FILE *file1=fopen ("4024025_lasertest_w1g_res.log","r");
  //FILE *file1=fopen ("5006004_lasertest_w1g_res.log","r");
  //FILE *file1=fopen ("6082062_lasertest_w2g_res.log","r");
  FILE *file1=fopen ("res.log","r");
  
  Int_t datab1;

  const Float_t lpos1=11.91;
  const Float_t lpos2=19.55;
  const Float_t lpos3=28.56;

  float resx,resy,resrad,resphi, t0, gas, c2,rad1,rad11, rad2, rad21, rad3, rad31, err;
  

  while(!feof(file1))
    {
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&resx,&c2);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&resy,&c2);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&resrad,&c2);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&resphi,&c2);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&rad1,&err);
      datab1 = fscanf(file1,"%f %f %f",&t0,&gas,&rad11);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&rad2,&err);
      datab1 = fscanf(file1,"%f %f %f",&t0,&gas,&rad21);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&rad3,&err);
      datab1 = fscanf(file1,"%f %f %f",&t0,&gas,&rad31);
      if (feof(file1)) break;

	  if (a=='a')
	    {
	      hr0->Fill(t0,gas,lpos1-rad1);
	    }
	  else if (a=='b')
	    {
	      hr0->Fill(t0,gas,lpos2-rad2);
	    }
	  else if (a=='c')
	    {
	      hr0->Fill(t0,gas,lpos3-rad3);
	    }
	  else if (a=='d')
	    {
	      hr->Fill(t0,gas,lpos1-rad1);
	      hr2->Fill(t0,gas,lpos2-rad2);
	      hr3->Fill(t0,gas,lpos3-rad3);
	    }
	  else
	    {cout<<"Funktion nicht vorhanden !"<<endl;break;}
	}
  if (a=='d')
    {
      c1->Divide(2,2);
      c1->cd(1);
      hr->DrawCopy(opt);
      c1->cd(2);
      hr2->DrawCopy(opt);
      c1->cd(3);
      hr3->DrawCopy(opt);
    }
  else 
    hr0->DrawCopy(opt);
  
  //float minx,miny;
  //cout<<hr->GetMinimumBin()<<endl;;
  //cout<<minx<<" "<<miny<<endl;
  c1->Update();
  if (fclose(file1) != 0)
     cout<<"Datei nicht geschlossen !"<<endl;

}
