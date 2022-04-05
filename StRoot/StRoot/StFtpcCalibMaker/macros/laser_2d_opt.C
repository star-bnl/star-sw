// $Id: laser_2d_opt.C,v 1.4 2009/10/14 15:58:43 jcs Exp $
//
// $Log: laser_2d_opt.C,v $
// Revision 1.4  2009/10/14 15:58:43  jcs
// change and add macros so that in addition to varying t0 and the gas compostion,
// the gas temperature can be varied
//
// Revision 1.3  2008/05/15 20:58:34  jcs
// removed debug print out statement
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

//======================================================================
//
//   laser_2d_opt reads the file produced in StFtpcLaserCalib::MakePs()
//   and draws the histograms for the residuals
//
//   Inputs to macro:
//        a    - select value(s) to be histogrammed
//               x   resx
//               y   resy
//               r   resrad
//               p   resphi
//               c   c2
//               e   rad3-28.56
//               a   resx, resy, resrad, resphi
//      opt    - set Draw Options
//               (see ftp://root.cern.ch/root/doc/chapter3.pdf, pages 5-6)
//
//======================================================================

void laser_2d_opt(TString a,char *opt)
{
cout<<"a = "<<a<<" opt "<<opt<<endl;
  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  gStyle->SetPalette(1);
  gStyle->SetOptStat(0);
  gStyle->SetOptFit(0);

  TH2F *hr = new TH2F("hr","",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr->GetXaxis()->SetTitle("#Delta t_{0}");
  TH2F *hr2 = new TH2F("hr2","Residual x",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr2->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr2->GetXaxis()->SetTitle("#Delta t_{0}");
  TH2F *hr3 = new TH2F("hr3","Residual y",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr3->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr3->GetXaxis()->SetTitle("#Delta t_{0}");
  TH2F *hr4 = new TH2F("hr4","Residual radius",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr4->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr4->GetXaxis()->SetTitle("#Delta t_{0}");
  TH2F *hr5 = new TH2F("hr5","Residual phi",12,-0.55,0.65,12,-0.45,0.75); // binning !?????
  hr5->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr4->GetXaxis()->SetTitle("#Delta t_{0}");
  
  //hr->DrawCopy(); 
  //

  
  //FILE *file1=fopen ("5006004_inclined_s_laser1_b0.log","r");

  FILE *file1=fopen ("res.log","r");

  Int_t datab1;

  float resx,resy,resrad,resphi, t0, gas, T, c2,rad1,rad11, rad2, rad21, rad3, rad31, err;
  
  while(!feof(file1))
    {
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&resx,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&resy,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&resrad,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&resphi,&c2);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&rad1,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&T,&rad11);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&rad2,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&T,&rad21);
      datab1 = fscanf(file1,"%f %f %f %f %f",&t0,&gas,&T,&rad3,&err);
      datab1 = fscanf(file1,"%f %f %f %f",&t0,&gas,&T,&rad31);
      if (feof(file1)) break;
      
	  if (a=='x')
           {
            hr->Fill(t0,gas,resx);
           }
/*
	    {
	      if (resx<0.01)
		{
		  hr->Fill(t0,gas,resx);
		}
	      else
		{
		  hr->Fill(t0,gas,0.0);
		}
	    }
*/
	  else if (a=='y')
	    hr->Fill(t0,gas,resy);
	  else if (a=='r')
	    hr->Fill(t0,gas,resrad);
	  else if (a=='p')
	    hr->Fill(t0,gas,resphi);
	  else if (a=='c')
	    hr->Fill(t0,gas,(float) c2);
	  else if (a=='e')
	    hr->Fill(t0,gas,rad3-28.56);
	  //else if (a=='a' && !(gas==0 && t0==0))
	  else if (a=='a')
	    {
	      hr2->Fill(t0,gas,resx);
	      hr3->Fill(t0,gas,resy);
	      hr4->Fill(t0,gas,resrad);
	      hr5->Fill(t0,gas,resphi);
	    } 
	  else
	    {cout<<"Funktion nicht vorhanden !"<<endl;break;}
	}
  
  if (a=='a')
    {
      c1->Divide(2,2);
      c1->cd(1);
      hr2->DrawCopy(opt);
      c1->cd(2);
      hr3->DrawCopy(opt);
      c1->cd(3);
      hr4->DrawCopy(opt);
      c1->cd(4);
      hr5->DrawCopy(opt);
    }
  else
    hr->DrawCopy(opt);
  //float minx,miny;
  //cout<<hr->GetMinimumBin()<<endl;;
  //cout<<minx<<" "<<miny<<endl;
  c1->Update();
  if (fclose(file1) != 0)
     cout<<"Datei nicht geschlossen !"<<endl;
}
