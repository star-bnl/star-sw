// $Id: gasTemp_2d_pos.C,v 1.3 2010/04/27 20:48:29 jcs Exp $
//
// $Log: gasTemp_2d_pos.C,v $
// Revision 1.3  2010/04/27 20:48:29  jcs
// Prithwish's changes to automatically name and save the 2d plots in both ps and png format
//
// Revision 1.2  2009/11/22 20:48:30  jcs
// set 2D histogram limits depending on deltaT
//
// Revision 1.1  2009/10/14 15:58:43  jcs
// change and add macros so that in addition to varying t0 and the gas compostion,
// the gas temperature can be varied
//
//

//======================================================================
//
//   gasTemp_2d_pos reads the file produced in StFtpcLaserCalib::MakePs()
//   and draws the reconstructed position histograms
//
//   Inputs to macro:
//      ftpc   - 1 Ftpc West
//               2 Ftpc East
//      lsec   - laser sector 
//      deltaT - delta gas temperature
//        a    - select value(s) to be histogrammed
//               a  Laser I - straight track at lpos1
//               b  Laser II - straight track at lpos2
//               c  Laser III - straight track at lpos3
//               d  all 3 straight lasers
//      opt    - set Draw Options
//               (see ftp://root.cern.ch/root/doc/chapter3.pdf, pages 5-6)
//
//======================================================================

void gasTemp_2d_pos(int ftpc, int lsec, float deltaT, TString a, char *opt)
{

// FTPC WEST
  if (ftpc == 1 ) {
     if (lsec == 1) {
         // Ftpc West lsec 1 (raft #4)
        const Float_t lpos1=11.88;
        const Float_t lpos2=19.55;
        const Float_t lpos3=28.56;
        TH2F *hr1 = new TH2F("hr1","Laser I(west,lsec 1): nominal (11.88cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(west,lsec 1): nominal (19.55cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(west,lsec 1): nominal (28.56cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(west) lsec = "<<lsec<<endl;
     }
     else if (lsec == 2) {
         // Ftpc West lsec 2 (raft #8)
        const Float_t lpos1=11.68;
        const Float_t lpos2=19.45;
        const Float_t lpos3=28.51;
        TH2F *hr1 = new TH2F("hr1","Laser I(west,lsec 2): nominal (11.68cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(west,lsec 2): nominal (19.45cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(west,lsec 2): nominal (28.51cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(west) lsec = "<<lsec<<endl;
     }
     else if (lsec == 3) {
         // Ftpc West lsec 3 (raft #11)
        const Float_t lpos1=11.78;
        const Float_t lpos2=19.35;
        const Float_t lpos3=28.41;
        TH2F *hr1 = new TH2F("hr1","Laser I(west,lsec 3): nominal (11.78cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(west,lsec 3): nominal (19.35cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(west,lsec 3): nominal (28.41cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(west) lsec = "<<lsec<<endl;
     }
     else {
       cout<<"Invalid lsec for ftpc = "<<ftpc<<"(west) lsec = "<<lsec<<endl;
       exit(1);
     }
  }

// FTPC EAST
  if (ftpc == 2 ) {
     if (lsec == 1) {
         // Ftpc East lsec 1 (raft #4)
        const Float_t lpos1=11.75;
        const Float_t lpos2=19.44;
        const Float_t lpos3=28.41;
        TH2F *hr1 = new TH2F("hr1","Laser I(east,lsec 1): nominal (11.75cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(east,lsec 1): nominal (19.44cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(east,lsec 1): nominal (28.41cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(east) lsec = "<<lsec<<endl;
     }
     else if (lsec == 2) {
         // Ftpc East lsec 2 (raft #8)
        const Float_t lpos1=11.85;
        const Float_t lpos2=19.44;
        const Float_t lpos3=28.41;
        TH2F *hr1 = new TH2F("hr1","Laser I(east,lsec 2): nominal (11.85cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(east,lsec 2): nominal (19.44cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(east,lsec 2): nominal (28.41cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(east) lsec = "<<lsec<<endl;
     }
     else if (lsec == 3) {
         // Ftpc East lsec 3 (raft #11)
        const Float_t lpos1=11.86;
        const Float_t lpos2=19.45;
        const Float_t lpos3=28.31;
        TH2F *hr1 = new TH2F("hr1","Laser I(east,lsec 3): nominal (11.86cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr2 = new TH2F("hr2","Laser II(east,lsec 3): nominal (19.45cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        TH2F *hr3 = new TH2F("hr3","Laser III(east,lsec 3): nominal (28.31cm) - reconstructed position",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
        cout<<"Initialized for ftpc = "<<ftpc<<"(east) lsec = "<<lsec<<endl;
     }
     else {
       cout<<"Invalid lsec for ftpc = "<<ftpc<<"(east) lsec = "<<lsec<<endl;
       exit(2);
     }
  }
  
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

  TH2F *hr0 = new TH2F("hr","",12,deltaT-2.00,deltaT+4.00,12,-0.45,0.75); 
  hr0->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr0->GetXaxis()->SetTitle("#Delta T");
  hr1->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr1->GetXaxis()->SetTitle("#Delta T");
  hr2->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr2->GetXaxis()->SetTitle("#Delta T");
  hr3->GetYaxis()->SetTitle("#Delta Ar [%]");
  hr3->GetXaxis()->SetTitle("#Delta T");
  //

char name[100];
sprintf(name,"res.log",lsec);
  
printf("Input file opened %s \n",name);

  FILE *file1=fopen (name,"r");
  
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

	  if (a=='a')
	    {
	      hr0->Fill(T,gas,lpos1-rad1);
	    }
	  else if (a=='b')
	    {
	      hr0->Fill(T,gas,lpos2-rad2);
	    }
	  else if (a=='c')
	    {
	      hr0->Fill(T,gas,lpos3-rad3);
	    }
	  else if (a=='d')
	    {
	      hr1->Fill(T,gas,lpos1-rad1);
	      hr2->Fill(T,gas,lpos2-rad2);
	      hr3->Fill(T,gas,lpos3-rad3);
    }
	  else
	    {cout<<"Function not available !"<<endl;break;}
	}
  if (a=='d')
    {
      c1->Divide(2,2);
      c1->cd(1);
      hr1->DrawCopy(opt);
      c1->cd(2);
      hr2->DrawCopy(opt);
      c1->cd(3);
      hr3->DrawCopy(opt);
    }
  else 
    hr0->DrawCopy(opt);
  
  //float minx,miny;
  //cout<<hr1->GetMinimumBin()<<endl;;
  //cout<<minx<<" "<<miny<<endl;
  c1->Update();

  if(ftpc==1) sprintf(name,"w_lsec%d_2d_Del_g-Del_T%g.ps",lsec,deltaT);
  if(ftpc==2) sprintf(name,"e_lsec%d_2d_Del_g-Del_T%g.ps",lsec,deltaT);
  c1->SaveAs(name);
  if(ftpc==1) sprintf(name,"w_lsec%d_2d_Del_g-Del_T%g.png",lsec,deltaT);
  if(ftpc==2) sprintf(name,"e_lsec%d_2d_Del_g-Del_T%g.png",lsec,deltaT);
  c1->SaveAs(name);

  if (fclose(file1) != 0)
     cout<<"Error closing input file !"<<endl;

}
