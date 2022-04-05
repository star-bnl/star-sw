ttime() {
  TString path="/star/institutions/iucf/balewski/2008-L2-tmp/day333/"; 

 int runL[]={113,115,116,117,118,120,121,122,123,124,125};
  TString jay[]={"01","02","03","04","05","06","07","08","09","10","11","12"};
  TString kay[]={"a","b","c","d","e","f","g","h","i","j"};
  TString ell[]={"01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40"};
  TString craten[]={"01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","a","b","c","d","e","10","11","12","13","14","15","16","17","18","19","1a","1b","1c","1d","1e"}; 
  TString crate1[]={"0","0","0","0","0","0","0","0","0","1","1","1","1","1","1","1","1","1","1","2","2","2","2","2","2","2","2","2","2","3"};
  TString crate2[]={"1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0","1","2","3","4","5","6","7","8","9","0"};


  c=new TCanvas();
  c->Divide(6,5);



  Float_t es[11],ese[11];
  Float_t ws[11],wse[11];
  Float_t td[11];
  Float_t csum[30][11];
  Float_t mean;
  Int_t iiii=0;
  Int_t jc;
  Float_t gg,sum;

  e1= new TF1("e1","expo",60,200);
  w1= new TF1("w1","expo",60,200);

  TString htitle,title,crate,icrate;
  int i,j,k,l,ic,icc,iccc;
  for(i=0;i<11;i++) {
    cout<<" i = "<<i<<endl;
    TString fname=path+"run8333";
    fname+=runL[i];
    fname+=".l2ped.hist.root";
    fd1=new TFile(fname); assert(fd1->IsOpen());

    if(i==0)  east0 = new TH1D("east0","east0",500,0,500);
    if(i==1)  east1 = new TH1D("east1","east1",500,0,500);
    if(i==2)  east2 = new TH1D("east2","east2",500,0,500);
    if(i==3)  east3 = new TH1D("east3","east3",500,0,500);
    if(i==4)  east4 = new TH1D("east4","east4",500,0,500);
    if(i==5)  east5 = new TH1D("east5","east5",500,0,500);
    if(i==6)  east6 = new TH1D("east6","east6",500,0,500);
    if(i==7)  east7 = new TH1D("east7","east7",500,0,500);
    if(i==8)  east8 = new TH1D("east8","east8",500,0,500);
    if(i==9)  east9 = new TH1D("east9","east9",500,0,500);
    if(i==10)  east10 = new TH1D("east10","east10",500,0,500);


    if(i==0)  west0 = new TH1D("west0","west0",500,0,500);
    if(i==1)  west1 = new TH1D("west1","west1",500,0,500);
    if(i==2)  west2 = new TH1D("west2","west2",500,0,500);
    if(i==3)  west3 = new TH1D("west3","west3",500,0,500);
    if(i==4)  west4 = new TH1D("west4","west4",500,0,500);
    if(i==5)  west5 = new TH1D("west5","west5",500,0,500);
    if(i==6)  west6 = new TH1D("west6","west6",500,0,500);
    if(i==7)  west7 = new TH1D("west7","west7",500,0,500);
    if(i==8)  west8 = new TH1D("west8","west8",500,0,500);
    if(i==9)  west9 = new TH1D("west9","west9",500,0,500);
    if(i==10) west10 = new TH1D("west10","west10",500,0,500);


    for(j=0;j<12;j++){
     for(k=0;k<10;k++){
      for(l=0;l<40;l++){
	//        if(l!=11&&l!=12&&l!=27&&l!=28)continue;
      htitle="a"+jay[j]+"t"+kay[k]+ell[l];
    h1=(TH1F*)fd1->Get(htitle); assert(h1);
    title= h1->GetTitle();
    crate =title[19]+title[20];
     for(icc=0;icc<30;icc++){  

      if(crate1[icc] == title[19] && crate2[icc] == title[20]){
        iccc=icc;
	iiii+=1;
        h1->SetAxisRange(10.,1000.);
        mean = h1->GetMean();
        sum  = h1->Integral();
        if(mean<15)continue;
        if(mean>60)continue;
        if(sum<100)continue;

        if(icc>14&&j==8)continue;
	
        h1->SetAxisRange(mean+50.,mean+100.);
        sum=2400.*(h1->Integral())/sum;
	if(sum>1.5)cout<<"crate = "<<icc<<"j k l"<<j<<" "<<k<<" "<<l<<endl; 
       if(sum>1.5)continue;

    h1->SetAxisRange(mean+60.,mean+300.);
    sum=h1->Integral();
    csum[iccc][i] += sum;


    if(iccc<15 && i==0){
      for(jc=0;jc<300;jc++){
	gg =h1->GetBinContent(mean+jc+1);
        east0->Fill(20+jc,gg);
    }
    }
    if(iccc<15 && i==1){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east1->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==2){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east2->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==3){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east3->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==4){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east4->Fill(20+jc,gg);
      }
    }

    if(iccc<15 && i==5){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east5->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==6){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east6->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==7){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east7->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==8){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east8->Fill(20+jc,gg);
      }
    }
    if(iccc<15 && i==9){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east9->Fill(20+jc,gg);
      }
    }

    if(iccc<15 && i==10){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        east10->Fill(20+jc,gg);
      }
    }

    if(iccc>14 && i==0){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west0->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==1){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west1->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==2){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west2->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==3){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west3->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==4){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west4->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==5){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west5->Fill(20+jc,gg);
      }
    }

    if(iccc>14 && i==6){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west6->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==7){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west7->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==8){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west8->Fill(20+jc,gg);
      }
    }
    if(iccc>14 && i==9){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west9->Fill(20+jc,gg);
      }
    }

    if(iccc>14 && i==10){
      for(jc=0;jc<300;jc++){
        gg =h1->GetBinContent(mean+jc+1);
        west10->Fill(20+jc,gg);
      }
    }




      }
      }
      }
     }
    }


    if(i==0)east0->Fit("e1","RO");
    if(i==1)east1->Fit("e1","RO");
    if(i==2)east2->Fit("e1","RO");
    if(i==3)east3->Fit("e1","RO");
    if(i==4)east4->Fit("e1","RO");
    if(i==5)east5->Fit("e1","RO");
    if(i==6)east6->Fit("e1","RO");
    if(i==7)east7->Fit("e1","RO");
    if(i==8)east8->Fit("e1","RO");
    if(i==9)east9->Fit("e1","RO");
    if(i==10)east10->Fit("e1","RO");

    if(i==0)west0->Fit("w1","RO");
    if(i==1)west1->Fit("w1","RO");
    if(i==2)west2->Fit("w1","RO");
    if(i==3)west3->Fit("w1","RO");
    if(i==4)west4->Fit("w1","RO");
    if(i==5)west5->Fit("w1","RO");
    if(i==6)west6->Fit("w1","RO");
    if(i==7)west7->Fit("w1","RO");
    if(i==8)west8->Fit("w1","RO");
    if(i==9)west9->Fit("w1","RO");
    if(i==10)west10->Fit("w1","RO");


      td[i]=12 + 5.*i; 

      ese[i]=0.;

      es[i]=e1->GetParameter(1);
      if(es[i]!=0.){
       es[i]=-1./es[i];
      ese[i]=es[i]**2*(e1->GetParError(1));
      }     
      ese[i]=0.;
                                                                                                               
      ws[i]=w1->GetParameter(1);
      if(ws[i]!=0.){
	ws[i]=-1./ws[i];
	wse[i]=ws[i]**2*(w1->GetParError(1));
      }

      cout<<es[i]<<" "<<ws[i]<<endl;
  }



  c1->SetLogy();

  Float_t wp[7],ep[7],xp[7];
 
  gStyle->SetOptStat(0);
                                                                                
  c2 = new TCanvas("c2","Gain vs Delay", 200, 10, 700, 500);
  //  TH2F *hr = new TH2F("hr","East Crates Gain vs Delay: Day 333",1,10,70,1,0,25);
  //  hr->SetYTitle("Gain");
  //  hr->SetXTitle("Delay (ns)");
  //  hr->Draw();
 
  graph1 = new TGraphErrors(11,td,es,0,ese);

  graph1->SetTitle("East Crates:  Gain vs TCD Phase");
  graph1->GetXaxis()->SetTitle("TCD Phase (ns)");
  graph1->GetYaxis()->SetTitle("Average Gain");

  graph1->SetMarkerStyle(21);
  graph1->SetMarkerSize(1.0);
  graph1->SetMarkerColor(1);
  graph1->Draw("AP");

  for(j=0;j<7;j++){
    xp[j]=td[j+2];
    wp[j]=es[j+2];
    ep[j]=ese[j+2];
  }

  graph1a = new TGraphErrors(6,xp,wp,0,ep);
  graph1a->SetMarkerStyle(21);
  graph1a->SetMarkerSize(1.0);
  graph1a->SetMarkerColor(2);
  graph1a->Draw("SP");

  graph1a->Fit("gaus");

  gStyle->SetOptStat(0);
 

  c3 = new TCanvas("c3","Gain vs Delay", 200, 10, 700, 500);

 
  graph2 = new TGraphErrors(11,td,ws,0,wse);

  graph2->SetTitle("West Crates:  Gain vs TCD Phase");
  graph2->GetXaxis()->SetTitle("TCD Phase (ns)");
  graph2->GetYaxis()->SetTitle("Average Gain");

  graph2->SetMarkerStyle(21);
  graph2->SetMarkerSize(1.0);
  graph2->SetMarkerColor(1);
  graph2->Draw("AP");
 
  for(j=0;j<7;j++){
    xp[j]=td[j+2];
    wp[j]=ws[j+2];
    ep[j]=wse[j+2];
  }
                                                                                
  graph2a = new TGraphErrors(6,xp,wp,0,ep);
  graph2a->SetMarkerStyle(21);
  graph2a->SetMarkerSize(1.0);
  graph2a->SetMarkerColor(2);
  graph2a->Draw("SP");

  graph2a->Fit("gaus");

  ofstream out;
  out.open("timing.tex");

  Int_t is,iz,icc;
  for(is=0;is<11;is++){
   for(icc=0;icc<30;icc++){
     iz=icc+1; 
     out<<is<<" "<<iz<<" "<<csum[icc][is]<<endl;
  }
  }

  ofstream out1;
  out1.open("timing0.tex");

  Int_t i;
  for(i=0;i<11;i++){
    out1<<i<<" "<<es[i]<<" "<<ese[i]<<" "<<ws[i]<<" "<<wse[i]<<endl;
  }

  cout<<"finished writing output"<<endl;

}
    //   if(i>4) continue;
    //    c->cd(1+i); h1->Draw(); gPad->SetLogy(); h1->SetAxisRange(40.,100.);

