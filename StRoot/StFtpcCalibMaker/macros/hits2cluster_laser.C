// $Id: hits2cluster_laser.C,v 1.4 2008/05/16 18:36:56 jcs Exp $
//
// $Log: hits2cluster_laser.C,v $
// Revision 1.4  2008/05/16 18:36:56  jcs
// update FTPC calibration macros
//
// Revision 1.3  2007/01/19 08:23:07  jcs
// replace true, false with kTRUE, kFALSE
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

struct HIT
{
  Float_t x,y,z;
  Float_t rad,phi;
  Float_t raderror,phierror;
};

struct CLUSTER
{
  Float_t timepos,padpos,timesigma,padsigma;
  Float_t peakheight, charge;
  Int_t timebin,pad;
  Int_t padlength,timelength;
  Int_t row,sec;
  Int_t flag;
  Int_t numpeaks;
};

struct EVENT
{
  Float_t run;
  Int_t nevent;
};


void hits2cluster_laser(TString eingabe,int evt,int laser_sec)
{

  CLUSTER cluster;
  HIT hit;
  EVENT event;

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
  gStyle->SetOptStat(11);
  gStyle->SetOptFit();

  Float_t fpad,ftime,fflag,frow, fsec;
  Int_t maxentries;

  TString histoname;

  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  c1->Divide(2,1);

  cout<<"Macro cluster_hits..."<<endl;
  TFile *f = new TFile(eingabe+".root");
  //TFile *f2=new TFile(eingabe+"-flag.root","RECREATE");
  eingabe +="_evt_";
  eingabe +=evt;
  eingabe +="_lsec_";
  eingabe +=laser_sec;
  eingabe +=".ps";

  //eingabe +="-h2cl_gain.ps";
  TPostScript *fps=new TPostScript(eingabe,112);
  
  // read in ntuple to get hit position and flag

  dtree=(TTree*) f->Get("cl");
  bhit=dtree->GetBranch("hit");
  bhit->SetAddress(&hit);
  bcluster=dtree->GetBranch("cluster");
  bcluster->SetAddress(&cluster);
  bevent=dtree->GetBranch("event");
  bevent->SetAddress(&event);

  topdir=(TDirectory*) f->Get("histograms");

  TString dirname="evt_";
  dirname +=evt;
  dirname +="_gain";

  //sprintf(dirname,"evt_%d",evt);
  

  histdir=(TDirectory*) topdir->Get(dirname);

  //topdir->cd();
  //histdir->cd();
  
  int maxentries = (int) bcluster->GetEntries();
  
  for (int i=1;i<=60;i++)
    {
      if (laser_sector(1,laser_sec,i))
      {
	fps->NewPage();
	for(int j=1;j<=2;j++)
	  {
	    histoname="hsec";
	    histoname +=i;
	    histoname +="_hrow";
	    histoname +=j;
	    histoname +="_gain";
	    TH1F *histo=(TH1F*) histdir->Get(histoname);
	    c1->cd(j);
	    //histo->SetMarkerSize(0.25);
	    //histo->DrawCopy("text");
	    histo->DrawCopy("colz");
	    TH2F *flag0=new TH2F("flag0","flag0",160,0,160,255,0,255);
	    flag0->SetMarkerStyle(20);flag0->SetMarkerSize(0.5);flag0->SetMarkerColor(1);
	    TH2F *flag1=new TH2F("flag1","flag1",160,0,160,255,0,255);
	    flag1->SetMarkerStyle(29);flag1->SetMarkerSize(0.5);flag1->SetMarkerColor(7);
	    TH2F *flag2=new TH2F("flag2","flag2",160,0,160,255,0,255);
	    flag2->SetMarkerStyle(21);flag2->SetMarkerSize(0.5);flag2->SetMarkerColor(3);
	    TH2F *flag3=new TH2F("flag3","flag3",160,0,160,255,0,255);
	    flag3->SetMarkerStyle(23);flag3->SetMarkerSize(0.5);flag3->SetMarkerColor(5);
	    TH2F *flag4=new TH2F("flag4","flag4",160,0,160,255,0,255);
	    flag4->SetMarkerStyle(29);flag4->SetMarkerSize(0.5);flag4->SetMarkerColor(3);
	    TH2F *flag5=new TH2F("flag5","flag5",160,0,160,255,0,255);
	    flag5->SetMarkerStyle(22);flag5->SetMarkerSize(0.5);flag5->SetMarkerColor(6);
	    TH2F *flag10=new TH2F("flag10","flag10",160,0,160,255,0,255);
	    flag10->SetMarkerStyle(20);flag10->SetMarkerSize(0.5);flag10->SetMarkerColor(2);
	    TH2F *flag11=new TH2F("flag11","flag11",160,0,160,255,0,255);
	    flag11->SetMarkerStyle(29);flag11->SetMarkerSize(0.5);flag11->SetMarkerColor(7);
	    TH2F *flag12=new TH2F("flag12","flag12",160,0,160,255,0,255);
	    flag12->SetMarkerStyle(29);flag12->SetMarkerSize(0.5);flag12->SetMarkerColor(3);
	    TH2F *flag13=new TH2F("flag13","flag13",160,0,160,255,0,255);
	    flag13->SetMarkerStyle(23);flag13->SetMarkerSize(0.5);flag13->SetMarkerColor(5);
	    TH2F *flag14=new TH2F("flag14","flag14",160,0,160,255,0,255);
	    flag14->SetMarkerStyle(29);flag14->SetMarkerSize(0.5);flag14->SetMarkerColor(3);
	    TH2F *flag15=new TH2F("flag15","flag15",160,0,160,255,0,255);
	    flag15->SetMarkerStyle(22);flag15->SetMarkerSize(0.5);flag15->SetMarkerColor(6);
	    TH2F *flag16=new TH2F("flag16","flag16",160,0,160,255,0,255);
	    flag16->SetMarkerStyle(28);flag16->SetMarkerSize(0.5);flag16->SetMarkerColor(8);
	    
	    for (int k=0;k<=maxentries;k++)
	      {
		bevent->GetEntry(k);
		bcluster->GetEntry(k);
		
		if (event.nevent==evt)
		  {
		    //cout<<"event.nevent = "<<event.nevent<<endl;
		    
		    if (cluster.sec==i && cluster.row==j)
		      {    
			//cout<<"sec = "<<cluster.sec<<" row =  "<<cluster.row<<endl;
			//cout<<"flag= "<<cluster.flag<<endl;
			//cout<<" padpos = "<<cluster.padpos<<" timepos = "<<cluster.timepos<<endl;
			
			if (cluster.flag==0) {flag0->Fill(cluster.padpos,cluster.timepos);}
		      
			if (cluster.flag==1) {flag1->Fill(cluster.padpos,cluster.timepos);}
   
			if (cluster.flag==2) {flag2->Fill(cluster.padpos,cluster.timepos);}
			
			if (cluster.flag==3) {flag3->Fill(cluster.padpos,cluster.timepos);}
			
			if (cluster.flag==4) {flag4->Fill(cluster.padpos,cluster.timepos);}
			
			if (cluster.flag==5) {flag5->Fill(cluster.padpos,cluster.timepos);}
			
			if (cluster.flag==10) {flag10->Fill(cluster.padpos,cluster.timepos);}
		     
			if (cluster.flag==11) {flag11->Fill(cluster.padpos,cluster.timepos);}
			
			if (cluster.flag==12) {flag12->Fill(cluster.padpos,cluster.timepos);}

			if (cluster.flag==13) {flag13->Fill(cluster.padpos,cluster.timepos);}

			if (cluster.flag==14) {flag14->Fill(cluster.padpos,cluster.timepos);}

			if (cluster.flag==15) {flag15->Fill(cluster.padpos,cluster.timepos);}
		      
			if (cluster.flag>=16) {flag16->Fill(cluster.padpos,cluster.timepos);}
		      
		     
		      }
		  }
	      }
		  
	  flag0->DrawCopy("same");
	  flag1->DrawCopy("same");
	  flag2->DrawCopy("same");
	  flag3->DrawCopy("same");
	  flag4->DrawCopy("same");
	  flag5->DrawCopy("same");
	  
	  flag10->DrawCopy("same");
	  flag11->DrawCopy("same");
	  flag12->DrawCopy("same");
	  flag13->DrawCopy("same");
	  flag14->DrawCopy("same");
	  flag15->DrawCopy("same");
	  flag16->DrawCopy("same");
	  TLegend *leg = new TLegend(0.46,0.72,0.80,0.86);
	  leg->SetFillColor(10);
	  leg->SetTextSize(0.03);
	  leg->AddEntry(flag0,"flag=0","P");
	  leg->AddEntry(flag10,"flag=10","P");
	  leg->AddEntry(flag11,"flag=1 & flag=11","P");
	  leg->AddEntry(flag3,"flag=3 & flag=13","P");
	  leg->AddEntry(flag4,"flag=4 & flag=14","P");
	  leg->AddEntry(flag15,"flag=5 & flag=15","P");
	  leg->AddEntry(flag16,"flag=6 & flag=16","P");
	  //leg->AddEntry(flag11,"flag==11","P");
	  //leg->AddEntry(flag14,"flag==14","P");
	  //leg->AddEntry(flag15,"flag==15","P");
	  //leg->AddEntry(flag16,"flag==16","P"); 
	  leg->Draw();
	  c1->Update();
	  flag0->Delete();
	  flag1->Delete();
	  flag2->Delete();
	  flag3->Delete();
	  flag4->Delete();
	  flag5->Delete();
	  
	  flag10->Delete();
	  flag11->Delete();
	  flag12->Delete();
	  flag13->Delete();
	  flag14->Delete();
	  flag15->Delete();
	  flag16->Delete();
	  //leg->Delete();
	  
	  }
      }
      else break
    }     
 
  cout<<"Ps-file created !"<<endl;
  //fps->Close();
  //f2->Write();
  //f2->Close();
  f->Close();
  leg->Delete();
  c1->Delete();
}

//---------------------------------------------------------------

bool laser_sector(int whichftpc,int whichsec,int sec)
{
  if (whichftpc==2)
      {
	switch(whichsec)
	  {

	  case 0:
	    // all sectors
	    if (sec>30)
	      return kTRUE;
	    break;
	  case 1:
	    if (sec==32 || sec==38 || sec==44 || sec==50 || sec==56)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 2:
	    if (sec==34 || sec==40 || sec==46 || sec==52 || sec==58)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 3:
	    if (sec==36 || sec==42 || sec==48 || sec==54 || sec==60)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  default :
	    {
	      cout<<"ERROR : Kein gueltiger Lasersector !"<<endl;
	      return kFALSE;
	    }
	  }
      }
  else if (whichftpc==1)
    {
      switch(whichsec)
	{
	case 0:
	  // all sectors
	  if (sec<31)
	    return kTRUE;
	  break;
	 case 1:
	    if (sec==2 || sec==8 || sec==14 || sec==20 || sec==26)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 2:
	    if (sec==4 || sec==10 || sec==16 || sec==22 || sec==28)
	      return kTRUE;
	    else
	      return kFALSE;
	    break;
	  case 3:
	    if (sec==6 || sec==12 || sec==18 || sec==24 || sec==30)
	      return kTRUE;
	    else
	      return kFALSE;
	    break; 
	default :
	  {
	    cout<<"ERROR : Kein gueltiger Lasersector !"<<endl;
	    return kFALSE;
	  }
	}
    }
  else {
    cout<<"ERROR : Keine FTPC gewaehlt !"<<endl;
    return kFALSE;
  }
}
