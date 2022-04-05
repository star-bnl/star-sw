// $Id: vertex.C,v 1.3 2007/01/19 08:23:07 jcs Exp $
//
// $Log: vertex.C,v $
// Revision 1.3  2007/01/19 08:23:07  jcs
// replace true, false with kTRUE, kFALSE
//
// Revision 1.2  2006/03/15 15:14:06  jcs
// add lines for listing CVS update info
//

// vertex-extrapolation (d.h. plot - program for new Tree-Structure !!!)

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

struct TEVENT
{
  Float_t run;
  Int_t nevent;
};


struct TCLUSTER
{
  Int_t row,sec, padlength, timelength;
  Float_t peakheight, charge;
  Int_t ntracks;
  Float_t padpos, timepos;
  Float_t padpossigma, timepossigma; 
};

struct THIT
{
  Float_t x,y,z;
  Float_t ex,ey,ez;
  Float_t globResX,globResY,globResPhi,globResR;
  Float_t primResX,primResY,primResPhi,primResR;
};

struct TREVENT
{
  Float_t run;
  Int_t nevent;
};

struct TRACK
{
  Float_t px,py,pz;
  Float_t eta,p,pt;
  Int_t npoints;
  Int_t charge;
  Int_t type;
  Int_t sec;
};

struct MVERTEX
{
  Float_t x,y,z;
};

CLUSTER cluster;
HIT hit;
TCLUSTER tcluster;
THIT thit;
TRACK track;
EVENT event;
TEVENT tevent;
TREVENT trevent;
MVERTEX mvertex;


void vertex(TString eingabe,int evt)
{

  TBranch *bhit, *bevent, *bcluster;
  TBranch *bthit, *btcluster, *btevent;
  TBranch *btrevent, *btrack,*btrvertex;
  TDirectory *histdir, *vertexdir;

  TTree *dtree;
  TTree *dttree;
  TTree *dtrtree;

  TStyle *plain  = new TStyle("Plain","Plain Style (no colors/fill areas)");
  plain->SetTitleOffset(1.25);
  plain->SetCanvasBorderMode(0);
  plain->SetPadBorderMode(0);
  plain->SetPadColor(10);
  plain->SetCanvasColor(10);
  plain->SetTitleColor(0);
  plain->SetStatColor(0);
  plain->SetPalette(1);
  plain->SetOptStat(11);
  plain->SetOptFit();

  TString histoname;

  cout<<"Vertex Analysis started..."<<endl;
  cout<<endl;

  TFile *f=new TFile(eingabe+".root");

  eingabe +="-vertex";
  TPostScript *fps=new TPostScript(eingabe+".ps",112);

  // get trees in file !!!

  /*
  dtree=(TTree*) f->Get("cl");
  bhit=dtree->GetBranch("hit");
  bhit->SetAddress(&hit);
  bcluster=dtree->GetBranch("cluster");
  bcluster->SetAddress(&cluster);
  bevent=dtree->GetBranch("event");
  bevent->SetAddress(&event);
  */

  dtrtree=(TTree*) f->Get("tr");
  btrevent=dtrtree->GetBranch("event");
  btrevent->SetAddress(&trevent);
  btrack=dtrtree->GetBranch("track");
  btrack->SetAddress(&track);
  btrvertex=dtrtree->GetBranch("vertex");
  btrvertex->SetAddress(&mvertex);

  dttree=(TTree*) f->Get("clot");
  btcluster=dttree->GetBranch("cluster");
  btcluster->SetAddress(&tcluster);
  bthit=dttree->GetBranch("hit");
  bthit->SetAddress(&thit);
  btevent=dttree->GetBranch("event");
  btevent->SetAddress(&tevent);

  TCanvas *c1 = new TCanvas("vertex","vertex",100,10,800,600);
  TH2F *hr = new TH2F("hr","",100,-275,275,100,0,30);
  hr->SetTitle("Vertex Extrapolation (linear)");
  hr->GetYaxis()->SetTitleOffset(1.1);
  hr->GetYaxis()->SetTitle("rad");
  hr->GetXaxis()->SetTitle("z");
  hr->DrawCopy(); 

  TH1F *hvertex=new TH1F("hvertex","z-position of used vertex",201,-100,100);
  TH1F *vertexeast=new TH1F("vertexeast","z-position of extrapolated vertex east",101,-50,50);
  TH1F *vertexwest=new TH1F("vertexwest","z-position of extrapolated vertex west",101,-50,50);
  TH1F *vertexall=new TH1F("vertexall","z-position of extrapolated vertex",101,-50,50);

  Int_t maxentries1 = (Int_t)btcluster->GetEntries();
  cout<<"Process Cluster-on-Track-Tree with "<<maxentries1<<" clusters..."<<endl; 

  cout<<endl;
  cout<<"Investigate Event "<<evt<<"..."<<endl;
  Float_t mean_vertex=0;
  Float_t mean_vertex_west=0;
  Float_t mean_vertex_east=0;

  Float_t radius[11],z[11];

  Bool_t EndOfEvent=kFALSE;
  Int_t i=0;
  Int_t ntracksold=0;
  Int_t ntrack=0;
  Int_t ntrackeast=0;
  Int_t ntrackwest=0;

  TF1 *linfit=new TF1("linfit","[0]*x+[1]",-275,275);
  linfit->SetParameters(1,10);
  linfit->SetLineWidth(0.5);

  for (int k=0;k<=maxentries1;k++)
    {
      btcluster->GetEntry(k);
      bthit->GetEntry(k);
      btevent->GetEntry(k);
      
      if (EndOfEvent) break;
      if (tevent.nevent > evt) 
       {
         if (i==0) break;
         tevent.nevent=evt;
         EndOfEvent=kTRUE;
       }
      if (tevent.nevent==evt)
	{
	  if (tcluster.ntracks != ntracksold)
	    {

	      TGraph *ver=new TGraph(i,z,radius);
	      ver->SetMarkerStyle(22);
	      ver->SetMarkerSize(0.75);
	      ver->Draw("P");

	      if (z[0]<0)
		{
		  ntrackeast++;
		  ver->SetMarkerColor(2);
		  linfit->SetLineColor(2);
		}
	      else
		{
		  ntrackwest++;
		  ver->SetMarkerColor(3);
		  linfit->SetLineColor(3);
		}

	      ver->Fit(linfit,"QR+"); 

	      cout<<"Event "<<tevent.nevent<<" Track "<<ntrack<<" Vertex = "<<-linfit->GetParameter(1)/linfit->GetParameter(0)<<endl;
	      ntrack++;
	      mean_vertex=(mean_vertex+(-linfit->GetParameter(1)/linfit->GetParameter(0)));
	      vertexall->Fill(-linfit->GetParameter(1)/linfit->GetParameter(0));
	      if (z[0]<0)
		{
		  mean_vertex_east=mean_vertex_east+-linfit->GetParameter(1)/linfit->GetParameter(0);
		  vertexeast->Fill(-linfit->GetParameter(1)/linfit->GetParameter(0));
		}
	      else
		{
		  mean_vertex_west=mean_vertex_west+-linfit->GetParameter(1)/linfit->GetParameter(0);	
		  vertexwest->Fill(-linfit->GetParameter(1)/linfit->GetParameter(0));
		}
	      i=0;

	    }
	  radius[i]=sqrt(thit.x*thit.x+thit.y*thit.y);
	  z[i]=thit.z;
          cout<<"DEBUG: ntrack = "<<tcluster.ntracks<<" cluster "<<i<<" x,y,z = "<<thit.x<<" "<<thit.y<<" "<<thit.z<<" radius = "<<radius[i]<<endl;
	  i++;
	    
	  ntracksold=tcluster.ntracks;
	}
    }
  cout<<"---------------------------"<<endl;
  cout<<"Mean_vertex = "<<mean_vertex/ntrack<<endl;
  cout<<"Mean_vertex_east = "<<mean_vertex_east/ntrackeast<<endl;
  cout<<"Mean_vertex_west = "<<mean_vertex_west/ntrackwest<<endl;
  cout<<"---------------------------"<<endl;

  TCanvas *c2 = new TCanvas("vertex2","vertex2",150,60,850,650);

  vertexall->DrawCopy();
  vertexwest->SetLineColor(3);vertexwest->DrawCopy("same");
  vertexeast->SetLineColor(2);vertexeast->DrawCopy("same");
  TLegend *leg = new TLegend(0.7,0.6,0.95,0.8);
  leg->SetFillColor(10);
  leg->SetTextSize(0.03);
  leg->AddEntry(vertexall,"vertex East&West","L");
  leg->AddEntry(vertexeast,"vertex East","L");
  leg->AddEntry(vertexwest,"vertex West","L");
  leg->Draw();
  //cout<<vertexwest->GetEntries()+ vertexeast->GetEntries()<<endl;
  Int_t maxentries2 = (Int_t)btrack->GetEntries();
  cout<<endl;
  cout<<"Process Track-Tree with "<<maxentries2<<" Tracks..."<<endl;  
  
  Int_t oldevent=0;
  for (int k=0;k<=maxentries2;k++)
  //for (int k=0;k<=1;k++)
    {
      btrack->GetEntry(k);
      btrvertex->GetEntry(k);
      btrevent->GetEntry(k);
      if (trevent.nevent!=oldevent)
	{
	  hvertex->Fill(mvertex.z);
	  if (trevent.nevent==evt)
	    {
	      cout<<"---------------------------"<<endl;
	      cout<<"Used Vertex (Event "<<evt<<")  x = "<<mvertex.x<<" y = "<<mvertex.y<<" z = "<<mvertex.z<<endl;
	      cout<<"---------------------------"<<endl;
	    }
	}
      oldevent=trevent.nevent;
    }
 
  cout<<endl;
  //cout<<"Analyse beendet !"<<endl;
  cout<<"Histograms in ROOT-file : "<<eingabe+".root"<<endl;

  //ana->Write();
  //ana->Close();
  //f->Close();

  //return 0;
  //c1->Delete();
}

