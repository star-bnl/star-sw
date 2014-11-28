// $Id: print_TCluster.C,v 1.1 2010/01/14 18:25:16 jcs Exp $
//
// $Log: print_TCluster.C,v $
// Revision 1.1  2010/01/14 18:25:16  jcs
// macro to print out the clusters on tracks in the FTPC root file
//
//

// Print contents of struct TCluster  - cluster on track information 

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


void print_TCluster(TString eingabe)
{

  TBranch *bhit, *bevent, *bcluster;
  TBranch *bthit, *btcluster, *btevent;
  TBranch *btrevent, *btrack,*btrvertex;
  TDirectory *histdir, *vertexdir;

  TTree *dtree;
  TTree *dttree;
  TTree *dtrtree;


  cout<<"Clusters on Tracks Analysis started..."<<endl;
  cout<<endl;

  TFile *f=new TFile(eingabe+".root");


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

  Int_t maxentries1 = (Int_t)btcluster->GetEntries();
  cout<<"Process Cluster-on-Track-Tree with "<<maxentries1<<" clusters..."<<endl; 

  cout<<endl;

  Int_t i=0;
  Int_t ntracksold=0;
  Int_t ntrack=0;
  Int_t nevent=0;

  for (int k=0;k<=maxentries1;k++)
    {
      btcluster->GetEntry(k);
      bthit->GetEntry(k);
      btevent->GetEntry(k);
      if (nevent==0){
        nevent = tevent.nevent;
        cout<<endl;
        cout<<"Event "<<nevent<<endl;
      }
      
      if (tevent.nevent != nevent) 
       {
         if (i==0) break;
         nevent = tevent.nevent;
        cout<<endl;
        cout<<"Event "<<nevent<<endl;
       }
      if (tevent.nevent==nevent)
	{
	  if (tcluster.ntracks != ntracksold)
	    {
              cout<<endl;
	      ntrack++;
	      i=0;
	    }
          cout<<"ntrack = "<<tcluster.ntracks<<" cluster "<<i<<" padpos "<<tcluster.padpos<<" timepos "<<tcluster.timepos<<" x,y,z = "<<thit.x<<" "<<thit.y<<" "<<thit.z<<endl;
//          cout<<"ntrack = "<<tcluster.ntracks<<" padpos "<<tcluster.padpos<<" timepos "<<tcluster.timepos<<endl;
	  i++;
	    
	  ntracksold=tcluster.ntracks;
	}
    }

}

