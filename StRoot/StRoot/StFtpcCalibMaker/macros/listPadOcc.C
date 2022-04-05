// List pad occupancy for all clusters in all sectors which pass the 
// cuts (3<=padlength<=6 and 4<=timelength<=8) for a "good" cluster

// structures definitions

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
  Float_t temperature,pressure;
};

// =====================================

void listPadOcc(const Char_t *fileName="filelist.txt")
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
  gStyle->SetOptStat(1111);
  gStyle->SetOptFit();

  Int_t maxentries;
  Int_t nevents=0;
  
  Int_t PadEntries[2][60][160];

   // Clear pad entries
   for (int isec=0;isec<60;isec++){
      for (int irow=0;irow<2;irow++){
         for (int ipad=0;ipad<160;ipad++){
           PadEntries[irow][isec][ipad]=0;
         }
      }
    }
// input

//++++++++++++++++++++++++++++++++++++++++++++++++++
// read file list from text file (maximum of 200 files)
  char **fList = new char[200][256];
  int nfile=0;
  ifstream fin(fileName);
  if (fin) {
    int j=0;
    cout << "Input file " << fileName << " found." << endl;
    for (Int_t i=0;i<200;i++){
      char *line = new char[256];
      fin >> line;
      if (fin.eof()) break;
      fList[j] = line;
      cout << " Added file : " << fList[j] << endl;
      j++;
    }
    nfile = j;
  }
  fin.close();

//++++++++++++++++++++++++++++++++++++++++++++++++++

  for (Int_t ifile=0;ifile<nfile;ifile++) {
     TString eingabe = fList[ifile];
     TFile f(eingabe);

  // read in ntuple
  // ==============

  TTree *dtree=(TTree*) f.Get("cl");
  TBranch *bcluster=dtree->GetBranch("cluster");
  bcluster->SetAddress(&cluster);
  TBranch *bevent=dtree->GetBranch("event");
  bevent->SetAddress(&event);
  
  bevent->GetEntry(0);
//  cout<<"Event number "<<event.nevent<<endl;
  Int_t CurrentEvent = event.nevent;
  nevents++;
  maxentries=bcluster->GetEntries();
  //cout<<"maxentries "<<maxentries<<endl;
  
  for (int i=0;i<=maxentries;i++) {
      bevent->GetEntry(i);
      if (event.nevent != CurrentEvent){
//        cout<<"Event number "<<event.nevent<<endl;
        CurrentEvent = event.nevent;
        nevents++;
      }
      bcluster->GetEntry(i);
      // Cut on #pad and #timebins for "good" clusters
      if ((cluster.padlength>=3&&cluster.padlength<=6) &&
          (cluster.timelength>=4&&cluster.timelength<=8)){
//cout<<"cluster.row "<<cluster.row<<" cluster.sec "<<cluster.sec<<" cluster.pad"<<endl;
          PadEntries[cluster.row-1][cluster.sec-1][cluster.pad-1]++;
      } 
   }
 }

   cout<<nevents<<" Events analyzed"<<endl;

   cout<<endl;
   cout<<" Pad occupancy for all clusters in all sectors which passed the cuts (3<=padlength<=6 and 4<=timelength<=8) for a 'good' cluster:"<<endl;
   cout<<endl;

   Int_t npad; 

   // Padrows 1 & 2
   for (int irow=0;irow<2;irow++){
      cout<<"Padrow "<<irow+1<<endl;
      npad=0;
      for (int isec=0;isec<6;isec++){
         for (int ipad=0;ipad<160;ipad++){
         // write out all entries
           cout<<irow+1<<" "<<isec+1<<" "<<npad<<" ("<<ipad+1<<") "<<PadEntries[irow][isec][ipad]<<endl;
           npad++;
         }
      }
    }

   // Padrows 3 & 4
   for (int irow=0;irow<2;irow++){
      cout<<"Padrow "<<irow+3<<endl;
      npad=0;
      for (int isec=6;isec<12;isec++){
         for (int ipad=0;ipad<160;ipad++){
         // write out all entries
           cout<<irow+1<<" "<<isec+1<<" "<<npad<<" ("<<ipad+1<<") "<<PadEntries[irow][isec][ipad]<<endl;
           npad++;
         }
      }
    }

   // Padrows 5 & 6
   for (int irow=0;irow<2;irow++){
      cout<<"Padrow "<<irow+5<<endl;
      npad=0;
      for (int isec=12;isec<18;isec++){
         for (int ipad=0;ipad<160;ipad++){
         // write out all entries
           cout<<irow+1<<" "<<isec+1<<" "<<npad<<" ("<<ipad+1<<") "<<PadEntries[irow][isec][ipad]<<endl;
           npad++;
         }
      }
    }

   // Padrows 7 & 8
   for (int irow=0;irow<2;irow++){
      cout<<"Padrow "<<irow+7<<endl;
      npad=0;
      for (int isec=18;isec<24;isec++){
         for (int ipad=0;ipad<160;ipad++){
         // write out all entries
           cout<<irow+1<<" "<<isec+1<<" "<<npad<<" ("<<ipad+1<<") "<<PadEntries[irow][isec][ipad]<<endl;
           npad++;
         }
      }
    }

   // Padrows 9 & 10
   for (int irow=0;irow<2;irow++){
      cout<<"Padrow "<<irow+9<<endl;
      npad=0;
      for (int isec=24;isec<30;isec++){
         for (int ipad=0;ipad<160;ipad++){
         // write out all entries
           cout<<irow+1<<" "<<isec+1<<" "<<npad<<" ("<<ipad+1<<") "<<PadEntries[irow][isec][ipad]<<endl;
           npad++;
         }
      }
    }

}

