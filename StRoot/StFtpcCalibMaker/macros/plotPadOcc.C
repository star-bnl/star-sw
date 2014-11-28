// Plot pad occupancy for all clusters in all sectors which pass the 
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

void plotPadOcc(const Char_t *fileName="filelist.txt")
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

  Int_t secrow[61][3][161];
  
  for (Int_t isec=1;isec<61;isec++) {
     for (Int_t irow=1;irow<3;irow++) {
        for (Int_t ipad=1;ipad<161;ipad++) {
         secrow[isec][irow][ipad]=0;
        }
     }
  }

// Ftpc West
  sec1row1 = new TH1I("hsec1_hrow1","hsec1_hrow1",160,0,160);
  sec2row1 = new TH1I("hsec2_hrow1","hsec2_hrow1",160,0,160);
  sec3row1 = new TH1I("hsec3_hrow1","hsec3_hrow1",160,0,160);
  sec4row1 = new TH1I("hsec4_hrow1","hsec4_hrow1",160,0,160);
  sec5row1 = new TH1I("hsec5_hrow1","hsec5_hrow1",160,0,160);
  sec6row1 = new TH1I("hsec6_hrow1","hsec6_hrow1",160,0,160);
  sec1row2 = new TH1I("hsec1_hrow2","hsec1_hrow2",160,0,160);
  sec2row2 = new TH1I("hsec2_hrow2","hsec2_hrow2",160,0,160);
  sec3row2 = new TH1I("hsec3_hrow2","hsec3_hrow2",160,0,160);
  sec4row2 = new TH1I("hsec4_hrow2","hsec4_hrow2",160,0,160);
  sec5row2 = new TH1I("hsec5_hrow2","hsec5_hrow2",160,0,160);
  sec6row2 = new TH1I("hsec6_hrow2","hsec6_hrow2",160,0,160);

  sec7row1 = new TH1I("hsec7_hrow1","hsec7_hrow1",160,0,160);
  sec8row1 = new TH1I("hsec8_hrow1","hsec8_hrow1",160,0,160);
  sec9row1 = new TH1I("hsec9_hrow1","hsec9_hrow1",160,0,160);
  sec10row1 = new TH1I("hsec10_hrow1","hsec10_hrow1",160,0,160);
  sec11row1 = new TH1I("hsec11_hrow1","hsec11_hrow1",160,0,160);
  sec12row1 = new TH1I("hsec12_hrow1","hsec12_hrow1",160,0,160);
  sec7row2 = new TH1I("hsec7_hrow2","hsec7_hrow2",160,0,160);
  sec8row2 = new TH1I("hsec8_hrow2","hsec8_hrow2",160,0,160);
  sec9row2 = new TH1I("hsec9_hrow2","hsec9_hrow2",160,0,160);
  sec10row2 = new TH1I("hsec10_hrow2","hsec10_hrow2",160,0,160);
  sec11row2 = new TH1I("hsec11_hrow2","hsec11_hrow2",160,0,160);
  sec12row2 = new TH1I("hsec12_hrow2","hsec12_hrow2",160,0,160);

  sec13row1 = new TH1I("hsec13_hrow1","hsec13_hrow1",160,0,160);
  sec14row1 = new TH1I("hsec14_hrow1","hsec14_hrow1",160,0,160);
  sec15row1 = new TH1I("hsec15_hrow1","hsec15_hrow1",160,0,160);
  sec16row1 = new TH1I("hsec16_hrow1","hsec16_hrow1",160,0,160);
  sec17row1 = new TH1I("hsec17_hrow1","hsec17_hrow1",160,0,160);
  sec18row1 = new TH1I("hsec18_hrow1","hsec18_hrow1",160,0,160);
  sec13row2 = new TH1I("hsec13_hrow2","hsec13_hrow2",160,0,160);
  sec14row2 = new TH1I("hsec14_hrow2","hsec14_hrow2",160,0,160);
  sec15row2 = new TH1I("hsec15_hrow2","hsec15_hrow2",160,0,160);
  sec16row2 = new TH1I("hsec16_hrow2","hsec16_hrow2",160,0,160);
  sec17row2 = new TH1I("hsec17_hrow2","hsec17_hrow2",160,0,160);
  sec18row2 = new TH1I("hsec18_hrow2","hsec18_hrow2",160,0,160);

  sec19row1 = new TH1I("hsec19_hrow1","hsec19_hrow1",160,0,160);
  sec20row1 = new TH1I("hsec20_hrow1","hsec20_hrow1",160,0,160);
  sec21row1 = new TH1I("hsec21_hrow1","hsec21_hrow1",160,0,160);
  sec22row1 = new TH1I("hsec22_hrow1","hsec22_hrow1",160,0,160);
  sec23row1 = new TH1I("hsec23_hrow1","hsec23_hrow1",160,0,160);
  sec24row1 = new TH1I("hsec24_hrow1","hsec24_hrow1",160,0,160);
  sec19row2 = new TH1I("hsec19_hrow2","hsec19_hrow2",160,0,160);
  sec20row2 = new TH1I("hsec20_hrow2","hsec20_hrow2",160,0,160);
  sec21row2 = new TH1I("hsec21_hrow2","hsec21_hrow2",160,0,160);
  sec22row2 = new TH1I("hsec22_hrow2","hsec22_hrow2",160,0,160);
  sec23row2 = new TH1I("hsec23_hrow2","hsec23_hrow2",160,0,160);
  sec24row2 = new TH1I("hsec24_hrow2","hsec24_hrow2",160,0,160);

  sec25row1 = new TH1I("hsec25_hrow1","hsec25_hrow1",160,0,160);
  sec26row1 = new TH1I("hsec26_hrow1","hsec26_hrow1",160,0,160);
  sec27row1 = new TH1I("hsec27_hrow1","hsec27_hrow1",160,0,160);
  sec28row1 = new TH1I("hsec28_hrow1","hsec28_hrow1",160,0,160);
  sec29row1 = new TH1I("hsec29_hrow1","hsec29_hrow1",160,0,160);
  sec30row1 = new TH1I("hsec30_hrow1","hsec30_hrow1",160,0,160);
  sec25row2 = new TH1I("hsec25_hrow2","hsec25_hrow2",160,0,160);
  sec26row2 = new TH1I("hsec26_hrow2","hsec26_hrow2",160,0,160);
  sec27row2 = new TH1I("hsec27_hrow2","hsec27_hrow2",160,0,160);
  sec28row2 = new TH1I("hsec28_hrow2","hsec28_hrow2",160,0,160);
  sec29row2 = new TH1I("hsec29_hrow2","hsec29_hrow2",160,0,160);
  sec30row2 = new TH1I("hsec30_hrow2","hsec30_hrow2",160,0,160);

// Ftpc East
  sec31row1 = new TH1I("hsec31_hrow1","hsec31_hrow1",160,0,160);
  sec32row1 = new TH1I("hsec32_hrow1","hsec32_hrow1",160,0,160);
  sec33row1 = new TH1I("hsec33_hrow1","hsec33_hrow1",160,0,160);
  sec34row1 = new TH1I("hsec34_hrow1","hsec34_hrow1",160,0,160);
  sec35row1 = new TH1I("hsec35_hrow1","hsec35_hrow1",160,0,160);
  sec36row1 = new TH1I("hsec36_hrow1","hsec36_hrow1",160,0,160);
  sec31row2 = new TH1I("hsec31_hrow2","hsec31_hrow2",160,0,160);
  sec32row2 = new TH1I("hsec32_hrow2","hsec32_hrow2",160,0,160);
  sec33row2 = new TH1I("hsec33_hrow2","hsec33_hrow2",160,0,160);
  sec34row2 = new TH1I("hsec34_hrow2","hsec34_hrow2",160,0,160);
  sec35row2 = new TH1I("hsec35_hrow2","hsec35_hrow2",160,0,160);
  sec36row2 = new TH1I("hsec36_hrow2","hsec36_hrow2",160,0,160);

  sec37row1 = new TH1I("hsec37_hrow1","hsec37_hrow1",160,0,160);
  sec38row1 = new TH1I("hsec38_hrow1","hsec38_hrow1",160,0,160);
  sec39row1 = new TH1I("hsec39_hrow1","hsec39_hrow1",160,0,160);
  sec40row1 = new TH1I("hsec40_hrow1","hsec40_hrow1",160,0,160);
  sec41row1 = new TH1I("hsec41_hrow1","hsec41_hrow1",160,0,160);
  sec42row1 = new TH1I("hsec42_hrow1","hsec42_hrow1",160,0,160);
  sec37row2 = new TH1I("hsec37_hrow2","hsec37_hrow2",160,0,160);
  sec38row2 = new TH1I("hsec38_hrow2","hsec38_hrow2",160,0,160);
  sec39row2 = new TH1I("hsec39_hrow2","hsec39_hrow2",160,0,160);
  sec40row2 = new TH1I("hsec40_hrow2","hsec40_hrow2",160,0,160);
  sec41row2 = new TH1I("hsec41_hrow2","hsec41_hrow2",160,0,160);
  sec42row2 = new TH1I("hsec42_hrow2","hsec42_hrow2",160,0,160);

  sec43row1 = new TH1I("hsec43_hrow1","hsec43_hrow1",160,0,160);
  sec44row1 = new TH1I("hsec44_hrow1","hsec44_hrow1",160,0,160);
  sec45row1 = new TH1I("hsec45_hrow1","hsec45_hrow1",160,0,160);
  sec46row1 = new TH1I("hsec46_hrow1","hsec46_hrow1",160,0,160);
  sec47row1 = new TH1I("hsec47_hrow1","hsec47_hrow1",160,0,160);
  sec48row1 = new TH1I("hsec48_hrow1","hsec48_hrow1",160,0,160);
  sec43row2 = new TH1I("hsec43_hrow2","hsec43_hrow2",160,0,160);
  sec44row2 = new TH1I("hsec44_hrow2","hsec44_hrow2",160,0,160);
  sec45row2 = new TH1I("hsec45_hrow2","hsec45_hrow2",160,0,160);
  sec46row2 = new TH1I("hsec46_hrow2","hsec46_hrow2",160,0,160);
  sec47row2 = new TH1I("hsec47_hrow2","hsec47_hrow2",160,0,160);
  sec48row2 = new TH1I("hsec48_hrow2","hsec48_hrow2",160,0,160);

  sec49row1 = new TH1I("hsec49_hrow1","hsec49_hrow1",160,0,160);
  sec50row1 = new TH1I("hsec50_hrow1","hsec50_hrow1",160,0,160);
  sec51row1 = new TH1I("hsec51_hrow1","hsec51_hrow1",160,0,160);
  sec52row1 = new TH1I("hsec52_hrow1","hsec52_hrow1",160,0,160);
  sec53row1 = new TH1I("hsec53_hrow1","hsec53_hrow1",160,0,160);
  sec54row1 = new TH1I("hsec54_hrow1","hsec54_hrow1",160,0,160);
  sec49row2 = new TH1I("hsec49_hrow2","hsec49_hrow2",160,0,160);
  sec50row2 = new TH1I("hsec50_hrow2","hsec50_hrow2",160,0,160);
  sec51row2 = new TH1I("hsec51_hrow2","hsec51_hrow2",160,0,160);
  sec52row2 = new TH1I("hsec52_hrow2","hsec52_hrow2",160,0,160);
  sec53row2 = new TH1I("hsec53_hrow2","hsec53_hrow2",160,0,160);
  sec54row2 = new TH1I("hsec54_hrow2","hsec54_hrow2",160,0,160);

  sec55row1 = new TH1I("hsec55_hrow1","hsec55_hrow1",160,0,160);
  sec56row1 = new TH1I("hsec56_hrow1","hsec56_hrow1",160,0,160);
  sec57row1 = new TH1I("hsec57_hrow1","hsec57_hrow1",160,0,160);
  sec58row1 = new TH1I("hsec58_hrow1","hsec58_hrow1",160,0,160);
  sec59row1 = new TH1I("hsec59_hrow1","hsec59_hrow1",160,0,160);
  sec60row1 = new TH1I("hsec60_hrow1","hsec60_hrow1",160,0,160);
  sec55row2 = new TH1I("hsec55_hrow2","hsec55_hrow2",160,0,160);
  sec56row2 = new TH1I("hsec56_hrow2","hsec56_hrow2",160,0,160);
  sec57row2 = new TH1I("hsec57_hrow2","hsec57_hrow2",160,0,160);
  sec58row2 = new TH1I("hsec58_hrow2","hsec58_hrow2",160,0,160);
  sec59row2 = new TH1I("hsec59_hrow2","hsec59_hrow2",160,0,160);
  sec60row2 = new TH1I("hsec60_hrow2","hsec60_hrow2",160,0,160);


  // input
  cout<<"Histogram the number of good clusters per pad for each sector"<<endl;

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
  //TBranch *bhit=dtree->GetBranch("hit");
  //bhit->SetAddress(&hit);
  TBranch *bcluster=dtree->GetBranch("cluster");
  bcluster->SetAddress(&cluster);
  TBranch *bevent=dtree->GetBranch("event");
  bevent->SetAddress(&event);
  
  bevent->GetEntry(0);
  //cout<<"Event number "<<event.nevent<<endl;
  Int_t CurrentEvent = event.nevent;
  maxentries=bcluster->GetEntries();
  
  for (int i=0;i<=maxentries;i++) {
      bevent->GetEntry(i);
      if (event.nevent != CurrentEvent){
        //cout<<"Event number "<<event.nevent<<endl;
        CurrentEvent = event.nevent;
      }
      bcluster->GetEntry(i);

     // Cut on #pad and #timebins for "good" clusters
     if ((cluster.padlength>=3&&cluster.padlength<=6) &&
         (cluster.timelength>=4&&cluster.timelength<=8)){
         secrow[cluster.sec][cluster.row][cluster.pad]++;
     } // End of cut loop
    } // End of event loop
  }  // End of input file loop

      for (Int_t i=0;i<160;i++) {
      //FTPC West
      sec1row1->SetBinContent(i,secrow[1][1][i]);
      sec2row1->SetBinContent(i,secrow[2][1][i]);
      sec3row1->SetBinContent(i,secrow[3][1][i]);
      sec4row1->SetBinContent(i,secrow[4][1][i]);
      sec5row1->SetBinContent(i,secrow[5][1][i]);
      sec6row1->SetBinContent(i,secrow[6][1][i]);
      sec1row2->SetBinContent(i,secrow[1][2][i]);
      sec2row2->SetBinContent(i,secrow[2][2][i]);
      sec3row2->SetBinContent(i,secrow[3][2][i]);
      sec4row2->SetBinContent(i,secrow[4][2][i]);
      sec5row2->SetBinContent(i,secrow[5][2][i]);
      sec6row2->SetBinContent(i,secrow[6][2][i]);

      sec7row1->SetBinContent(i,secrow[7][1][i]);
      sec8row1->SetBinContent(i,secrow[8][1][i]);
      sec9row1->SetBinContent(i,secrow[9][1][i]);
      sec10row1->SetBinContent(i,secrow[10][1][i]);
      sec11row1->SetBinContent(i,secrow[11][1][i]);
      sec12row1->SetBinContent(i,secrow[12][1][i]);
      sec7row2->SetBinContent(i,secrow[7][2][i]);
      sec8row2->SetBinContent(i,secrow[8][2][i]);
      sec9row2->SetBinContent(i,secrow[8][2][i]);
      sec10row2->SetBinContent(i,secrow[10][2][i]);
      sec11row2->SetBinContent(i,secrow[11][2][i]);
      sec12row2->SetBinContent(i,secrow[12][2][i]);

      sec13row1->SetBinContent(i,secrow[13][1][i]);
      sec14row1->SetBinContent(i,secrow[14][1][i]);
      sec15row1->SetBinContent(i,secrow[15][1][i]);
      sec16row1->SetBinContent(i,secrow[16][1][i]);
      sec17row1->SetBinContent(i,secrow[17][1][i]);
      sec18row1->SetBinContent(i,secrow[18][1][i]);
      sec13row2->SetBinContent(i,secrow[13][2][i]);
      sec14row2->SetBinContent(i,secrow[14][2][i]);
      sec15row2->SetBinContent(i,secrow[15][2][i]);
      sec16row2->SetBinContent(i,secrow[16][2][i]);
      sec17row2->SetBinContent(i,secrow[17][2][i]);
      sec18row2->SetBinContent(i,secrow[18][2][i]);

      sec19row1->SetBinContent(i,secrow[19][1][i]);
      sec20row1->SetBinContent(i,secrow[20][1][i]);
      sec21row1->SetBinContent(i,secrow[21][1][i]);
      sec22row1->SetBinContent(i,secrow[22][1][i]);
      sec23row1->SetBinContent(i,secrow[23][1][i]);
      sec24row1->SetBinContent(i,secrow[24][1][i]);
      sec19row2->SetBinContent(i,secrow[19][2][i]);
      sec20row2->SetBinContent(i,secrow[20][2][i]);
      sec21row2->SetBinContent(i,secrow[21][2][i]);
      sec22row2->SetBinContent(i,secrow[22][2][i]);
      sec23row2->SetBinContent(i,secrow[23][2][i]);
      sec24row2->SetBinContent(i,secrow[24][2][i]);

      sec25row1->SetBinContent(i,secrow[25][1][i]);
      sec26row1->SetBinContent(i,secrow[26][1][i]);
      sec27row1->SetBinContent(i,secrow[27][1][i]);
      sec28row1->SetBinContent(i,secrow[28][1][i]);
      sec29row1->SetBinContent(i,secrow[29][1][i]);
      sec30row1->SetBinContent(i,secrow[30][1][i]);
      sec25row2->SetBinContent(i,secrow[25][2][i]);
      sec26row2->SetBinContent(i,secrow[26][2][i]);
      sec27row2->SetBinContent(i,secrow[27][2][i]);
      sec28row2->SetBinContent(i,secrow[28][2][i]);
      sec29row2->SetBinContent(i,secrow[29][2][i]);
      sec30row2->SetBinContent(i,secrow[30][2][i]);
      //FTPC East
      sec31row1->SetBinContent(i,secrow[31][1][i]);
      sec32row1->SetBinContent(i,secrow[32][1][i]);
      sec33row1->SetBinContent(i,secrow[33][1][i]);
      sec34row1->SetBinContent(i,secrow[34][1][i]);
      sec35row1->SetBinContent(i,secrow[35][1][i]);
      sec36row1->SetBinContent(i,secrow[36][1][i]);
      sec31row2->SetBinContent(i,secrow[31][2][i]);
      sec32row2->SetBinContent(i,secrow[32][2][i]);
      sec33row2->SetBinContent(i,secrow[33][2][i]);
      sec34row2->SetBinContent(i,secrow[34][2][i]);
      sec35row2->SetBinContent(i,secrow[35][2][i]);
      sec36row2->SetBinContent(i,secrow[36][2][i]);

      sec37row1->SetBinContent(i,secrow[37][1][i]);
      sec38row1->SetBinContent(i,secrow[38][1][i]);
      sec39row1->SetBinContent(i,secrow[39][1][i]);
      sec40row1->SetBinContent(i,secrow[40][1][i]);
      sec41row1->SetBinContent(i,secrow[41][1][i]);
      sec42row1->SetBinContent(i,secrow[42][1][i]);
      sec37row2->SetBinContent(i,secrow[37][2][i]);
      sec38row2->SetBinContent(i,secrow[38][2][i]);
      sec39row2->SetBinContent(i,secrow[39][2][i]);
      sec40row2->SetBinContent(i,secrow[40][2][i]);
      sec41row2->SetBinContent(i,secrow[41][2][i]);
      sec42row2->SetBinContent(i,secrow[42][2][i]);

      sec43row1->SetBinContent(i,secrow[43][1][i]);
      sec44row1->SetBinContent(i,secrow[44][1][i]);
      sec45row1->SetBinContent(i,secrow[45][1][i]);
      sec46row1->SetBinContent(i,secrow[46][1][i]);
      sec47row1->SetBinContent(i,secrow[47][1][i]);
      sec48row1->SetBinContent(i,secrow[48][1][i]);
      sec43row2->SetBinContent(i,secrow[43][2][i]);
      sec44row2->SetBinContent(i,secrow[44][2][i]);
      sec45row2->SetBinContent(i,secrow[45][2][i]);
      sec46row2->SetBinContent(i,secrow[46][2][i]);
      sec47row2->SetBinContent(i,secrow[47][2][i]);
      sec48row2->SetBinContent(i,secrow[48][2][i]);

      sec49row1->SetBinContent(i,secrow[49][1][i]);
      sec50row1->SetBinContent(i,secrow[50][1][i]);
      sec51row1->SetBinContent(i,secrow[51][1][i]);
      sec52row1->SetBinContent(i,secrow[52][1][i]);
      sec53row1->SetBinContent(i,secrow[53][1][i]);
      sec54row1->SetBinContent(i,secrow[54][1][i]);
      sec49row2->SetBinContent(i,secrow[49][2][i]);
      sec50row2->SetBinContent(i,secrow[50][2][i]);
      sec51row2->SetBinContent(i,secrow[51][2][i]);
      sec52row2->SetBinContent(i,secrow[52][2][i]);
      sec53row2->SetBinContent(i,secrow[53][2][i]);
      sec54row2->SetBinContent(i,secrow[54][2][i]);

      sec55row1->SetBinContent(i,secrow[55][1][i]);
      sec56row1->SetBinContent(i,secrow[56][1][i]);
      sec57row1->SetBinContent(i,secrow[57][1][i]);
      sec58row1->SetBinContent(i,secrow[58][1][i]);
      sec59row1->SetBinContent(i,secrow[59][1][i]);
      sec60row1->SetBinContent(i,secrow[60][1][i]);
      sec55row2->SetBinContent(i,secrow[55][2][i]);
      sec56row2->SetBinContent(i,secrow[56][2][i]);
      sec57row2->SetBinContent(i,secrow[57][2][i]);
      sec58row2->SetBinContent(i,secrow[58][2][i]);
      sec59row2->SetBinContent(i,secrow[59][2][i]);
      sec60row2->SetBinContent(i,secrow[60][2][i]);
     }  //End of pad loop
  
  TCanvas *c1 = new TCanvas("c1","ps",200,10,700,500);
  c1->Divide(3,2);
  psFile ="plotPadOcc.ps";  
  TPostScript *fps=new TPostScript(psFile,112);

  // PadRow 1
     fps->NewPage();  
       c1->cd(1);
       sec1row1->Draw();
       c1->cd(2);
       sec2row1->Draw();
       c1->cd(3);
       sec3row1->Draw();
       c1->cd(4);
       sec4row1->Draw();
       c1->cd(5);
       sec5row1->Draw();
       c1->cd(6);
       sec6row1->Draw();
     c1->Update();
  // PadRow 2
     fps->NewPage();  
       c1->cd(1);
       sec1row2->Draw();
       c1->cd(2);
       sec2row2->Draw();
       c1->cd(3);
       sec3row2->Draw();
       c1->cd(4);
       sec4row2->Draw();
       c1->cd(5);
       sec5row2->Draw();
       c1->cd(6);
       sec6row2->Draw();
     c1->Update();
  // PadRow 3
     fps->NewPage();  
       c1->cd(1);
       sec7row1->Draw();
       c1->cd(2);
       sec8row1->Draw();
       c1->cd(3);
       sec9row1->Draw();
       c1->cd(4);
       sec10row1->Draw();
       c1->cd(5);
       sec11row1->Draw();
       c1->cd(6);
       sec12row1->Draw();
     c1->Update();
  // PadRow 4
     fps->NewPage();  
       c1->cd(1);
       sec7row2->Draw();
       c1->cd(2);
       sec8row2->Draw();
       c1->cd(3);
       sec9row2->Draw();
       c1->cd(4);
       sec10row2->Draw();
       c1->cd(5);
       sec11row2->Draw();
       c1->cd(6);
       sec12row2->Draw();
     c1->Update();
  // PadRow 5
     fps->NewPage();  
       c1->cd(1);
       sec13row1->Draw();
       c1->cd(2);
       sec14row1->Draw();
       c1->cd(3);
       sec15row1->Draw();
       c1->cd(4);
       sec16row1->Draw();
       c1->cd(5);
       sec17row1->Draw();
       c1->cd(6);
       sec18row1->Draw();
     c1->Update();
  // PadRow 6
     fps->NewPage();  
       c1->cd(1);
       sec13row2->Draw();
       c1->cd(2);
       sec14row2->Draw();
       c1->cd(3);
       sec15row2->Draw();
       c1->cd(4);
       sec16row2->Draw();
       c1->cd(5);
       sec17row2->Draw();
       c1->cd(6);
       sec18row2->Draw();
     c1->Update();
  // PadRow 7
     fps->NewPage();  
       c1->cd(1);
       sec19row1->Draw();
       c1->cd(2);
       sec20row1->Draw();
       c1->cd(3);
       sec21row1->Draw();
       c1->cd(4);
       sec22row1->Draw();
       c1->cd(5);
       sec23row1->Draw();
       c1->cd(6);
       sec24row1->Draw();
     c1->Update();
  // PadRow 8
     fps->NewPage();  
       c1->cd(1);
       sec19row2->Draw();
       c1->cd(2);
       sec20row2->Draw();
       c1->cd(3);
       sec21row2->Draw();
       c1->cd(4);
       sec22row2->Draw();
       c1->cd(5);
       sec23row2->Draw();
       c1->cd(6);
       sec24row2->Draw();
     c1->Update();
  // PadRow 9
     fps->NewPage();  
       c1->cd(1);
       sec25row1->Draw();
       c1->cd(2);
       sec26row1->Draw();
       c1->cd(3);
       sec27row1->Draw();
       c1->cd(4);
       sec28row1->Draw();
       c1->cd(5);
       sec29row1->Draw();
       c1->cd(6);
       sec30row1->Draw();
     c1->Update();
  // PadRow 10
     fps->NewPage();  
       c1->cd(1);
       sec25row2->Draw();
       c1->cd(2);
       sec26row2->Draw();
       c1->cd(3);
       sec27row2->Draw();
       c1->cd(4);
       sec28row2->Draw();
       c1->cd(5);
       sec29row2->Draw();
       c1->cd(6);
       sec30row2->Draw();
     c1->Update();

  // PadRow 11
     fps->NewPage();  
       c1->cd(1);
       sec31row1->Draw();
       c1->cd(2);
       sec32row1->Draw();
       c1->cd(3);
       sec33row1->Draw();
       c1->cd(4);
       sec34row1->Draw();
       c1->cd(5);
       sec35row1->Draw();
       c1->cd(6);
       sec36row1->Draw();
     c1->Update();
  // PadRow 12
     fps->NewPage();  
       c1->cd(1);
       sec31row2->Draw();
       c1->cd(2);
       sec32row2->Draw();
       c1->cd(3);
       sec33row2->Draw();
       c1->cd(4);
       sec34row2->Draw();
       c1->cd(5);
       sec35row2->Draw();
       c1->cd(6);
       sec36row2->Draw();
     c1->Update();
  // PadRow 13
     fps->NewPage();  
       c1->cd(1);
       sec37row1->Draw();
       c1->cd(2);
       sec38row1->Draw();
       c1->cd(3);
       sec39row1->Draw();
       c1->cd(4);
       sec40row1->Draw();
       c1->cd(5);
       sec41row1->Draw();
       c1->cd(6);
       sec42row1->Draw();
     c1->Update();
  // PadRow 14
     fps->NewPage();  
       c1->cd(1);
       sec37row2->Draw();
       c1->cd(2);
       sec38row2->Draw();
       c1->cd(3);
       sec39row2->Draw();
       c1->cd(4);
       sec40row2->Draw();
       c1->cd(5);
       sec41row2->Draw();
       c1->cd(6);
       sec42row2->Draw();
     c1->Update();
  // PadRow 15
     fps->NewPage();  
       c1->cd(1);
       sec43row1->Draw();
       c1->cd(2);
       sec44row1->Draw();
       c1->cd(3);
       sec45row1->Draw();
       c1->cd(4);
       sec46row1->Draw();
       c1->cd(5);
       sec47row1->Draw();
       c1->cd(6);
       sec48row1->Draw();
     c1->Update();
  // PadRow 16
     fps->NewPage();  
       c1->cd(1);
       sec43row2->Draw();
       c1->cd(2);
       sec44row2->Draw();
       c1->cd(3);
       sec45row2->Draw();
       c1->cd(4);
       sec46row2->Draw();
       c1->cd(5);
       sec47row2->Draw();
       c1->cd(6);
       sec48row2->Draw();
     c1->Update();
  // PadRow 17
     fps->NewPage();  
       c1->cd(1);
       sec49row1->Draw();
       c1->cd(2);
       sec50row1->Draw();
       c1->cd(3);
       sec51row1->Draw();
       c1->cd(4);
       sec52row1->Draw();
       c1->cd(5);
       sec53row1->Draw();
       c1->cd(6);
       sec54row1->Draw();
     c1->Update();
  // PadRow 18
     fps->NewPage();  
       c1->cd(1);
       sec49row2->Draw();
       c1->cd(2);
       sec50row2->Draw();
       c1->cd(3);
       sec51row2->Draw();
       c1->cd(4);
       sec52row2->Draw();
       c1->cd(5);
       sec53row2->Draw();
       c1->cd(6);
       sec54row2->Draw();
     c1->Update();
  // PadRow 19
     fps->NewPage();  
       c1->cd(1);
       sec55row1->Draw();
       c1->cd(2);
       sec56row1->Draw();
       c1->cd(3);
       sec57row1->Draw();
       c1->cd(4);
       sec58row1->Draw();
       c1->cd(5);
       sec59row1->Draw();
       c1->cd(6);
       sec60row1->Draw();
     c1->Update();
  // PadRow 20
     fps->NewPage();  
       c1->cd(1);
       sec55row2->Draw();
       c1->cd(2);
       sec56row2->Draw();
       c1->cd(3);
       sec57row2->Draw();
       c1->cd(4);
       sec58row2->Draw();
       c1->cd(5);
       sec59row2->Draw();
       c1->cd(6);
       sec60row2->Draw();
     c1->Update();
  
     fps->Close();
}

