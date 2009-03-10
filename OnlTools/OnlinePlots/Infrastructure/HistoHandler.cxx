//Author: Sergey Panitkin
//Date:   2001,2002,2003

/////////////////////////////////////////////////////////////////
//  HistoHandler for the STAR EVP GUI
/////////////////////////////////////////////////////////////////

#include "HistoHandler.h"

#include "StRoot/StEmcPool/StBEMCPlots/BEMCPlots.h"
#include "StRoot/StEEmcPool/muEztPanitkin/EEMCPlots.h"
//L3 Tracker here
#include "eventTrackerLib.hh"
#ifdef NEW_DAQ_READER
#   include "DAQ_TRG/trgReader.h"
#   include "DAQ_TPC/tpcReader.h"
#   include "DAQ_SVT/svtReader.h"
#   include "DAQ_L3/l3Reader.h"
#   include "DAQ_FTP/ftpReader.h"
#   include "DAQ_TOF/tofReader.h"
#   include "DAQ_PMD/pmdReader.h"
#   include "DAQ_SSD/ssdReader.h"
#   include "DAQ_READER/daqReader.h"
#   include "DAQ_READER/daq_dta.h"
#   include "StEvent/StTriggerData.h"
#   include "TriggerData.h"
#endif


//#include "ServerGui.h"

#include <iostream>
#include <stdlib.h>
#include <cstdlib>
#include <stdio.h>
#include <string.h>

#include <TSystem.h>
#include <TEnv.h>

#include <TStyle.h>
#include <TNamed.h>

#include "EvpUtil.h"



static const int sizL3_max = 1000000;
static L3_P *l3p =(L3_P *)malloc(sizL3_max);
static EventTracker *evtTracker = new EventTracker();


using namespace std;
char*  HistoHandler::mListOfHistograms = EvpUtil::cat(gEnv->GetValue("Online.plotsDir","."),"/local/ListOfHistograms.txt");

char* HistoHandler::mL3Buffer = 0;

HistoHandler::HistoHandler(TMapFile*& file): mFile(file)   {
  if (mDebugLevel) cout << __PRETTY_FUNCTION__ << endl;
  oth = OTH::instance();
  for ( int i=0; i<MAX_NHIST_1; i++) {
    h1[i] = 0;
  }
  mL3Buffer =  (char*)l3p;

  SetPhiAngleMap();
  SetDefaults();
  if(mDebugLevel) {
    cout<<"Instantiating HistoHandler"<<endl;
  }
  
  mLaser = new StReadLaserEvent();
}

HistoHandler::~HistoHandler() {
  if (mDebugLevel) cout << " Deleting " << NHIST_1 << " histograms " << endl;
  for(int i=0; i < NHIST_1; i++) {
    //cout << i << endl;
    delete h1[i];
  } 

  delete mLaser; 
}
//-------------------------------
void HistoHandler::SetDefaults()
{
  //
  // Set Default valuse and create Shared Memory File
  //
  mDebugLevel = 0;

  if(mDebugLevel)
    cout<<"HistoHandler:Setting Defaults..."<<endl;
  // Change default to whatev is needed.
  //Don't forget to put slash at the end.
  // Default name for histogram definition file

  SetHistoListFile(mListOfHistograms);

}

//_____________________________________________________________________
int HistoHandler::ParseString (const TString &inLine, TObjArray &Opt)
{
  Int_t nParsed = 0;
  Ssiz_t begin, index, end, end2;
  begin = index = end = end2 = 0;
  //Comma separated tokens
  TRegexp separator("[^,]+");
  TString Tag, opt, nopt;

  TString line(inLine);

  // search for comment tag
  int comment = line.Index("//");
  if ( comment!=-1 ) {
    //cout << line.Data() << endl;
    line = line(0,comment);
    //cout << line.Data() << endl;
  }

  while ((begin < line.Length()) && (index != kNPOS) ) {
    // loop over given Chain options
    index = line.Index(separator,&end,begin);
    if (index >= 0)         {
      //csp TString substring(line(index,end));
      index = end;
      TString substring(line(begin,end));

      //            if(mDebugLevel)
      //                cout<<"begin:"<<begin<<" end:"<<end<<" index:"<<index<<" T:"<<substring.Data()<<endl;
      Opt.Add(new TObjString(substring.Data()));
      nParsed++;
    }
    begin += end+1;
  }
  return nParsed;
}

//---------------------------------------------
void HistoHandler::Book() {
  if ( mDebugLevel) { cout << __PRETTY_FUNCTION__ << endl; }

  int count=0;
  TObjArray tokens;
  TString buffer;
  unsigned long triggerBits;
  unsigned long detectorBits;

  // Open file for reading
  ifstream fin(mListOfHistograms);
  if (!fin) {
    cout << "Unable to open ***"<< mListOfHistograms <<"*** for reading\n";
    return;
  }

  // Histogram description
  int   dim = 1;       //Histogram dimension e.g. 1 or 2
  TString name;   // Histogram name

  TString title;  // Histogram title

  // x definitions
  int   nch1 =0;             // Number of channels x
  float lch1 =0;             // left edge x
  float rch1 = 0;             // right edge x
  // Same for y
  int   nch2 = 0;
  float lch2 = 0;
  float rch2 = 0;

  int i1 = 0;
  int i2 = 0;

  //
  //Color coding convention for various subsystem
  //
  Color_t tpc_color = 42;
  Color_t ctb_color = 42;
  Color_t zdc_color = 48;
  Color_t mwc_color = 40;
  Color_t ftp_color = 46;
  Color_t l3_color  = 52;
  Color_t svt_color = 49;
  Color_t ssd_color = 49;
  Color_t tof_color = 36;
  Color_t emc_color = 32;
  Color_t bbc_color = 30;
  Color_t fpd_color = 28;
  Color_t pmd_color = 26;


  while(!fin.eof()) {
    buffer.ReadLine(fin);

    count++;
    //
    //Convention here: first symbol "/" means commentary
    //
    if(buffer.BeginsWith("/"))
      continue;
    //if(mDebugLevel) cout <<"line "<<count<<"=>"<<buffer.Data()<<std::endl;
    //
    //Another convention here:
    // All tokens are separated by commas. Hence, no commas in the title!
    //

    int nTokens =0;


    nTokens = ParseString(buffer.Data(),tokens);

    //if(mDebugLevel) cout<<"Tokens: "<<nTokens<<endl;
    //Check if this is an empty string
    if(nTokens == 0)
      continue;


    // Junk String to begin with. Break
    if(nTokens < 6) {
      cout<<"Invalid definition at Line "<<count<<endl;
      cout<<buffer.Data()<<endl;
      exit(-1);
    }
    // Histogram dimensions
    dim  = atoi(((TObjString *)tokens[0])->String().Data());
    //Histogram name
    name =  ((TObjString *)tokens[1])->GetString();
    //histogram title
    title = ((TObjString *)tokens[2])->GetString();
    // booking information for x
    nch1 = atoi(((TObjString *)tokens[3])->String().Data());
    lch1  = atof(((TObjString *)tokens[4])->String().Data());
    rch1  = atof(((TObjString *)tokens[5])->String().Data());
    if (dim==1) {
      if ( nTokens != 8 ) {
	cerr << " wrong amount of tokens for 1D histogram " << endl;
	cout<<buffer.Data()<<endl;
	exit(-1);
	return;
      }  
      //Histogram name
      name =  ((TObjString *)tokens[1])->GetString();
      //histogram title
      title = ((TObjString *)tokens[2])->GetString();
      // booking information for x
      nch1 = atoi(((TObjString *)tokens[3])->String().Data());
      lch1  = atof(((TObjString *)tokens[4])->String().Data());
      rch1  = atof(((TObjString *)tokens[5])->String().Data());
      triggerBits = atoi(((TObjString *)tokens[6])->String().Data());
      detectorBits = atoi(((TObjString *)tokens[7])->String().Data());
      h1[i1] = new TH1F(name,title,nch1,lch1,rch1);
      oth->add(h1[i1],triggerBits,detectorBits);
      // assign subsystem specific colors
      if(name.Contains("tpc"))
	h1[i1]->SetFillColor(tpc_color);
      else if(name.Contains("ctb"))
	h1[i1]->SetFillColor(ctb_color);
      else if(name.Contains("zdc"))
	h1[i1]->SetFillColor(zdc_color);
      else if(name.Contains("mwc"))
	h1[i1]->SetFillColor(mwc_color);
      else if(name.Contains("ftp"))
	h1[i1]->SetFillColor(ftp_color);
      else if(name.Contains("l3"))
	h1[i1]->SetFillColor(l3_color);
      else if(name.Contains("svt"))
	h1[i1]->SetFillColor(svt_color);
      else if(name.Contains("ssd"))
	h1[i1]->SetFillColor(ssd_color);
      else if(name.Contains("tof"))
	h1[i1]->SetFillColor(tof_color);
      else if(name.Contains("emc"))
	h1[i1]->SetFillColor(emc_color);
      else if(name.Contains("bsmd"))
	h1[i1]->SetFillColor(emc_color);
      else if(name.Contains("bbc"))
	h1[i1]->SetFillColor(bbc_color);
      else if(name.Contains("fpd"))
	h1[i1]->SetFillColor(fpd_color);
      else if(name.Contains("pmd"))
	h1[i1]->SetFillColor(pmd_color);
      i1++;
      //	    cout << " press any key to continue ..." << endl;
      //	    cin.ignore();
      //if(mDebugLevel) cout<<dim<<","<<nch1<<","<<lch1<<","<<rch1<<endl;
    } 
    if (dim==2) {
      if ( nTokens != 11 ) {
	cerr << " wrong amount of tokens for 2D histogram " << endl;
	cout<<buffer.Data()<<endl;
	exit(-1);
      }  
      //Histogram name
      name =  ((TObjString *)tokens[1])->GetString();
      //histogram title
      title = ((TObjString *)tokens[2])->GetString();
      // booking information for x
      nch1 = atoi(((TObjString *)tokens[3])->String().Data());
      lch1  = atof(((TObjString *)tokens[4])->String().Data());
      rch1  = atof(((TObjString *)tokens[5])->String().Data());
      nch2 = atoi(((TObjString *)tokens[6])->String().Data());
      lch2  = atof(((TObjString *)tokens[7])->String().Data());
      rch2  = atof(((TObjString *)tokens[8])->String().Data());
      triggerBits = atoi(((TObjString *)tokens[9])->String().Data());
      detectorBits = atoi(((TObjString *)tokens[10])->String().Data());
      //    cout<<" "<<","<<nch2<<","<<lch2<<","<<rch2<<endl;
      h1[i1] = new TH2F(name,title,nch1,lch1,rch1,nch2,lch2,rch2);
      oth->add(h1[i1],triggerBits,detectorBits);
      i1++; // increment histogram counter
      i2++;  // this one is just for bookkeeping of 2d histoes only
      //cout << " " << i1;
    }
    
    //Prepare array for the next string
    tokens.Delete();
  }



  //==============================
  //      EMC histograms
  //==============================

  EEMCPlots::initHisto(0, gEnv->GetValue("Online.eemcDbDump", "eemcDbDump.dat"), gEnv->GetValue("Online.eemcPathIn", "."), gEnv->GetValue("Online.eemcPathOut", "."));
  //EEMCPlots::initHisto(0, "/star/u/rfatemi/TestPPlots/home_local/eemc/defaultPanitkinSetup/eemcDbDump.dat", gEnv->GetValue("Online.eemcPathIn", "."), gEnv->GetValue("Online.eemcPathOut", "."));
  BEMCPlots::initHisto(0, gEnv->GetValue("Online.bemcStatus", "bemcStatus.txt"));
  //BEMCPlots::initHisto(0,"/star/u/rfatemi/TestPPlots/home_local/bemc/bemcStatus.txt");
   
  // EMC histograms end

  NHIST_1 = i1; // Set Current Number of 1d Histogram

  if ( mDebugLevel) {
    cout<<"Number of Lines: "<<count<<endl;
    cout<<"Total number of histograms: "<<NHIST_1<<endl;
    cout<<"Number of 1d hists: "<<i1-i2<<endl;
    cout<<"Number of 2d hists: "<<i2<<endl;
  }
  // Don't forget to close input stream
  fin.close();

  h1[0]->SetFillColor(48);

  return;
}

//-------------------------------------------------------------------------

void HistoHandler::Reset() {
  if ( mDebugLevel) { 
    cout << __PRETTY_FUNCTION__ << endl;
  }
  //cout << " Resetting " << NHIST_1 << " histograms " << endl;
  for(int i=0; i < NHIST_1; i++) {
    if (h1[i] != 0) {
      h1[i]->Reset(); // reset 1d histoes
    } else {
      cout << "histogram #"<< i << " does not exist " << endl; 
    }
  }
  
  //==============================
  //      EMC histograms
  //==============================
  EEMCPlots::resetHisto();
  BEMCPlots::resetHisto(gEnv->GetValue("Online.bemcStatus", "bemcStatus.txt"));
  //BEMCPlots::resetHisto("/star/u/rfatemi/TestPPlots/home_local/bemc/bemcStatus.txt");

  if(mDebugLevel) {
    cout<<"HistoHandler: Exiting Reset..."<<endl;
  }

  // Reset the laser
  mLaser->resetAll();
}
//--------------------------------------------------------------------
int  HistoHandler::Save(const char* filename) {
  // Save Histogras in a Root File
  if(mDebugLevel) {
    cout<<"HistoHandler: Save as Root File:"<<filename<<endl;
  }
  
  TFile hfile(filename,"recreate","STAR EVP ROOT file with histograms"); 
  if ( hfile.IsWritable() == 0 ) {
    cerr << "#error ###  Can not write to file : " << filename << endl;
    return -1;
  }
  
  for(int i=0; i < NHIST_1; i++)  {
    h1[i]->Write();
  }

  //==============================
  //      EMC histograms
  //==============================
  EEMCPlots::saveHisto(&hfile);
  BEMCPlots::saveHisto(&hfile);

  hfile.Close();
  return 0;
}

//----------------------------------------------------------------------
void HistoHandler::CopyMapFile() {
  // Copy MaP file
  // In order to switch to the next run without waiting for histogram Presenter (test_3)
  // to finish creating pdf file and upload to database, create a copy and keep going with hsimple map
  if(mDebugLevel) {
    cout<<"HistoHandler: Copying mapfile"<<endl;
  }
  TString Message = "Write PDF to Data Base Error: ";
  

  TString Command = "cp hsimple.map hcopy.map";
  cout<<"Command:"<<Command.Data()<<endl;

  int i = 999;// initialize return code to some crazy number
  //csp commented out for test 10/04/04
  //i = gSystem->Exec(Command);

  cout <<"i ="<<i<<endl;

  //Codes used by Jeff as of 02/05/03
  //  0 => Success
  //  1 => DB Connection failed
  //  2 => Incomplete arguments
  //  3 => file specified not found or empty
  //  4 => Write to DB Failed

  // Succesful comletion
  //   if(i == 0){
  //     new DGHelp("/home_local/panitkin/online/messages/db_ok.message");
  //     file_copied = 1;
  //   }
  //   else{
  //     sprintf(return_code,"%d",i);
  //     TString code = return_code;
  //     Message.Append(code);
  //     Message.Append(". email porter@bnl.gov !");
  //     new PGMessage(Message.Data());
  //     //new DGHelp("/home/panitkin/online/messages/db_error.message");
  //     file_copied = -1; // error flag for upload
  //  }

}
//----------------------------------------------------------------------
void HistoHandler::MakePS()
{
  // Save all Histoes in a  Postscript File
  if(mDebugLevel)
    cout<<"HistoHandler: Save as PS file"<<endl;
}
//----------------------------------------------------------------------
void HistoHandler::Print()
{
  // Print All Histogarms
  if(mDebugLevel)
    cout<<"HistoHandler: Printing PS"<<endl;
  MakePS();
}
//---------------------------------------------------------------------
void  HistoHandler::SetPhiAngleMap(){

  const Int_t NP = 45; // # padrows
  const Float_t DEG = 57.296;//360/2pi

  Double_t Xpads[NP] = { 
    60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 
    93.6, 98.8,104.0,109.2,114.4,119.6, // inner Centres
    127.195, 129.195, 131.195, 133.195, 135.195, //Outer
    137.195, 139.195, 141.195, 143.195, 145.195,
    147.195, 149.195, 151.195, 153.195, 155.195,
    157.195, 159.195, 161.195, 163.195, 165.195,
    167.195, 169.195, 171.195, 173.195, 175.195,
    177.195, 179.195, 181.195, 183.195, 185.195,
    187.195, 189.195};   


  Int_t Npads[NP] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182, 
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122, 
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};

  Double_t pitches[2] = {0.335, 0.67};


  //Note from GENE
  //So within any supersector, I have a local X and Y.  X you can get from
  //Xpads above, below I have YMIN (the lower Y coordinate of each pad;
  //you would need to add half the "pitch" to get the Y center of each pad)

  float YMIN;
  float pitch;
  float phi0=60;
  float LPhi;//local phi
  float SPhi;//sector phi
  float GPhi;//global phi


  mDebugLevel = 0;

  //loop over sectors and find SPhi= phi in middle of sector
  for (int sec = 0; sec < 24; sec++) {
    if (sec<12) {
      SPhi = phi0 - (sec*30);
      if (SPhi<-180) SPhi+=360;
    }
    if (sec>=12) {
      SPhi = phi0 + ((sec-10)*30);
      if (SPhi>180) SPhi-=360;
    } 
    if(mDebugLevel) cout<<"sec = "<<sec<<" SPhi ="<<SPhi<<endl;
    //loop over each padrow in a sector
    for (int j=0; j<45; j++) {
      if (j >= 13) pitch = pitches[1];
      else pitch = pitches[0];
      for (int k=0; k<Npads[j]; k++) {//loop over # pads in each padrow
	YMIN = pitch * (k - 0.5*Npads[j]);//find Y at bottom of padrow
	LPhi=atan(YMIN/Xpads[j]);//find local Phi (LPhi) within sector
	LPhi*=DEG;
	GPhi=LPhi+SPhi;//find global Phi (GPhi) 

	//oth->fill( 	 h1,LPhi);
	//oth->fill(          h2,Xpads[j],YMIN);
	//oth->fill( 	 h3,Xpads[j],LPhi);
	//oth->fill( 	 h4,YMIN,LPhi);
	//oth->fill( 	 h5,GPhi);
	//oth->fill( 	 h6,sec,SPhi);

	//Fill Look up table for pad phi angle

	mPhiAngleMap[sec][j][k]=GPhi;

	if (mDebugLevel>10) cout<<"sec = "<<sec<<" Row = "<<j<<" pad#= "<<k<<" X = "<<Xpads[j]<<" YMIN= "<<YMIN<<"SPhi = "<<SPhi<<" LPhi= "<<LPhi<<"GPhi= "<<GPhi<<endl;
      }//pad
    }//padrow
  }// sector
   

  mDebugLevel = 0;
}

void HistoHandler::SetHistoListFile(char *lHistoListFile){ 
  char* txt = new char[1024];
  sprintf(txt,"%s",lHistoListFile);
  mListOfHistograms = txt;
  if ( mDebugLevel) cout << mListOfHistograms << endl;
}



#include "StReadLaserEvent.h"

#include "SsdAdcLogTable.h"               // Bo from Renaud 03/03



// Jing Liu, for TOF  12/10/2007 ---
#include "tofr.h"

int mDebugLevel = 0;


////////////////////////////////////////////SSD
// SSD Temporary Module and Strip Conversion
void SsdTabToStripConversion();
int  SsdStripNumber[768] ;
// Mapping : array index=DAQ ladder, value=ladder geom position
// Mapping for:         run V 2004-2005
/*int  SsdDaqToRealLadder[40] = {10,  8,  6,  9,  7,
			       5,  3,  1,  4,  2, 
			       11, 13, 15, 12, 14,
			       16, 18, 20, 17, 19,
			       30, 28, 26, 29, 27,
			       25, 23, 21, 24, 22,
			       31, 33, 35, 32, 34,
			       36, 38, 40, 37, 39};*/

//correct mapping for year 2007
int   SsdDaqToRealLadder[40] = {11, 13, 15, 12, 14,
                               16, 18, 20, 17, 19,
                               10,  8,  6,  9,  7,
                               5,  3,  1,  4,  2,
                               30, 28, 26, 29, 27,
                               25, 23, 21, 24, 22,
                               31, 33, 35, 32, 34,
                               36, 38, 40, 37, 39};

///////////////////////////////////////////////

static const int nMaxTrgWd=10;
int modtrgwd[nMaxTrgWd] = {0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA};

//PMD Related constants
static const int Crate = 2;			// 2 Crates max
static const int CRAM=12; 			// Max no of Crams
static const int BLOCK=2; 			// Max no of blocks in each cram
static const int CHANNEL=1728; 		//Max no of channels in eack block
static const int pmd_hist_begin=381;  // PMD hist starts after 381 no.

//-----------------------------------
//Buffer for event storage


int HistoHandler::fill(evpReader* evp, char* mem, float mPhiAngleMap[24][45][182]) {

#include "zdc_smd.h"

  //printf("Histohandler fill\n");

#ifndef NEW_DAQ_READER
  char *datap = mem;
#else
  char *datap = (char *)evp;
#endif

  if (!evp) {
    printf("NO evpReader!!!\n");
    return 0;
  }
  
  static long lastRun = 0;
  int trgcmd;
  int trgword;
  int daqbits;
  //

  int ret = 0 ;
  int iret = 0;
  static int good = 0;
  
  ////////////////////////////////////////////SSD
  int ssd_good = 0;
  SsdTabToStripConversion();
  ///////////////////////////////////////////////
  
  // Buffer sizes for subsystems
  
  float total_size, tpc_size, svt_size, ssd_size, ftp_size;
  float bemc_size, l3_size, tof_size;
  
  total_size = tpc_size  = ftp_size = 0.;
  svt_size   = ssd_size  = tof_size = 0.;
  l3_size    = bemc_size = 0.;
  
  float tpc_fract,  ftp_fract;
  float svt_fract,  ssd_fract, tof_fract;
  float bemc_fract, l3_fract;
  
  tpc_fract  = ftp_fract = 0.;
  svt_fract  = ssd_fract = tof_fract = 0.;
  bemc_fract = l3_fract  = 0.;
  
  u_int ctbch, zdcch;
  // u_int pre_ctbch;
  
  ctbch  = zdcch   = 0;
  u_int zdcch_east, zdcch_west;
  zdcch_east = zdcch_west = 0;
  u_int zdcTime_east, zdcTime_west;
  zdcTime_east = zdcTime_west = 0;
  //int bbctdiff = -999; // strange number from Akio
  
  //time stamp of the first recieved event in unix seconds
  static u_int t_00;
  static u_int t_01;
  static u_int t_02 ;
  //t_00 = t_01 = t_02 = 0;
  
  unsigned int d_t_00, d_t_01=0,d_t_02;
  
  //float tofadc[120];	//WJL... TOF: needed for adc mapping for hit patterns
  
  double tpc_channels = 0; // sum for calculation of tpc occupancy
  double tpc_max_channels = 0; // sum for maximum occupacy
  double tpc_occ = 0;
  float  adc = 0; // adc sum for the whole tpc
  float  pad_adc = 0; // adc sum per pad
  
  //	delta = time(NULL) ;
  
  ////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////
  
  // Check whether we have a new run
  if ( lastRun != (long) evp->run ) {
    Reset();
    cout<< "Start time "<<evp->evt_time<<endl;
    t_00 = evp->evt_time;
    t_01 = t_00;
    t_02 = t_00;
    lastRun = evp->run;
  }
  good++ ;
  // Total event size in bytes
  total_size = (float) evp->bytes;
  
  oth->fill(  h1[0],(total_size>0? log10(total_size) :0));
  
  
  // Get current trigger command
  trgcmd = evp->trgcmd;
  
  // Get current trigger word
  trgword = evp->trgword;

  // Get current daq bits from L3_summary[0]
  daqbits = evp->daqbits;

  
  printf("**** Event %d: bytes %d, token %d, trg_cmd %d, FILE %s\n",evp->event_number,evp->bytes,evp->token,evp->trgcmd, evp->file_name) ;
  fflush(stdout);

  //  		if(good % 10 == 0) {
  //  			delta = time(NULL) - delta ;
  //  			printf("Got %d good events, %d bad events in %d seconds\n",good,bad,delta) ;
  //  			delta = time(NULL) ;
  //  		}

  //csp		datap = datapReader(mem) ;	// always present

  //datap = mem;

  
  if(datap == NULL)
    {	// huge error
      printf("datap is null\n");
    if (mDebugLevel) fprintf(stderr, "\nProblems with DATAP - ignoring this event!") ;
      //			continue ;
      goto END;
    }

  daqReader* daqr = (daqReader*)datap;

  //Get event's time stamp
  u_int event_time = daqr->evt_time;
  
  d_t_00 = event_time - t_00;
  // 2 hour histograms
  if(d_t_00 > 120){
    t_00 = event_time;
    h1[155]->Reset();	
  }
  d_t_01 = event_time - t_01;
  // 10 min histogram
  if(d_t_01 > 600){
    t_01 = event_time;
    h1[156]->Reset();
    h1[337]->Reset();     //JCS
  }
  // 60 Second histograms
  d_t_02 = event_time - t_02;  if(d_t_02 > 7200){
    t_02 = event_time;
    h1[157]->Reset();
  }
  oth->fill(	  h1[155],float(d_t_00),(total_size>0? log10(float(total_size)) :0));
  oth->fill(	  h1[156],float(d_t_01),(total_size>0? log10(float(total_size)) :0));
  oth->fill(	  h1[157],float(d_t_02),(total_size>0? log10(float(total_size)) :0));
  cout<<"T="<<d_t_01<<"  Size="<< total_size<<endl;


  //-----------------------------------------------------------------------
  // TRIGGER!
  StTriggerData* trgd = TriggerData::Instance(datap);
  if(trgd){
    //Bunch Counters
    unsigned int bunch7bit = trgd->bunchId7Bit();
    oth->fill(h1[266],bunch7bit);
    
    //Spin Bits    
    int ispinb = trgd->spinBit();
    if(ispinb &   1) oth->fill(h1[442],bunch7bit);  
    if(ispinb &   2) oth->fill(h1[443],bunch7bit);  
    if(ispinb &   4) oth->fill(h1[444],bunch7bit);  
    if(ispinb &   8) oth->fill(h1[445],bunch7bit);  
    if(ispinb &  16) oth->fill(h1[446],bunch7bit);  
    if(ispinb &  32) oth->fill(h1[447],bunch7bit);  
    if(ispinb &  64) oth->fill(h1[448],bunch7bit);  
    if(ispinb & 128) oth->fill(h1[449],bunch7bit);  
    
    //ZDC
    mZdcTimeDiff = -9999;
    mZdcVertex   = -9999;
    int te = trgd->zdcPmtTDC(east,1);
    int tw = trgd->zdcPmtTDC(west,1);
    if(te>20 && te<4000) oth->fill(h1[76], float(te));    
    if(tw>20 && tw<4000) oth->fill(h1[77], float(tw));    
    if(te>20 && te<4000 && tw>20 && tw<4000){
      mZdcTimeDiff = float(tw-te);
      mZdcVertex   = mZdcTimeDiff/2*40.0/0.03;
      oth->fill(h1[78], mZdcTimeDiff);          
      oth->fill(h1[146],mZdcVertex);
    }          
    oth->fill(h1[474],float(trgd->zdcADC(east,1)));
    oth->fill(h1[475],float(trgd->zdcADC(west,1)));
    oth->fill(h1[476],float(trgd->zdcADC(east,2)));
    oth->fill(h1[477],float(trgd->zdcADC(west,2)));
    oth->fill(h1[478],float(trgd->zdcADC(east,3)));
    oth->fill(h1[479],float(trgd->zdcADC(west,3)));
    oth->fill(h1[480],float(trgd->zdcUnAttenuated(east)));
    oth->fill(h1[481],float(trgd->zdcUnAttenuated(west)));

    //BBC
    mBbcTimeDiff = -9999;
    mBbcVertex   = -9999;
    int nhit[2]={0,0},nhitl[2]={0,0};
    for(int i=0; i<2; i++){
      for(int j=1; j<=24; j++){
	float adc = (float)trgd->bbcADC((StBeamDirection)i,j);
	if(adc>5) {
	  if(j<=16) nhit[i]++;
	  else      nhitl[i]++;
	  oth->fill(h1[190+i],float(j));
	  oth->fill(h1[192+i],float(j),adc);
	}
      }
      for(int j=1; j<=16; j++){
	float tac=(float)trgd->bbcTDC((StBeamDirection)i,j);
	oth->fill(h1[157+i*16+j],tac);
      }
    }
    oth->fill(	  h1[194],float(nhit[0]));
    oth->fill(	  h1[195],float(nhit[1]));
    oth->fill(	  h1[196],float(nhitl[0]));
    oth->fill(	  h1[197],float(nhitl[1]));
    oth->fill(	  h1[198],float(trgd->bbcADCSum(east)));
    oth->fill(	  h1[199],float(trgd->bbcADCSum(west)));
    oth->fill(	  h1[200],float(trgd->bbcADCSumLargeTile(east)));
    oth->fill(	  h1[201],float(trgd->bbcADCSumLargeTile(west)));
    oth->fill(	  h1[202],float(trgd->bbcEarliestTDC(east)));
    oth->fill(	  h1[203],float(trgd->bbcEarliestTDC(west)));
    oth->fill(	  h1[205],float(trgd->bbcEarliestTDC(east)),float(trgd->bbcEarliestTDC(west)));
    if( trgd->bbcEarliestTDC(east)>10 && trgd->bbcEarliestTDC(east)<3000 &&
	trgd->bbcEarliestTDC(west)>10 && trgd->bbcEarliestTDC(west)<3000 ) {
      mBbcTimeDiff = trgd->bbcTimeDifference()-4096;
      mBbcVertex   = mBbcTimeDiff/3/10.0;      
      oth->fill(h1[204],mBbcTimeDiff);
      oth->fill(h1[452],mBbcVertex);
    }
    
    //ZDCSMD
    for(int i=0; i<2; i++){
      for(int j=0; j<2; j++){
	for(int k=1; k<=8; k++){
	  int adc = trgd->zdcSMD((StBeamDirection)i,j,k);
	  if(adc>0){
	    oth->fill(h1[331 +j -i*2],float(k));	    
	    oth->fill(h1[335 +j -i*2],float(k),float(adc));
	  }
	}
      }
    }

  }//end of good triggers
    
  //-----------------------------------------------------------------------
  // TPC!
  int sec;


  //  		tpc_channels = 0; // sum for calculation of tpc occupancy
  //  		tpc_max_channels = 0; // sum for maximum occupacy
  //  		tpc_occ = 0;
  //  		adc = 0;


  // laser event
  printf("fill TPC HistoHandler.cxx 1402\n");
  if (trgcmd == 9) {
    float vDrift = mLaser->Make(int(evp->run), int(evp->event_number), datap);
    if(vDrift == 1972.){ mLaser->resetAll();}//try twice, bad value triggered by laser->Make

    //if(mDebugLevel)printf("drift velocity: %2.4f\n", vDrift);
    //    if(mDebugLevel)
    //printf("EventLopper::vDrift = %10.3f\n  run=%d\n", vDrift,evp->run);
    //fflush(stdout);

    if (vDrift > 0 && vDrift <999 ){ 
      h1[113]->Add( mLaser->driftVelocityDistribution() );
      cout << __PRETTY_FUNCTION__ << " " << mLaser->driftVelocityDistribution()->GetEntries() << endl;;
      //oth->fill(h1[113],vDrift); 
      oth->fill(h1[102],vDrift); 
    }
  } 
  
  for(sec=0;sec<24;sec++) {

    int secnum = sec+1;
    //printf("Filling laser:  Sector %d\n",sec);
    //fflush(stdout);


#ifndef NEW_DAQ_READER
    //ret = EVP_NO_DET;
    switch(ret) {
    case EVP_NO_DET :
      if(mDebugLevel)
	fprintf(stderr,"TPC: not present...");
      break ;
    case EVP_NO_DATA :
      if(mDebugLevel)
	fprintf(stderr,"TPC: Sector %d not present...",secnum);
      break ;
    case EVP_DATA_ERR :
      if(mDebugLevel)
	fprintf(stderr, "TPC: Sector %d: problems with data [%d], token %d, EVP num %d, EVB seq %d - continuing...",secnum,ret,evp->token,evp->event_number,evp->seq) ;
      break ;
    default :
      if(mDebugLevel)
	fprintf(stderr,"TPC: Sector %d: %d bytes",secnum,ret) ;
      break ;
    }
    
    //printf("Ret---- %d\n",ret);
    //fflush(stdout);

    if(ret == EVP_NO_DET)
      break ;
  
      
    if(ret < 0)
      continue ;	// try another sector
#else
    if(!tpcReader(datap,secnum)) continue;
#endif
    
    
    
    // example usage: calculate total charge and
    // print occupancy
    int r,p,t ;
    float adc_sector ;
    //uchar_t val ;
    unsigned char val;
    float tpc_sector_size = 0;
    
    // Watch out here! The position depends on the
    // histogram description file.
    // if description file is changed then change
    // starting index accordingly
    
    int hist_index = sec + 15;
    int nhist = sec + 120;
    
    tpc_sector_size = float(ret); //data size for the sector
    tpc_size += tpc_sector_size; // sum for all valid sectors
    
    adc_sector = 0 ; // adc sum for the sector
    //PR(tpc.mode);
    if(tpc.mode==0) {	// normal event
      //PR(trgcmd);

      //printf("About to loop over adcs\n");
      //fflush(stdout);

      for(r=0;r<45;r++) {	// padrow
	for(p=0;p<182;p++) {	// pad
	  pad_adc =0; // adc sum per pad
	  for(t=0;t<tpc.counts[r][p];t++) {
	    val = tpc.adc[r][p][t] ;
	    pad_adc += (float)val;
	    
	    //if(trgcmd != 4) { // 4 means physics    // FL, commented out on 5/30/2007 as requested by Blair
	    // i.e. timebin for that channel would be
	    int timebin = tpc.timebin[r][p][t];
	    //PR(val);
	    //PR(timebin);
	      oth->fill(h1[nhist],timebin,val);
	    //}
	  }//end pad time sequence
	  TH2 *hh =  (TH2 *)h1[hist_index];
	  oth->fill(		  hh,(float)p, (float)r, pad_adc);
	  
	  adc_sector += pad_adc;// adc sum per sector
	  
	  
	  //Pad histograms for LASER triggers goes here
	  // West Side
	  if(trgcmd == 9 && sec == 1) {
	    TH2 *hh =  (TH2 *)h1[94];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  if(trgcmd == 9 && sec == 6) {
	    TH2 *hh =  (TH2 *)h1[95];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  if(trgcmd == 9 && sec == 11) {
	    TH2 *hh =  (TH2 *)h1[96];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  // East Side
	  if(trgcmd == 9 && sec == 13) {
	    TH2 *hh =  (TH2 *)h1[97];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  if(trgcmd == 9 && sec == 19) {
	    TH2 *hh =  (TH2 *)h1[98];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  if(trgcmd == 9 && sec == 23) {
	    TH2 *hh =  (TH2 *)h1[99];
	    oth->fill(		      hh,(float)p, (float)r, pad_adc);
	  }
	  
	  oth->fill(		  h1[66],mPhiAngleMap[sec][r][p],pad_adc);
	  oth->fill(		  h1[67],float(secnum),pad_adc);
	  
	}// end of pad loop
      }// end padrow
      //fflush(stderr);
      //fflush(stdout);
      //printf("looped over adcs\n");
      //fflush(stdout);

      if(mDebugLevel) {
	fprintf(stderr,"TPC: Sector %d: occupancy %3d %%, charge %d",secnum,(tpc.max_channels_sector!=0? (int)(100.0 *((double)tpc.channels_sector/(double)tpc.max_channels_sector)) :0),(int)adc_sector);
      }
      adc += adc_sector;
      
      tpc_channels += (double)tpc.channels_sector;
      //Aren't all sectors the same?
      tpc_max_channels += (double)tpc.max_channels_sector;


      //printf("Ok what's up here\n");
      //fflush(stdout);

    } else {	// special mode - currently just for pedestals
      // special mode has different packing i.e. it
      // overrides the meaning of the structure
      
      // tpc.adc contains the pedestals
      // i.e. the pedestal of sector, row 12, pad 3, timebin 0 is
      // ped = tpc.adc[11][2][0]
      // note that the index goes from 0 not 1 !
      
      // tpc.timebins contains the RMS's shifted 4 bits
      // i.e. for the RMS of the above
      // rms = (double)tpc.timebins[11][2][0] / 16.0

      if(mDebugLevel)
	fprintf(stderr,"TPC: Sector %d is mode 1 (non-zero suppressed)",secnum);
    }
    
  } // end of sector loop for the TPC


  //  		cout<<"h0: "<<h1[0]->GetEntries()<<endl;
  //  		cout<<"h1: "<<h1[1]->GetEntries()<<endl;
  //  		cout<<"h2: "<<h1[2]->GetEntries()<<endl;
  //  		cout<<"h3: "<<h1[3]->GetEntries()<<endl;
  //  		cout<<"h43: "<<h1[43]->GetEntries()<<endl;
  //  		cout<<"h44: "<<h1[44]->GetEntries()<<endl;
  //  		cout<<"h101: "<<h1[101]->GetEntries()<<endl;
  //  		cout<<"h245: "<<h1[245]->GetEntries()<<endl;

  oth->fill(  h1[1],(adc>0? log10(adc) :0));

  oth->fill(  h1[2],(tpc_size>0? log10(tpc_size) :0));
  if(total_size != 0.) {
    tpc_fract = 100.*tpc_size/total_size;
    oth->fill(      h1[103],tpc_fract);
  }
  if(tpc_max_channels != 0.) {
    tpc_occ = 100.0 *(tpc_channels/tpc_max_channels);
    // Physics triggers goes here
    if(trgcmd != 8 && trgcmd != 9 && trgcmd !=10){
      oth->fill(	  h1[3],tpc_occ);
    }
    // Occupancy for Pulser triggers goes here
    if(trgcmd == 8 && trgcmd ==10) {
      oth->fill(	  h1[43],tpc_occ);
    }
    //Occupancy for Laser triggers goes here
    if(trgcmd == 9){
      //printf("filling oth\n");
      //fflush(stdout);
      oth->fill(	  h1[44],tpc_occ);
      //printf("filled oth\n");
      //fflush(stdout);
    }
  }
  if(mDebugLevel) {
    fprintf(stderr,"TPC: occupancy %3d %%, charge %d",(int)tpc_occ,(int)adc) ;
  }
  //-----------------------------------------------------------------------
  // Below is code for the old TPC reader
  // Left for reference SP Nov 2002

  //  		ret = tpcReader(datap) ;
  //  		if(ret <= 0) {
  //  		  if(ret < 0) {
  //  		    fprintf(stderr,"TPC: problems with data (%d) - continuing...",ret) ;
  //  		  }
  //  		  else{
  //  		    fprintf(stderr,"TPC: not present...") ;
  //  		  }
  //                  }
  //  		else{
  //  		  // example usage: calculate total charge and
  //  		  // print occupancy
  //  		  int s,r,p,t ;

  //  		  uint_t adc ;
  //  		  uchar_t val ;

  //  		  if(mDebugLevel)fprintf(stderr,"TPC: %d bytes",ret);

  //  		  tpc_size = float(ret);
  //oth->fill(    		  h1[2],log10(tpc_size));

  //  		  if(total_size != 0.){
  //  		    tpc_fract = 100.* tpc_size/total_size;
  //oth->fill(    		    h1[103],tpc_fract);
  //  		  }
  //  		  adc = 0 ; // Total Charge Counter
  //  		  float dice = fabs(rand())/32767.;
  //  		  if(ret != 0 && dice < 0.1) {
  //  		    for(s=0;s<24;s++) {	// sector
  //  		      int hist_index = s + 15;
  //  		      int nhist = s + 120;

  //  		      for(r=0;r<45;r++) {	// padrow
  //  			for(p=0;p<182;p++) {	// pad

  //  			  int pad_adc = 0; // Per pad counter
  //  			  for(t=0;t<tpc.counts[s][r][p];t++) {
  //  			    val = tpc.adc[s][r][p][t] ;
  //                              pad_adc += val;
  //  			    if (trgcmd == 4) {
  //  			      int timebin = tpc.timebin[s][r][p][t];
  //  			      if (0 < val && val < 1024) {
  //oth->fill(    				h1[nhist],timebin,val);
  //  			      }
  //  			    }
  //  			  }
  //  			  adc += pad_adc ; // Total charge counter

  //  			  //Watch out here! The position depends on the
  //  			  // histogram description file.
  //  			  // If description file is change then change
  //  			  // it appropriately


  //  			  TH2 *hh =  (TH2 *)h1[hist_index];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);

  //  			  //Pad histograms for LASER triggers goes here
  //  			  // West Side
  //  			  if(trgcmd == 9 && s == 1){
  //  			  TH2 *hh =  (TH2 *)h1[94];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }
  //  			  if(trgcmd == 9 && s == 6){
  //  			  TH2 *hh =  (TH2 *)h1[95];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }
  //  			  if(trgcmd == 9 && s == 11){
  //  			  TH2 *hh =  (TH2 *)h1[96];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }
  //                            // East Side
  //  			  if(trgcmd == 9 && s == 13){
  //  			  TH2 *hh =  (TH2 *)h1[97];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }
  //  			  if(trgcmd == 9 && s == 19){
  //  			  TH2 *hh =  (TH2 *)h1[98];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }
  //  			  if(trgcmd == 9 && s == 23){
  //  			  TH2 *hh =  (TH2 *)h1[99];
  //oth->fill(    			  hh,(float)p, (float)r, (float)pad_adc);
  //  			  }

  //  			}}}
  //oth->fill(   		    h1[1],log10(float(adc)));
  //  		  }
  //  		  double tpc_occ = 100.0 *((double)tpc.channels/(double)tpc.max_channels);
  //  		  // Physics triggers goes here
  //  		  if(trgcmd != 8 && trgcmd != 9 && trgcmd !=10){
  //oth->fill(   		    h1[3],tpc_occ);
  //  		  }
  //  		  // Occupancy for Pulser triggers goes here
  //  		  if(trgcmd == 8 && trgcmd ==10){
  //oth->fill(    		    h1[43],tpc_occ);
  //  		  }
  //  		  //Occupancy for Laser triggers goes here
  //  		  if(trgcmd == 9){
  //oth->fill(    		    h1[44],tpc_occ);

  //    		    float vDrift = laser->Make(mRunNumber, evp->evb_cou, &tpc);
  //    		    if(mDebugLevel)printf("drift velocity: %2.4f\n", vDrift);
  //oth->fill(    		    if(vDrift > 0.)h1[102],vDrift);
  //  		  }

  //  		  if(mDebugLevel)fprintf(stderr,"TPC: occupancy %3d %%, charge %d",(int)tpc_occ,adc) ;
  //  		}

  //--------------------------------------------------------------------

  //printf("svt\n");
  //fflush(stdout);

  ret = svtReader(datap) ;
  if(ret <= 0)
    {
      if (mDebugLevel) fprintf(stderr,"SVT: problems with data (%d) - continuing...",ret);
    }
  else
    {
      // example usage: calculate total charge and
      // print occupancy
      int s,r,p,a,t;
      unsigned int adc_total, adc;
      unsigned char val, tb;
      int index;
      int n_pixels, n_pixels_thresh;
      int n_pixels_west = 0;
      int n_pixels_east = 0;
      int n_pixels_rec[24];  // Bo added 02/08/03
      for(int reci=0;reci<24;reci++)
	{n_pixels_rec[reci]=0;}
      const unsigned int threshold = 100;
      unsigned int adc_anode, adc_time;
      float mean_anode, mean_time;
      index = 0;
      if(mDebugLevel)
	fprintf(stderr,"SVT: %d bytes",ret) ;

      svt_size = (float) ret;
      oth->fill(      h1[13],(svt_size>0? log10(svt_size) :0));

      if(total_size != 0.)
	{
	  svt_fract = 100.* svt_size/total_size;
	  oth->fill(	  h1[104],svt_fract);
	}
      adc_total = 0; // total charge count for SVT
      if(ret != 0)
	{
	  // Initialize (Bo: moved 02/16/05)
	  n_pixels_west   = 0;
	  n_pixels_east   = 0;
	  for(s=0;s<24;s++)
	    { // receiver
	      n_pixels_rec[s] = 0;
	      for(r=0;r<3;r++)
		{ // mezzanine
		  for(p=0;p<6;p++)
		    { // asic
		      adc = 0;
		      adc_anode  = 0;
		      adc_time   = 0;
		      mean_anode = 0;
		      mean_time  = 0;
		      n_pixels        = 0;
		      n_pixels_thresh = 0;
		      index++;
		      for(a=0;a<240;a++)
			{ // anode
			  for(t=0;t<svt.counts[s][r][p][a];t++)
			    {
			      val = svt.adc[s][r][p][a][t] ;
			      tb = svt.timebin[s][r][p][a][t];
			      if(adc > 0)
				{
				  n_pixels++;
				  n_pixels_rec[s] = n_pixels_rec[s]+1;
				  if(adc > threshold) n_pixels_thresh++;
				  if(s<12) n_pixels_west++;
				  else n_pixels_east++;
				}
			      adc_anode+= a*val;
			      adc_time += tb*val; // (Bo: modified with Marcelo (from +=t*val) 02/15/03)
			      adc += val;
			      adc_total += val;
			    }
			  if ( adc > threshold) {
			    oth->fill(			  (TH2*)h1[56],(float)index,(float)a);
			  }
			} // anode 
		      if(adc != 0)
			{
			  mean_anode = adc_anode/adc;
			  mean_time  = adc_time/adc;
			}
		      else
			{
			  // Put some safe numer here
			  mean_anode = 1;
			  mean_time  = 1;
			  if(mDebugLevel)
			    cout<<"SVT: division by 0 for averages!"<<endl;
			  if(mDebugLevel)
			    cout<<"s "<<s<<" r "<<r<<" p "<<p<<endl;
			}
			
		      TH2* hh1_svt = (TH2*)h1[39];
		      TH2* hh2_svt = (TH2*)h1[40];
                        
		      if(n_pixels>1000)
			{// Changes requested by Michal Bystersky
			  oth->fill(			  hh1_svt,(float)index,(float)n_pixels);
			    
			  oth->fill(			  hh2_svt,(float)index,(float)n_pixels_thresh);
			}
		      TH2* hh3_svt = (TH2*)h1[41];
		      oth->fill(		      hh3_svt,(float)index, mean_anode);

		      TH2* hh4_svt = (TH2*)h1[42];
		      oth->fill(		      hh4_svt,(float)index, mean_time);
		    } // asic
		} // mezzanine
	    } // receiver
	} // condition on ret 
      double svt_occ = 100.0 *((double)svt.channels/(double)svt.max_channels);
      double half_max_pixels = ((double)svt.max_channels)/2.; // Bo 12*3*6*240*128 = 6635520
      double receiver_max_pixels = ((double)svt.max_channels)/24.; // Bo 552960
      double svt_occ_west = 100.0 *((double)n_pixels_west/half_max_pixels);// Bo added 01/24/03
      double svt_occ_east = 100.0 *((double)n_pixels_east/half_max_pixels);// Bo added 01/24/03
      double svt_occ_rec[24];
      for(int reci=0;reci<24;reci++)
	{
	  svt_occ_rec[reci]= 100.0 *((double)n_pixels_rec[reci]/receiver_max_pixels);// Bo added 02/08/03
	}
      // Physics triggers goes here
      if(trgcmd != 8 && trgcmd != 9 && trgcmd !=10)
	{
	  oth->fill(	  h1[45],svt_occ); // Bo removed 01/24/03
	  oth->fill(	  h1[267],svt_occ_west);
	  oth->fill(	  h1[270],svt_occ_east);
	  if (svt_occ < 3.)
	    {
	      oth->fill(	      h1[144],float(ctbch));
	      oth->fill(	      h1[145],float(ctbch));
	    }
	}
      // Occupancy for Pulser triggers goes here
      if(trgcmd == 8 && trgcmd ==10)
	{
	  oth->fill(	  h1[46],svt_occ); // Bo removed 01/24/03
	  oth->fill(	  h1[268],svt_occ_west);
	  oth->fill(	  h1[271],svt_occ_east);
	}
      //Occupancy for Laser triggers goes here
      if(trgcmd == 9)
	{
	  oth->fill(	  h1[47],svt_occ); // Bo removed 01/24/03
	  oth->fill(	  h1[269],svt_occ_west);
	  oth->fill(	  h1[272],svt_occ_east);
	}
      for(int reci=0;reci<24;reci++)
	oth->fill(	h1[273+reci],svt_occ_rec[reci]);// Bo added 02/08/03
      if(mDebugLevel)
	fprintf(stderr,"SVT: occupancy %3d %%, charge %d",(int)svt_occ,adc_total) ;
    }
  //------------------------------------------------------------------SSD
  //              Silicon Strip Detector (Bo Hippolyte modified 12/14/04) 

  //printf("SSD\n");
  //fflush(stdout);
  
  ret = ssdReader(datap) ;
  if(ret <= 0)
    {
      if (mDebugLevel) fprintf(stderr,"SSD: problems with data (%d) - continuing...",ret) ;
    }
  else
    {// ret okay
      // for loops
      int n_ladder=40, n_module=16, n_strip=768, n_pad=64;
      int i_ladder=0, i_daq_ladder=0, i_real_ladder=0;
      int i_module=0, i_strip=0;
      // ladder wise counts
      float f_firedStrip_ladder=0; // to count fired strips per ladder for each event
      float f_signalStrip_ladder=0; // to count strips with signal (pulse>threshold) per ladder for each event
      int   n_firedStrip_ladder=0;
      int   n_signalStrip_ladder=0;
      int   pulseLadder=0; // mean pulse per fired strip per ladder
      // module wise counts
      float n_fired_module[16]; // to count fired strips per module for each event
      float n_signalStrip_module[16]; // to count strips with signal (pulse>threshold) per module for each event
      int pulseModule[16]; // mean pulse per fired strip per module
      // clustering and matching
      int ssdData_moduleN[16][768];
      int ssdData_moduleP[16][768];
      int clusterN=0, clusterP=0, matchingQ=0;
      int totalClusterQinModuleN=0, totalClusterQinModuleP=0;
      // values from data bank
      int n_fired_strips=0;
      int i_fired_strip=0;
      int strip_adc=0;
      // real module and strip numbering
      int i_real_module=0, i_real_strip=0;
      // --- Cuts ---
      int   ssdSignalThreshold=   25; // Threshold on ADC counts for Seed Strip (or signal strip)
      float ssdMaxAdcRatioForMatching =  0.5; // Max ratio between N&P Adc for Matching
      if(ssd.mode==0)
	{// mode okay
	  // ssd event size and fraction
	  ssd_size = float(ret);
	  oth->fill(	  h1[250],(ssd_size>0? log10(ssd_size) :0));
	  // modified for nice update
	  //            ssd_good++;
	  if(ssd_size !=0.) ssd_good=(int)h1[250]->GetEntries();
	  if(total_size != 0.)
	    {
	      ssd_fract = 100.* ssd_size/total_size;
	      oth->fill(	      h1[251],ssd_fract);
	    }
	  //------- SSD LADDER LOOP
	  // ========================================================================
	  //    The following block is the loop on all the ladders
	  //    In order to perform an online matching of clusters, the program reads : 
	  //    ladder 0 (N) and ladder 20 (P), then ladder 1 (N) and ladder 21 (P)   etc.
	  // ========================================================================
	  for (i_ladder=0; i_ladder<n_ladder; i_ladder++) {//LOOP on ladders 0->39
	    // Do a nasty trick to read daq ladder in the following order
	    // 0, 20, 1, 21, 2, 22, 3, 23, ..., 19, 39 instead of
	    // 0,  1, 2,  3, 4,  5, 6,  7, ..., 38, 39
	    // so as to read successively N and P sides of the same ladder
	    i_daq_ladder = (i_ladder%2)*20 + i_ladder/2;
	    i_real_ladder=SsdDaqToRealLadder[i_daq_ladder];
	    if (i_real_ladder==99) continue; //skip missing ladders
	    if (mDebugLevel>0)
	      printf("\n\n *** Starting Loop on Ladder Daq %d: so real is %d\n\n",i_daq_ladder,i_real_ladder);
	    // Init for online counts for online histos
	    n_firedStrip_ladder  = 0; // init fired strips counts to 0 for this event
	    n_signalStrip_ladder = 0; // init signal strips counts to 0 for this event
	    pulseLadder = 0; // sum of adc counts on ladder

	    //init fired strips counts for each module of this ladder
	    for( i_module=0;i_module<n_module;i_module++) { // LOOP on module 0->15
	      n_fired_module[i_module]=0.;
	      n_signalStrip_module[i_module]=0.;
	      pulseModule[i_module] = 0;
	      for( i_strip=0; i_strip<n_strip; i_strip++ ) {  // LOOP on strip 0->767
		if (i_daq_ladder<20) {
		  ssdData_moduleN[i_module][i_strip] = 0;
		}
		else {
		  ssdData_moduleP[i_module][i_strip] = 0;
		}
	      }
	    }
	    // Loop on the different levels of the data bank
	    // to retrieve data per module, strip
	    for (int i_pad=0; i_pad<n_pad; i_pad++) {//LOOP virtual pad: 0->63
	      n_fired_strips = ssd.counts[i_daq_ladder][i_pad];
	      for ( i_strip=0;i_strip<n_fired_strips;i_strip++) {//LOOP on fired strips
		i_fired_strip=ssd.strip[i_daq_ladder][i_pad][i_strip];
		strip_adc=log8to10_table[ssd.adc[i_daq_ladder][i_pad][i_strip]];
		i_real_strip=SsdStripNumber[((i_fired_strip*n_pad)+i_pad)%n_strip];
		if( i_daq_ladder<20 ) {
		  i_real_module = i_fired_strip/12; // 0->15 N side
		  ssdData_moduleN[i_real_module][i_real_strip] = strip_adc;
		}
		else {
		  i_real_module = 15 - i_fired_strip/12; // 0->15 P side
		  ssdData_moduleP[i_real_module][i_real_strip] = strip_adc;
		}
		n_firedStrip_ladder++; 
		n_fired_module[i_real_module] += 1;
		pulseLadder += strip_adc;
		pulseModule[i_real_module] += strip_adc;

		if( strip_adc > ssdSignalThreshold ) {
		  //----- Histogram is Filled
		  //oth->fill(		  		    h1SsdLadderLandauDist[i_real_ladder-1],strip_adc);
		  n_signalStrip_ladder++;
		  n_signalStrip_module[i_real_module] += 1.;
		}
		//----- Histogram is Filled
		//oth->fill(		  h1SsdLadderAdc[i_real_ladder-1],strip_adc);
	      } // end loop on fired strips
	    } // end loop on virtual pads
	    if (mDebugLevel>1) printf("*** Check 1 Good Ssd %d !! Filling Ladder %d: pulseLadder is %d with %d fired strips and %d strips of signal (max strip %d)\n",ssd_good,i_real_ladder,pulseLadder,n_firedStrip_ladder,n_signalStrip_ladder,n_strip*n_module);
	    // update mean pulse per strip
	    if( n_firedStrip_ladder>0. ) { pulseLadder /= (int)n_firedStrip_ladder; }
	    else { pulseLadder= 0; }
	    for( i_module=0;i_module<n_module;i_module++){ // LOOP module 0->15
	      if( n_fired_module[i_module]>0. ){
		if (mDebugLevel>2) printf("*** Checking Module=%d number of hits %.2f , signal %.2f and pulse %d \n",i_module+1,n_fired_module[i_module],n_signalStrip_module[i_module],pulseModule[i_module]);
		pulseModule[i_module] /= (int)n_fired_module[i_module];
	      }
	      else { pulseModule[i_module]=0; }
	    }
	    if (mDebugLevel>1) printf("*** Check 2 Good Ssd %d !! Filling Ladder %d: pulseLadder is %d with %d fired strips and %d strips of signal (max strip %d)\n",ssd_good,i_real_ladder,pulseLadder,n_firedStrip_ladder,n_signalStrip_ladder,n_strip*n_module);
	    // update occupancy (in %)
	    if( n_firedStrip_ladder>0) { f_firedStrip_ladder = 100.*((float)n_firedStrip_ladder)/(n_strip*n_module); }
	    else { f_firedStrip_ladder=0; }
	    if( n_signalStrip_ladder>0. ) { f_signalStrip_ladder = 100.*((float)n_signalStrip_ladder)/(n_strip*n_module); }
	    else { f_signalStrip_ladder=0; }
	    for(int module=0;module<n_module;module++){ // LOOP module 0->15
	      if( n_fired_module[module]>0. ){ n_fired_module[module] /= n_strip/100.; }
	      else { n_fired_module[module]=0.; }
	      if( n_signalStrip_module[module]>0. ){ n_signalStrip_module[module] /= n_strip/100.; }
	      else { n_signalStrip_module[module]=0.; }
	    }
	    // Histograms are Filled with updating bin content (first modified bin is 1 because bin 0 is underflow)
	    h1[252]->SetBinContent(i_real_ladder, (h1[252]->GetBinContent(i_real_ladder)*(ssd_good-1)+f_firedStrip_ladder)/ssd_good ); // h1SsdLadderOcc
	    h1[252]->SetEntries(ssd_good); // h1SsdLadderOcc
	    //	      if (mDebugLevel>1) printf("*** Checking Ladder=%d Occupancy=%.2f\%, entries = %d = ssd_good %d \n",i_real_ladder,h1[252]->GetBinContent(i_real_ladder),(int)h1[252]->GetEntries(),ssd_good);
	    h1[254]->SetBinContent(i_real_ladder, (h1[254]->GetBinContent(i_real_ladder)*(ssd_good-1)+f_signalStrip_ladder)/ssd_good ); // h1SsdLadderSigOcc
	    h1[254]->SetEntries(ssd_good); // h1SsdLadderSigOcc
	    if (mDebugLevel>1) printf("*** Checking Ladder=%d SignalOccupancy=%.5f, entries = %d = ssd_good %d \n",i_real_ladder,h1[254]->GetBinContent(i_real_ladder),(int)h1[254]->GetEntries(),ssd_good); // h1SsdLadderSigOcc
	    h1[256]->SetBinContent(i_real_ladder, (h1[256]->GetBinContent(i_real_ladder)*(ssd_good-1)+pulseLadder)/ssd_good ); //h1SsdLadderPulse
	    h1[256]->SetEntries(ssd_good);
	    if (mDebugLevel>1)
	      printf("*** Checking Ladder=%d Pulse=%.2f, entries = %d = ssd_good %d \n",i_real_ladder,h1[256]->GetBinContent(i_real_ladder),(int)h1[256]->GetEntries(),ssd_good);
	    if (mDebugLevel>1) printf("*** Filling Module Now\n");
	    for( i_module=0;i_module<n_module;i_module++){// LOOP module 0->15
	      if (mDebugLevel>2) printf("*** Checking Module=%d number of hits %.2f , signal %.2f and pulse %d \n",i_module+1,n_fired_module[i_module],n_signalStrip_module[i_module],pulseModule[i_module]);
	      h1[253]->SetBinContent(i_real_ladder, i_module+1, (h1[253]->GetBinContent(i_real_ladder,i_module+1)*(ssd_good-1)+n_fired_module[i_module])/ssd_good ); // h2SsdModuleOcc
	      h1[253]->SetEntries(ssd_good); // h2SsdModuleOcc
	      h1[255]->SetBinContent(i_real_ladder, i_module+1, (h1[255]->GetBinContent(i_real_ladder, i_module+1)*(ssd_good-1)+n_signalStrip_module[i_module])/ssd_good ); // h2SsdModuleSigOcc
	      h1[255]->SetEntries(ssd_good);
	      h1[257]->SetBinContent(i_real_ladder, i_module+1, (h1[257]->GetBinContent(i_real_ladder, i_module+1)*(ssd_good-1)+pulseModule[i_module])/ssd_good); // h2SsdModulePulse
	      h1[257]->SetEntries(ssd_good);
	    }
	    //----- Histogram is Filled
	    oth->fill(	    h1[340+i_real_ladder-1],f_firedStrip_ladder); // h1SsdLadderOccupDist[i_real_ladder-1]
	    if (mDebugLevel>1) cout<<"*** Checking Ladder="<<i_real_ladder<<", f_firedStrip_ladder="<<f_firedStrip_ladder<<", entries="<<(int)h1[340+i_real_ladder-1]->GetEntries()<<" \n";
	    //-----
	    //===========================================================
	    //   end of data reading for the current ladder
	    //    N and P sides
	    //  -> Now OnLine Clustering and Matching : only when loop on P (side N needs to be filled)
	    //===========================================================


	    for( i_module=0; i_module<n_module; i_module++ ) { // LOOP on modules for Clustering and Matching 
	      if (mDebugLevel>2) printf("*** Starting Cluster Matching for Ladder %d and Module %d \n",i_real_ladder,i_module);
	      totalClusterQinModuleP=0;
	      totalClusterQinModuleN=0;

	      for( int i_strip=0; i_strip<n_strip; i_strip++ ) { // LOOP on strips in current module
		//------------
		// Start Clustering
		if(i_daq_ladder<20){// For N side
		  if( ssdData_moduleN[i_module][i_strip] > ssdSignalThreshold ) {
		    clusterN=ssdData_moduleN[i_module][i_strip];
		    if (ssdData_moduleN[i_module][i_strip+1] >= clusterN) continue;
		    // Got a seed strip (above threshold) for current cluster
		    // Needs to add 2 adjacent strips (taking into account boundaries)
		    // Then skip the corresponding number of strips to the right
		    else {
		      switch (i_strip) {
		      case 0   : clusterN += ssdData_moduleN[i_module][i_strip+1]+ssdData_moduleN[i_module][i_strip+2]; i_strip+=2;
			break;
		      case 1   : clusterN += ssdData_moduleN[i_module][i_strip-1]+ssdData_moduleN[i_module][i_strip+1]+ssdData_moduleN[i_module][i_strip+2];i_strip+=2;
			break;
		      case 766 : clusterN += ssdData_moduleN[i_module][i_strip+1]+ssdData_moduleN[i_module][i_strip-1]+ssdData_moduleN[i_module][i_strip-2]; i_strip++; 
			break;
		      case 767 : clusterN += ssdData_moduleN[i_module][i_strip-1]+ssdData_moduleN[i_module][i_strip-2];
			break;
		      default  : clusterN += ssdData_moduleN[i_module][i_strip-2]+ssdData_moduleN[i_module][i_strip-1]+ssdData_moduleN[i_module][i_strip+1]+ssdData_moduleN[i_module][i_strip+2]; i_strip+=2;
			break;
		      }
		    } // end found seed strip for cluster
		    //----- Histograms are Filled
		    if (mDebugLevel>3) 
		      printf("*** Cluster N Found with charge %d \n",clusterN);
		    //oth->fill(		    		      h1SsdLadderClusterDist[i_real_ladder-1], (float)clusterN );    // N side
		    totalClusterQinModuleN+=clusterN;
		  }
		}
		else{// For P side
		  if( ssdData_moduleP[i_module][i_strip] > ssdSignalThreshold ) {
		    if (mDebugLevel>3) printf("Here this is P so i_daq_ladder %d >20 \n",i_daq_ladder);
		    clusterP=ssdData_moduleP[i_module][i_strip];
		    if (ssdData_moduleP[i_module][i_strip+1] >= clusterP) continue;
		    // Got a seed strip (above threshold) for current cluster
		    // Needs to add 2 adjacent strips (taking into account boundaries)
		    // Then skip the corresponding number of strips to the right
		    else {
		      switch (i_strip) {
		      case 0   : clusterP += ssdData_moduleP[i_module][i_strip+1]+ssdData_moduleP[i_module][i_strip+2]; i_strip+=2;
			break;
		      case 1   : clusterP += ssdData_moduleP[i_module][i_strip-1]+ssdData_moduleP[i_module][i_strip+1]+ssdData_moduleP[i_module][i_strip+2];i_strip+=2;
			break;
		      case 766 : clusterP += ssdData_moduleP[i_module][i_strip+1]+ssdData_moduleP[i_module][i_strip-1]+ssdData_moduleP[i_module][i_strip-2]; i_strip++; 
			break;
		      case 767 : clusterP += ssdData_moduleP[i_module][i_strip-1]+ssdData_moduleP[i_module][i_strip-2];
			break;
		      default  : clusterP += ssdData_moduleP[i_module][i_strip-2]+ssdData_moduleP[i_module][i_strip-1]+ssdData_moduleP[i_module][i_strip+1]+ssdData_moduleP[i_module][i_strip+2]; i_strip+=2;
			break;
		      }
		    } // end found seed strip for cluster
		    //----- Histograms are Filled
		    if (mDebugLevel>3)
		      printf("*** Cluster P Found with charge %d \n",clusterP);
		    // HERE WAS THE PROBLEM: to check
		    //oth->fill(		    	      h1SsdLadderClusterDist[i_real_ladder-1], (float)clusterP );    // P side
		    totalClusterQinModuleP+=clusterP;

		    // Start Matching only for P side of ladder
		    matchingQ = 0;
		    int first_strip = (767-i_strip-7<0)?0:767-i_strip-7; 
		    int last_strip = (767-i_strip+7>767)?767:767-i_strip+7;
		    for (int matchingStrip=first_strip; matchingStrip<= last_strip; matchingStrip++) {
		      matchingQ+=ssdData_moduleN[i_module][matchingStrip];
		    }
		    // Selection on Charge Ratio between P and N for Matching Clusters
		    if (matchingQ==0 || abs((1.-(float)clusterP/matchingQ)> ssdMaxAdcRatioForMatching) || abs((1.-(float)matchingQ/clusterP)> ssdMaxAdcRatioForMatching)) {i_strip++;i_strip++;continue; }
		    if (mDebugLevel>3)
		      printf("*** Found matching for ladder %d module %d and with charge P %d + charge N %d \n",i_real_ladder,i_module,clusterP,matchingQ);

		    oth->fill(		    h1[309+i_real_ladder-20-1], (float)matchingQ/clusterP ); // h1SsdLadderMatchingRatio // N divided by P
		    //oth->fill(		    		      h2SsdLadderChargeMatching[i_real_ladder-20-1],clusterP,matchingQ); // N vs P
		    //oth->fill(		    		      h1SsdLadderMatchedDist[i_real_ladder-20-1], (float)matchingQ );    // N side
		    //oth->fill(		    		      h1SsdLadderMatchedDist[i_real_ladder-1],(float)clusterP);          // P side
		  }
		}
	      } // end loop on strips
	      if (mDebugLevel>3) printf("*** Total cluster P charge=%d, N charge=%d \n",totalClusterQinModuleP,totalClusterQinModuleN);
	    } // end loop on modules
	  }// end loop on ladders
	  //------- End of the SSD LADDER LOOP
	}
    }
  //---------------------------------------------------------------------

  //printf("FTP\n");
  //fflush(stdout);

  ret = ftpReader(datap) ;
  if(ret <= 0)
    {
      if (mDebugLevel) fprintf(stderr,"FTP: problems with data (%d) - continuing...",ret) ;
    }
  else
    {
      // example usage: calculate total charge and
      // print occupancy
      int s,r,p,t ;
      int adc ;
      u_char val ;

      adc = 0 ;

      if(mDebugLevel)
	fprintf(stderr,"FTP: %d bytes",ret) ;

      ftp_size = float(ret);
      oth->fill(      h1[11],(ftp_size>0? log10(ftp_size) :0));
      if(total_size != 0.)
	{
	  ftp_fract = 100.* ftp_size/total_size;
	  oth->fill(	  h1[105],ftp_fract);
	}

      oth->fill(      h1[337],float(d_t_01),(ftp_size>0? log10(ftp_size) :0));  //JCS
      if(ret != 0)
	{
	  for(s=0;s<2;s++)
	    {	// FTPC: east, west
	      for(r=0;r<10;r++)
		{	// 10 rows each
		  for(p=0;p<960;p++)
		    {	// 960 pads each row
		      for(t=0;t<ftp.counts[s][r][p];t++)
			{
			  //	for(t=0;t<256;t++) {
			  val = ftp.adc[s][r][p][t] ;
			  adc += int(val) ;

			  //if(int(val)>0)cout << "t "<<t<<"val "<< int(val)<<endl;

			  oth->fill(			  h1[109+s],float(ftp.timebin[s][r][p][t]), float(val));
			  TH2 *hf  =  (TH2 *)h1[338+s];  //JCS
			  oth->fill(			  hf,r,p,int(val));        //JCS
			}
		    }
		}
	    }
	  oth->fill(	  h1[48],(adc>0? log10(float(adc)) :0));
	}
      double ftp_occ = 100.0 *((double)ftp.channels/(double)ftp.max_channels);
      // Physics triggers goes here
      if(trgcmd != 8 && trgcmd != 9 && trgcmd !=10)
	{
	  oth->fill(	  h1[49],ftp_occ);
	}
      // Occupancy for Pulser triggers goes here
      if(trgcmd == 8 && trgcmd ==10)
	{
	  oth->fill(	  h1[50],ftp_occ);
	}
      //Occupancy for Laser triggers goes here
      if(trgcmd == 9)
	{
	  oth->fill(	  h1[51],ftp_occ);
	}




      if(mDebugLevel)
	fprintf(stderr,"FTP: occupancy %3d %%, charge %d", (int)ftp_occ, adc) ;
    }
  //---------------------------------------------------------------------
  // TOF online QA, fill histograms. 
  ret = tofReader(datap) ;
  leadinghits.clear();
  trailinghits.clear();
  //cout<<"leading size="<<leadinghits.size()<<" trailing size "<<trailinghits.size()<<endl;
  if(ret <= 0)   {
    if (mDebugLevel) {fprintf(stderr,"TOF: problems in data (%d) - continuing...",ret);}
  } else {
    
    if(mDebugLevel) fprintf(stderr,"TOF: %d bytes",ret) ;
    /*
    tof_size =float(ret);
    oth->fill(    h1[14],log10(tof_size));
    if(total_size != 0.){tof_fract = 100.* tof_size/total_size;oth->fill(	h1[107],tof_fract);}
    */
    // Jing Liu, 12/05/2007

    int timeinbin=0;
    float time=0.;
    int halftrayid=-1;
    int trayid=-1;

    for(int ifib=0;ifib<4;ifib++) {  // fiber 0, east, fiber 2, west 
 
      int ndataword = tof.ddl_words[ifib];    // 
      //cout<<"TOF:: ifib="<<ifib<<" ndataword="<< ndataword<<endl;
      if(ndataword<=0) continue;
      for(int iword=0;iword<ndataword;iword++){
	int dataword=tof.ddl[ifib][iword];
        //cout<<"TOF :: dataword=0x"<<hex<<dataword<<dec<<" "<<iword<<"/"<<dec<<ndataword<<" ifiber="<<ifib<<endl;
	if( (dataword&0xF0000000)>>28 == 0x2) continue;  
	if( (dataword&0xF0000000)>>28 == 0xD) continue;  
	if( (dataword&0xF0000000)>>28 == 0xE) continue;  
        if( (dataword&0xF0000000)>>28 == 0xA) {  // header trigger data flag
	 // do nothing at this moment.
	  continue;
        }   
	// liujing new .....         
        if( (dataword&0xF0000000)>>28 == 0x6) {
          //cout<<"StBTofHitMaker::DATA ERROR!! packid=0x6"<<endl;
	  continue;
        }            
        if( (dataword&0xF0000000)>>28 == 0xC) {
          halftrayid = dataword&0x01;    
          trayid     = (dataword&0x0FE)>>1;
          //cout<<"TOF:: dataword/ndataword=0x"<<hex<<dataword<<"/"<<dec<<ndataword<<" halftrayid="<<halftrayid<<" trayid="<<trayid<<" fiber="<<ifib<<endl;
          continue;
        }

        if(halftrayid<0) continue;
        //cout<<"TOFaa:: dataword/ndataword=0x"<<hex<<dataword<<"/"<<dec<<ndataword<<" halftrayid="<<halftrayid<<" trayid="<<trayid<<" fiber="<<ifib<<endl;
        if(trayid<1 || trayid >122) {
          cout<<"StBTofHitMaker::DATA ERROR!! unexpected trayID ! "<<endl;
          continue;
        }

        //cout<<"TOFbb:: dataword/ndataword=0x"<<hex<<dataword<<"/"<<dec<<ndataword<<" halftrayid="<<halftrayid<<" trayid="<<trayid<<" fiber="<<ifib<<endl;

       // now get tdc chan, time from trailing and leading edge.
       // some triger words will be skipped.
       int edgeid =int( (dataword & 0xf0000000)>>28 );
       if((edgeid !=LEADING) && (edgeid!=TRAILING)) continue;


       int tdcid=(dataword & 0x0F000000)>>24;  // 0-15
       //cout<<"tdcid="<<tdcid<<" halftrayid="<<halftrayid<<endl;
       int tdigboardid=tdcid/4;   // 0-3 for half tray.
       int tdcchan=0;

       TofHitList temphit;
       //
       temphit.trayid=trayid;
       temphit.fiberid=ifib; 
       temphit.halftrayid=halftrayid;
       temphit.hptdcid = tdcid%4;     // tdcid here is 0-3;
       temphit.edgeid=edgeid;

       tdcchan=(dataword&0x00E00000)>>21;          // tdcchan is 0-7 here.
       timeinbin=((dataword&0x7ffff)<<2)+((dataword>>19)&0x03);  // time in tdc bin
       temphit.timeinbin=timeinbin; 
       time = timeinbin * 25./1024;   // time in ns 
       temphit.time=time;
       temphit.tdcchan=tdcchan;

       temphit.globaltdcchan=tdcchan + (tdcid%4)*8+tdigboardid*24+96*halftrayid; // 0-191 for tray

       int moduleid=-1;
       int modulechan=-1;
       int globalmodulechan=-1;
       if(trayid>0 && trayid< 121) { // TOF tray
	 globalmodulechan = tdcchan2mrpcchan(temphit.globaltdcchan);
         moduleid = globalmodulechan/6;
	 modulechan = globalmodulechan%6;
       } else if (trayid == 122 || trayid==121 ) {
         moduleid=trayid;
         modulechan=tdcchan2upvpdPMTchan(temphit.globaltdcchan,edgeid,trayid);
         globalmodulechan=modulechan;
       } 
	//
       temphit.moduleid=moduleid;
       temphit.modulechan=modulechan;
       temphit.globalmodulechan=globalmodulechan;
	//  
       temphit.numberforsort= time+globalmodulechan*1.e5+trayid*1.e8+ifib*1.e11;

       if(edgeid == LEADING) {
	 leadinghits.push_back(temphit);
       } else if(edgeid==TRAILING){
	 trailinghits.push_back(temphit); 
       } 
       //
       //cout<<"dataword=0x"<<hex<<dataword<<dec<<" edgeid="<<edgeid<<" tdcid="<<tdcid<<" tdigboardid="<<tdigboardid<<" hptdcid="<<tdcid%4<<" tdcchan="<<tdcchan<<" timeinbin="<<timeinbin<<" time="<<time;
       //cout<<" globaltdcchan="<<temphit.globaltdcchan<<" halftrayid="<<halftrayid<<" trayid="<<trayid<<" ifib="<<ifib<<endl;
       //cout<<" moduleid="<<temphit.moduleid<<" modulechan="<<temphit.modulechan<<" globalmodulechan="<<temphit.globalmodulechan<<" trayid="<<trayid<<endl<<endl;        
       //           
      }   // end loop all data words
    }  // end loop all fibers

    // all information we need are put in 2 vectors, now do some anaylysis and fill histograms.
    // sort it:
    std::sort(leadinghits.begin(), leadinghits.end(), compareIt);
    std::sort(trailinghits.begin(), trailinghits.end(), compareIt);
    for(unsigned int i=0;i<leadinghits.size();i++){
      //cout<<"leadinghits="<<i<<" fib="<<leadinghits[i].fiberid<<" trayid="<<leadinghits[i].trayid<<" globalmoduleid="<<leadinghits[i].globalmodulechan%19<<" time="<<leadinghits[i].time<<endl;
    }
    for(unsigned int i=0;i<trailinghits.size();i++){
      //cout<<"trailinghits="<<i<<" fib="<<trailinghits[i].fiberid<<" trayid="<<trailinghits[i].trayid<<" globalmoduleid="<<trailinghits[i].globalmodulechan%19<<" time="<<trailinghits[i].time<<endl;
    }

    // 
    // Make different loops on purpose, more readeable....
    //
    // get start time, t0east, t0west, t0 , only calculated from leading edge
    //
    float let0east(0),let0west(0);
    int neast(0),nwest(0);
    float let0(0);
    int counted[TOTPVPDCHAN];
    int pvpdchan[TOTPVPDCHAN];

    for(int i=0;i<TOTPVPDCHAN;i++){counted[i]=0;pvpdchan[i]=i;}
    // leading edge 
    for(unsigned i=0;i<leadinghits.size();i++){
      if(leadinghits[i].trayid==121) {
	for(int ich=0;ich<NEASTCHAN;ich++){
	  if((leadinghits[i].globalmodulechan==pvpdchan[ich]) && (counted[ich]==0 && leadinghits[i].time>0)){
	    let0west = leadinghits[i].time+let0west;nwest++;counted[ich]++;
	  }
	}
      }
      //
      if(leadinghits[i].trayid==122) {
	for(int ich=NEASTCHAN;ich<TOTPVPDCHAN;ich++){
	  if((leadinghits[i].globalmodulechan==pvpdchan[ich]) && (counted[ich]==0) && leadinghits[i].time>0){
	    let0east = leadinghits[i].time+let0east;neast++;counted[ich]++;
	  }
	}
      }
    }

    if(neast>0) let0east = let0east/neast;
    if(nwest>0) let0west = let0west/nwest;
    if(neast*nwest>0) {let0=(let0east+let0west)/2.;} 
    else if (neast>0) {let0=let0east;} 
    else if (nwest>0) {let0=let0west;} 
    else {// no hit in either start detector? // 
      //doing nothing
    }
    //cout<<"TOF:: let0west="<<let0west<<" let0east="<<let0east<<endl;
    //oth->fill(h1[453],let0west,let0east);
    int eastused[19],westused[19];
    for(int i=0;i<19;i++){eastused[i]=0;westused[i]=0;}
    for(unsigned int i=0;i<leadinghits.size();i++){
      if(leadinghits[i].trayid != 121) continue;    
      int westchan=leadinghits[i].globalmodulechan;
      for(unsigned int j=0;j<leadinghits.size();j++){
        if(leadinghits[j].trayid != 122) continue;    
        int eastchan=leadinghits[j].globalmodulechan;
        //if(eastchan !=westchan) continue;
        //cout<<" east: "<<leadinghits[j].time<<" west: "<<leadinghits[i].time<<endl; 
        if(eastused[eastchan]>0 || westused[westchan]>0) continue;
        oth->fill(h1[453],leadinghits[i].time,leadinghits[j].time);
        eastused[eastchan]++;westused[westchan]++;
      }
      }

    // jing liu, 12/10/2007
    // T0 from trailing edge , 
    // similar to above, we can add later if necessary.
 
    // fill histograms.
    // leading edge
    for(unsigned int i=0;i<leadinghits.size();i++){
      if(leadinghits[i].time>0&& leadinghits[i].trayid>0 && leadinghits[i].trayid<61) {
	oth->fill(h1[458],leadinghits[i].trayid,leadinghits[i].globaltdcchan/8);
      } else if(leadinghits[i].time>0&& leadinghits[i].trayid>60 && leadinghits[i].trayid<121){
	oth->fill(h1[457],leadinghits[i].trayid,leadinghits[i].globaltdcchan/8);
      } else if (leadinghits[i].time>0&& leadinghits[i].trayid==121){  //west start
	oth->fill(h1[454],leadinghits[i].modulechan);
      } else if (leadinghits[i].time>0&&leadinghits[i].trayid==122){  //east start
	oth->fill(h1[454],leadinghits[i].modulechan);
      }
    }
    // trailing edge.
    for(unsigned int i=0;i<trailinghits.size();i++){
      if(trailinghits[i].time>0&& trailinghits[i].trayid>0 && trailinghits[i].trayid<61) {
	oth->fill(h1[460],trailinghits[i].trayid,trailinghits[i].globaltdcchan/8);
      } else if(trailinghits[i].time>0&& trailinghits[i].trayid>60 && trailinghits[i].trayid<121){
	oth->fill(h1[459],trailinghits[i].trayid,trailinghits[i].globaltdcchan/8);
      } else if (trailinghits[i].time>0&& trailinghits[i].trayid==121){  //west start
	oth->fill(h1[455],trailinghits[i].modulechan);
      } else if (trailinghits[i].time>0&&trailinghits[i].trayid==122){  //east start
	oth->fill(h1[455],trailinghits[i].modulechan);
      }
    }

    // Make TOT plots ........
    float trayToT=0.; 
    int leused[120][192],teused[120][192];
    for(int i=0;i<120;i++){for(int j=0;j<192;j++){leused[i][j]=0;teused[i][j]=0;}}
    for(unsigned int ile=0;ile<leadinghits.size();ile++){
      int ldchan = leadinghits[ile].globalmodulechan;
      int ldtray = leadinghits[ile].trayid;
      if(ldtray<0 || ldtray>120) continue;
      if(ldchan<0 || ldchan>191 || leused[ldtray-1][ldchan]) continue;
      for(unsigned int jte=0;jte<trailinghits.size();jte++){
	int trchan=trailinghits[jte].globalmodulechan;
        int trtray = trailinghits[jte].trayid;
        if(trtray<0 || trtray>120) continue;
        if(trtray != ldtray) continue;

	if(trchan<0 || trchan>191 || teused[trtray-1][trchan]) continue;
        //
	if(ldchan == trchan) {
	  trayToT=trailinghits[jte].time-leadinghits[ile].time;

          //cout<<"TOFTOT:: TOT="<<trayToT<<" ldchan="<<ldchan<<" trchan="<<trchan<<" trayid="<<ldtray<<" ldtime="<<leadinghits[ile].time<<" trtime="<<trailinghits[jte].time<<endl;
          if(trayToT<=0.00001) continue;
	  teused[ldtray-1][ldchan]++;leused[ldtray-1][trchan]++;
          int TheTrayChan=ldchan + 192*((ldtray-1)%10);
          if(trtray >120 && trtray<123) { 
            // oth->fill(	  h1[465],ldchan,trayToT);
	  } else if (trtray>0 && trtray<11) {
            oth->fill(	  h1[467],TheTrayChan,trayToT);
	  } else if (trtray>10 && trtray<21) {
	    oth->fill(	  h1[468],TheTrayChan,trayToT);
	  } else if (trtray>20 && trtray<31) {
	    oth->fill(	  h1[469],ldchan+((ldtray-1)%10)*192,trayToT);
	  } else if (trtray>30 && trtray<41) {
	    oth->fill(	  h1[470],TheTrayChan,trayToT);
	  } else if (trtray>40 && trtray<51) {
	    oth->fill(	  h1[471],TheTrayChan,trayToT);
	  } else if (trtray>50 && trtray<61) {
	    oth->fill(	  h1[472],TheTrayChan,trayToT);
	  } else if (trtray>60 && trtray<71) {
	    oth->fill(	  h1[461],TheTrayChan,trayToT);
	  } else if (trtray>70 && trtray<81) {
	    oth->fill(	  h1[462],TheTrayChan,trayToT);
	  } else if (trtray>80 && trtray<91) {
	    oth->fill(	  h1[463],TheTrayChan,trayToT);
	  } else if (trtray>90 && trtray<101) {
	    oth->fill(	  h1[464],TheTrayChan,trayToT);
	  } else if (trtray>100 && trtray<111) {
	    oth->fill(	  h1[465],TheTrayChan,trayToT);
	  } else if (trtray>110 && trtray<121) {
	    oth->fill(	  h1[466],TheTrayChan,trayToT);
	  }
	}
      }
    }  

    // ToT plots for upvpd.
    //
    int sleused[38],steused[38];
    for(int i=0;i<38;i++){sleused[i]=0;steused[i]=0;}
    for(unsigned int ile=0;ile<leadinghits.size();ile++){
      int ldchan = leadinghits[ile].globalmodulechan;
      int ldtray = leadinghits[ile].trayid;
      if(ldchan<0 || ldchan>37) continue;
      if(sleused[ldchan]) continue;
      if(ldtray != 121 && ldtray != 122) continue;
      //cout<<"START:: ldtray="<<ldtray<<" ldchan="<<ldchan<<" time="<<leadinghits[ile].time<<endl;
      for(unsigned int jte=0;jte<trailinghits.size();jte++){
        int trtray = trailinghits[jte].trayid;
        if(trtray != ldtray) continue;
        int trchan=trailinghits[jte].globalmodulechan;
        //cout<<"START:: trtray="<<trtray<<" trchan="<<trchan<<" time="<<trailinghits[ile].time<<endl;
        if(trchan<0 || trchan>37) continue;
        if(trchan != ldchan) continue;
        if(steused[trchan]) continue;
        trayToT=trailinghits[jte].time-leadinghits[ile].time;
        steused[ldchan]++;sleused[trchan]++;
        //cout<<"STARTTOT:: TOT="<<trayToT<<" ldchan="<<ldchan<<" trchan="<<trchan<<" trayid="<<ldtray<<" ldtime="<<leadinghits[ile].time<<" trtime="<<trailinghits[jte].time<<endl;
        if(trayToT>0 && trayToT<80) {oth->fill(h1[456],ldchan,trayToT); oth->fill(h1[473],trayToT);}
      }                
    }
    //
 
    //
  }  // end if (ret <=0)

  //==============================================================================
  // end of TOF
  //==============================================================================


  //printf("PMD\n");
  //fflush(stdout);

  ret = pmdReader(datap) ;
  if(ret <= 0)
    {
      if (mDebugLevel) fprintf(stderr,"PMD: problems in data (%d) - continuing...",ret) ;
    }
  else
    {
      if(mDebugLevel)
	fprintf(stderr,"PMD: %d bytes",ret) ;
    }

  // 1D hists for 48 chains (mean ped and ADC), for PED RMS new set of hists(48 nos) added.
  if ( mDebugLevel) {
    cout<<"PMD Mode ="<<pmd.mode<<endl;
  }
  if(pmd.mode == 0)
    {     // normal event
      for( int sec=0; sec < Crate; sec++)//<2
	{
	  for(int rb=0; rb < CRAM; rb++)// < 12
	    {
	      for(int mz=0; mz < BLOCK; mz++)//BLK
		{
		  for(int channel=0; channel < CHANNEL; channel++)
		    {//<1728
		      Int_t Chain_No=(rb+1)+(sec*12)+(mz*24);
		      Int_t index=pmd_hist_begin+Chain_No;
		      //            cout<<"Ch:"<<channel<<" ADC="<<pmd.adc[sec][rb][mz][channel]<<endl;
		      oth->fill(		      h1[index],channel,pmd.adc[sec][rb][mz][channel]);
		      // filling 2D for chain_no vs channel
                       
		      TH2* h_chainCh_2d = (TH2*)h1[430];
		      if(pmd.adc[sec][rb][mz][channel]>0){
			oth->fill(			h_chainCh_2d,channel,Chain_No);
		      }
		      TH2* h_chainCh_2d_adc = (TH2*)h1[431];
		      oth->fill(		      h_chainCh_2d_adc,channel,Chain_No,pmd.adc[sec][rb][mz][channel]);
		      // filling 2D complete
		    }
		} //mz(BLK)
	    } //rb (CRAM)
	}//sec
    }
  //////
  /*
    if(pmd.mode == 1)
    {     // Pedestal event
    for( int sec=0; sec < Crate; sec++)//<2
    {
    for(int rb=0; rb < CRAM; rb++)// < 12
    {
    for(int mz=0; mz < BLOCK; mz++)//2
    {
    for(int channel=0; channel < CHANNEL; channel++)
    {//<1728
    Int_t Chain_No=(rb+1)+(sec*12)+(mz*24);
    Int_t index1=pmd_hist_begin+Chain_No;
    oth->fill(    h1[index1],channel,(double)pmd.ped[sec][rb][mz][channel]/16.);
    Int_t index2=pmd_hist_begin+48+Chain_No;
    oth->fill(    h1[index2],channel,(double)pmd.rms[sec][rb][mz][channel]/16.);
    }
    } //mz(BLK)
    } //rb (CRAM)
    }//sec
    }
  */

  //printf("bemc\n");
  //fflush(stdout);


  //==============================
  //      EMC histograms
  //==============================
  if(evp->token!=0){ // skip event summary

    BEMCPlots::fillHisto( (char*)datap
			  , trgd ? trgd->getDsm0_BEMCE() : 0
			  , trgd ? trgd->getDsm0_BEMCW() : 0
			  , trgd ? trgd->getDsm1_BEMC() : 0
			  , trgd ? trgd->getDsm2_EMC() : 0
			  , trgd ? trgd->getDsm3() : 0
			  );
    EEMCPlots::fillHisto( (char*)datap
			  , trgd ? trgd->getDsm0_EEMC() : 0
			  , trgd ? trgd->getDsm1_EEMC() : 0
			  , trgd ? trgd->getDsm2_EMC() : 0
			  , trgd ? trgd->getDsm3() : 0
			  );			      
  }
    // EMC end

      

  // printf("event tracker\n");
  //fflush(stdout);

  //---------------------------------------------------------------------
  // Event tracking is done here
  //
  ret = evtTracker->trackEvent(evp, mem, l3p, sizL3_max);
  if (!(ret<0)) ret = evtTracker->copyl3_t(l3,l3p);

  if(ret < 0)
    {
      fprintf(stderr,"L3: problems in data (%d) - continuing...",ret) ;
      cout<<"Error tracking event: "<<evp->seq<<endl;
      goto END;
    }
     
  // if(l3p->tracks.off == 0){
  //       cout<<"No tracks produced for event: "<<evp->seq<<endl;
  //       goto END;
  //}
    
    
    
  if(mDebugLevel)
    fprintf(stderr,"L3: %d bytes",ret) ;
  l3_size =float(ret);
  oth->fill(  h1[12],(l3_size>0? log10(l3_size) :0));

  if(total_size != 0.)
    {
      l3_fract = 100.* l3_size/total_size;
      oth->fill(      h1[108],l3_fract);
    }

  //csp 10/08/01
  if(trgcmd == 4)
    {
      // Fill L3 specific histograms
      oth->fill(      h1[88],float(l3.tracks_num));
      if(mDebugLevel)
	cout<<"L3 Tracks:"<<l3.tracks_num<<endl;
      // Fill only nonzero vertexes
      if((l3.xVertex != 0.) && (l3.yVertex != 0.) && (l3.zVertex != 0.))
	{
	  oth->fill(	  h1[89],l3.xVertex);
	  oth->fill(	  h1[90],l3.yVertex);
	  oth->fill(	  h1[91],l3.zVertex);

	  // ZDC/BBC vs L3 vertex
	  TH2* hh1_l3_zdc = (TH2*)h1[100];
	  oth->fill( hh1_l3_zdc,l3.zVertex, mZdcVertex);
	  TH2* hh1_l3_bbc = (TH2*)h1[230];
	  oth->fill( hh1_l3_bbc,l3.zVertex, mBbcVertex);
	  oth->fill(h1[112],l3.zVertex, mZdcTimeDiff);

	  // L3 Y vs X
	  TH2* hh1_l3_x_y = (TH2*)h1[101];
	  oth->fill( hh1_l3_x_y,l3.xVertex, l3.yVertex);

	  // Write vertex files only when running LIVE
	  //
	  static time_t lastUpdateTime = time(NULL);
	  if ( difftime(time(NULL),lastUpdateTime) >60 ) {
	    lastUpdateTime = time(NULL);
	    cout << __PRETTY_FUNCTION__ << " :: trying to write vertex: " << ctime( &lastUpdateTime) << endl;
	    cout << __PRETTY_FUNCTION__ << " :: evp->isevp = " <<  evp->isevp << endl; 
	    if(evp->isevp){
	      writeVertex(89); 
	      writeVertex(90);
	      writeVertex(91);
	    }
	  }
	  //  		      int l3_track_vertex_good = 0;
	  //  		      int l3_track_vertex_bad  = 0;

	  float l3_pt_tot = 0;

	  for(unsigned int i=0; i<l3.tracks_num; i++)
	    {

	      oth->fill(	      h1[65],l3.track[i].z0);
	      // Fishing for upstream background tracks
	      //TH2* hh1_l3_in_out = (TH2*)h1[60];
	      //oth->fill(	      hh1_l3_in_out,l3.track[i].innerMostRow, l3.track[i].outerMostRow);
	      //float paddiff = l3.track[i].outerMostRow - l3.track[i].innerMostRow;
	      //Track properties
	      //oth->fill(	      h1[61],paddiff);
	      oth->fill(	      h1[62],l3.track[i].pt);
	      oth->fill(	      h1[63],l3.track[i].phi0);
	      oth->fill(	      h1[64],l3.track[i].psi);

	      l3_pt_tot += l3.track[i].pt;

	      //			 TH2* hh1_l3_paddiff_length = (TH2*)h1[65];
	      //oth->fill(	      hh1_l3_paddiff_length,paddiff,l3.track[i].length);

	      //TH2* hh1_l3_psi_length = (TH2*)h1[66];
	      //oth->fill(	      hh1_l3_psi_length,l3.track[i].psi,l3.track[i].length);

	    }// loop over L3 tracks
	  // Per event quantities here
	  //oth->fill(	 			  h1[57],l3_track_vertex_good);
	  //oth->fill(	  h1[58],l3_track_vertex_bad);

	  //oth->fill(	  if(l3_track_vertex_good != 0)h1[59],100.*float(vertex_bad)/float(vertex_good));

	  // Study for Steve Trentelage
	  for(int i=0; i<nMaxTrgWd; i++)
	    {

	      //oth->fill(	           h1[297],float(i));
	      //oth->fill(	       			      h1[298+i],minl);
	      //  			      double cont = h1[308]->GetBinContent(i);
	      //cout<<"i("<<i<<")->"<<trg.offline_id[i]<<" Pt_tot="<<l3_pt_tot<<endl;

	      int trgid = trg.offline_id[i];
	      if(trgid != 0)
		{
		  if(trgid == 1000)
		    oth->fill(		    h1[71],l3_pt_tot);
		  if(trgid == 1101)
		    oth->fill(		    h1[72],l3_pt_tot);
		  if(trgid == 1201)
		    oth->fill(		    h1[73],l3_pt_tot);
		  if(trgid == 1202)
		    oth->fill(		    h1[74],l3_pt_tot);
		}
	    }// i loop trg words



	}// L3 found good vertex

    }// trgcmd
  // obsolete for Run5   }// good L3 data



  //	}
 END:

  //printf("Histohandler::filled...\n");
  //fflush(stdout);

  iret = evp->status;
  return iret ;

}

  //---------------------------------------------------------------------
  void HistoHandler::writeVertex(const int histoNum) {
    //SP 03/08/2004
    // Write L3 vertex histograms to disk
    //
    // This is the dumbest way I could come out with. ;-)
    // This part can be streamlined and probably will be....
    // Ugly, but it works.

    // Remember number of previous entries
    int Entries_old   = 0; // common storage
    static int Entries_old_x = 0;
    static int Entries_old_y = 0;
    static int Entries_old_z = 0;
  
    //int interval = 0;   // how many events to wait
    int Nentries;

    int nbinx; 
    float xLo; float xHi;
    float mean; float rms;



    //Get Number of entries
    Nentries = (int) h1[histoNum]->GetEntries();

    if(histoNum == 89){
      Entries_old = Entries_old_x;
    }
    if(histoNum == 90){
      Entries_old = Entries_old_y;
    }
    if(histoNum == 91){
      Entries_old = Entries_old_z;
    }


    cout << "Entries= "<<Nentries<<" Old entries ="<<Entries_old<<endl;
    
    mean = h1[histoNum]->GetMean();
    rms = h1[histoNum]->GetRMS();
    nbinx = h1[histoNum]->GetNbinsX();
    xLo = h1[histoNum]->GetXaxis()->GetXmin();
    xHi = h1[histoNum]->GetXaxis()->GetXmax();
    
    // Position dependence here! Careful.
    //Open ASCII file for recreate

    ofstream fout;
    
    switch (histoNum) {
    case 89:
      fout.open("/a/histos/cdev/vertex_x.dat", ios::trunc | ios::out); 
      Entries_old_x = Nentries; // remember how many vertexes we have now
      break;
    case 90:
      fout.open( "/a/histos/cdev/vertex_y.dat", ios::trunc | ios::out); 
      Entries_old_y = Nentries; // remember how many vertexes we have now
      break;
    case 91:
      fout.open( "/a/histos/cdev/vertex_z.dat", ios::trunc | ios::out); 
      Entries_old_z = Nentries; // remember how many vertexes we have now
      break;
    default:
      cout << __PRETTY_FUNCTION__ << " :: unknown histogram id : " << histoNum << endl;
      return;
    }
    
    time_t seconds;
    
    seconds = time(NULL);
    fout <<seconds <<endl;
    fout << mean << endl;
    fout << rms << endl;
    fout << nbinx<<endl;
    fout << xLo<<endl;
    fout << xHi<<endl; 
    
    //Loop over bins
    float w;
    for (Int_t i=1; i<nbinx+1; i++) {
      w = (int) h1[histoNum]->GetBinContent(i);
      // fout<<w<<endl;
    }
    fout.close();
    return;
  }

  ////////////////////////////////////////////SSD
  // SSD Strip Conversion Method
  ///////////////////////////////////////////////
  void SsdTabToStripConversion() {
    int lStartStrip = 0;
    for (int iChip=0;iChip<6;iChip++){
      lStartStrip = iChip*128;
      for(int next=0;next<32;next++)
	SsdStripNumber[lStartStrip+64+2*next] = lStartStrip+next; 
      for(int next=0;next<64;next++)
	SsdStripNumber[lStartStrip+127-2*next] = lStartStrip+32+next; 
      for(int next=0;next<32;next++)
	SsdStripNumber[lStartStrip+2*next] = lStartStrip+96+next; 
    }
  }










  /***************************************************************************
   *
   * $Id: HistoHandler.cxx,v 1.13 2009/03/10 17:31:57 genevb Exp $
   *
   * Author: Frank Laue, laue@bnl.gov
   ***************************************************************************
   *
   * Description:
   *
   ***************************************************************************
   *
   * $Log: HistoHandler.cxx,v $
   * Revision 1.13  2009/03/10 17:31:57  genevb
   * Removed a reference to a user's directory
   *
   * Revision 1.12  2009/03/09 03:40:37  genevb
   * Akio's corrections for BBC timing
   *
   * Revision 1.11  2009/03/05 01:31:43  genevb
   * Update L3 tracking for new DAQ reader
   *
   * Revision 1.10  2009/03/05 00:03:16  dkettler
   * EMC Updates
   *
   * Revision 1.9  2009/02/27 22:30:17  dkettler
   * TOF Updates
   *
   * Revision 1.8  2009/02/17 19:24:56  dkettler
   * Fixed ZDC/BBC-l3 vertex plots
   *
   * Revision 1.7  2009/02/14 00:11:00  dkettler
   * BEMC trigger data check
   *
   * Revision 1.6  2009/02/13 22:23:06  dkettler
   * Trigger data changes
   *
   * Revision 1.5  2009/02/11 22:15:50  jeromel
   * Akio's implementation of a generic class, FPD, ZDC, DSM, ...
   *
   * Revision 1.4  2009/02/06 16:55:13  genevb
   * Trigger version problem temporary patch
   *
   * Revision 1.3  2009/02/04 01:25:51  dkettler
   * Remove ONLINEPLOTSDIR reference
   *
   * Revision 1.2  2009/01/23 22:26:35  jeromel
   * Change config file location
   *
   * Revision 1.1  2009/01/23 16:11:04  jeromel
   * Import from online/RTS/src/
   *
   * Revision 1.31  2009/01/23 00:36:33  genevb
   * Gently avoid TRG data from Runs <9 (from A. Ogawa)
   *
   * Revision 1.30  2009/01/21 01:24:17  genevb
   * EMC updates (from O. Grebenyuk)
   *
   * Revision 1.29  2009/01/20 22:02:06  genevb
   * Modifications to TRG data interpretation, now using StEvent (from A. Ogawa)
   *
   * Revision 1.28  2009/01/16 20:54:32  genevb
   * TPC 1 sector offset using new daq reader
   *
   * Revision 1.27  2009/01/13 00:12:13  fine
   * Zero pointer protection
   *
   * Revision 1.26  2009/01/12 19:40:11  genevb
   * NULL ptr and divide-by-zero protections
   *
   * Revision 1.25  2009/01/12 17:58:42  genevb
   * Consistent protection against log of <=0
   *
   * Revision 1.24  2009/01/12 17:44:54  genevb
   * protection against log of <=0
   *
   * Revision 1.23  2009/01/08 20:10:51  fine
   * fix the bemc interfaces
   *
   * Revision 1.22  2009/01/08 19:45:46  fine
   * fix typo
   *
   * Revision 1.21  2009/01/08 19:43:27  fine
   * fix typo
   *
   * Revision 1.20  2009/01/08 19:39:27  fine
   * fix the bemcFillHisto function signature  HistoHandler.cxx
   *
   * Revision 1.19  2008/12/19 17:09:15  fine
   * the first full compilation against of the new DAQ Reader
   *
   * Revision 1.18  2008/03/17 22:54:15  fine
   * get rif of the redundant tpc paramater to make it RTS for offline compliant too. Thanks Paul
   *
   * Revision 1.17  2008/03/03 20:51:55  psoren
   * reimplementation of TOF updates from Jing Liu
   *
   * Revision 1.16  2008/02/27 16:06:50  dkettler
   * Minor fix
   *
   * Revision 1.15  2008/02/27 15:19:46  dkettler
   * Laser fix, again
   *
   * Revision 1.14  2008/02/22 18:16:03  dkettler
   * *** empty log message ***
   *
   * Revision 1.13  2008/02/15 18:51:51  dkettler
   * Updates to laser and TOF reader
   *
   * Revision 1.12  2008/01/07 17:40:34  psoren
   * debug laser problem
   *
   * Revision 1.11  2007/12/13 02:48:19  psoren
   * Repeat drift velocity calculation if fails on first attempt
   *
   * Revision 1.10  2007/06/01 21:26:40  psoren
   * Run 7 Ssd mapping updated
   *
   * Revision 1.9  2007/05/25 14:53:44  jml
   * blah
   *
   * Revision 1.8  2007/05/07 18:58:22  laue
   * Added drift time distribution histograms
   *
   * Revision 1.7  2007/05/03 13:33:38  laue
   * *** empty log message ***
   *
   * Revision 1.6  2007/04/25 17:52:29  laue
   * Minor updates
   *
   * Revision 1.5  2007/04/05 16:49:29  laue
   * *** empty log message ***
   *
   * Revision 1.4  2007/04/03 13:19:33  laue
   * Some minor modification on laser histograms by request from Blair
   *
   * Revision 1.3  2007/03/26 16:16:37  laue
   * ZDC vertex recalibrated
   *
   * Revision 1.2  2007/02/27 18:15:53  laue
   * *** empty log message ***
   *
   * Revision 1.1  2007/02/27 15:23:37  laue
   * Initial version
   *
   * Revision 1.1  2006/10/27 17:43:29  laue
   * Resources folder added
   * histogram controll class OTH added
   *
   * Revision 1.1  2006/10/04 20:31:34  laue
   * Initial Version
   *
   *
   ***************************************************************************/
