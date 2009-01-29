#include "EvpUtil.h"

#include "TMapFile.h"
#include "TPad.h"
#include "TStyle.h"
#include "TLegend.h"
#include "TSystem.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include "StEEmcPool/muEztPanitkin/EEqaPresenter.h"
#include "StEmcPool/StBEMCPlots/BEMCPlotsPresenter.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TRegexp.h"
#include "TThread.h" 
#include "TEnv.h" 


#ifndef NEW_DAQ_READER
#  include <evpReader.hh>
#  include <daqFormats.h>
#else
#  include "HistoHandler.h"
#  include "DAQ_READER/daqReader.h"
#  include "DAQ_TRG/trgReader.h"
#  include "DAQ_READER/cfgutil.h"
#endif

#include "RunStatus.h"

#include <iomanip>
#include <utility>
using namespace std;


const char* EvpUtil::mProjectPath =  
      getenv("ONLINEPLOTSDIR")?  getenv("ONLINEPLOTSDIR") :
       gEnv->GetValue("Online.ProjectPath",".");
int EvpUtil::mCanvasWidth = 400;
int EvpUtil::mCanvasHeight = 400*4/3;
int EvpUtil::mDebugLevel = 0;
int EvpUtil::mSharedMemorySize = 140*1000*1000;
const char* EvpUtil::mInputPath = gEnv->GetValue("Online.InputPath","/a");
const char* EvpUtil::mOutputPath =  gEnv->GetValue("Online.OutputPath","/a/pplot/histos/");
char* EvpUtil::mMapFilePath = EvpUtil::cat(EvpUtil::mOutputPath,"/evpEventServer.map");
char* EvpUtil::mCanvasDescriptionFile = EvpUtil::cat(EvpUtil::mProjectPath,"/local/CanvasDescriptions.txt");
int EvpUtil::mNumberOfTabs = 0;
int EvpUtil::mNumberOfSubTabs[MAX_TABS];
int EvpUtil::nx[MAX_TABS][MAX_SUBTABS];
int EvpUtil::ny[MAX_TABS][MAX_SUBTABS];
int EvpUtil::nHist[MAX_TABS][MAX_SUBTABS];  
unsigned int EvpUtil::canvasTriggerBits[MAX_TABS][MAX_SUBTABS];  
unsigned int EvpUtil::canvasDetectorBits[MAX_TABS][MAX_SUBTABS];  
TString EvpUtil::hNames[MAX_TABS][MAX_SUBTABS][MAX_PADS]; 
TH1* EvpUtil::hHist[MAX_TABS][MAX_SUBTABS][MAX_PADS]; 
TH1* EvpUtil::hUnknown = new TH1D("unknown","unknown histogram",10,0.,10.);

extern TStyle* gStyle;
extern TSystem* gSystem;

char* EvpUtil::cat(const char* a, const char* b) {
  char* c= new char[1024];
  sprintf(c,"%s%s",a,b);
  return c;
}



unsigned int EvpUtil::evpgroupmask( char* group ) {
  unsigned int ret = str2evpgroupmask(group);
  if ( strstr(group,"any")!=0 )  ret = 0xffffffff; 
  return ret;
}
 
unsigned int EvpUtil::detmask( char* group) {
  if ( strstr(group,"any")!=0 ) return 0xffffffff; 
  return str2detmask(group);
}


//-------------------------------------------------------------
const char* EvpUtil::GetInputPath() { 
  return mInputPath;
}
//-------------------------------------------------------------
const char* EvpUtil::GetOutputPath() { 
  //   static char path[1024];
  //   sprintf(path,"%s/evpHistos",getenv("HOME"));
  //   return path;
  return mOutputPath;
}
//-------------------------------------------------------------
const char* EvpUtil::GetProjectPath() { 
  return mProjectPath;
}
//-------------------------------------------------------------
int EvpUtil::GetLogY(const int i,const int j)
{
  // Holds information about LogY settings for a canvas
  int mLogY = 0; // Default no logY

  //trigger words

  if(i==0 && j==3)
    {
      mLogY = 1;  // changed from 1. May 16th SP
    }
  //FTPC Second subpad (Charge Step)
  else if(i == 6 && j == 2)
    {
      //FTPC requested log scale for the second canvas
      mLogY = 1;
      //	   cout<<"Set Log Scale"<<endl;
    }
  //BBC stuff
  else if(i == 8 && (j == 3 || j == 4 || j == 5))
    {
      mLogY = 1;
    }
  //FPD stuff
  else if(i == 9 && (j== 1 || j==2) )
    {
      mLogY = 1;
    }
  return mLogY;
}
//-------------------------------------------------------------
int EvpUtil::GetLogZ(const int i,const int j)
{
  // Holds information about LogZ settings for a canvas
  int mLogZ = 0; // Default no LogZ

  if(i == 0 && j == 3)
    {
      mLogZ = 0;
    }
  else if(i == 3 && (j == 2 || j == 3))
    {
      //TPC sectors charge per pad
      mLogZ = 1;
    }
  return mLogZ;
}

//-------------------------------------------------------------
bool  EvpUtil::DisplayOneCanvas(TMapFile* mfile , TPad* gcc, const int i, const int j, bool doClear) {



  GenericFile* gen = new GenericFile(mfile);
  bool iret = DisplayOneCanvas(gen, gcc, i, j, doClear); 
  delete gen;
  return iret;
}

//--------------------------------------------------------------
int EvpUtil::ReadCanvasDefinitions(char *fname) {
  int nLines=0;
  int nTabs=0;
  TString buffer;
  mNumberOfTabs = 0;
  memset(mNumberOfSubTabs,0,sizeof(mNumberOfSubTabs));
  // Open file for reading
  ifstream fin(fname);
  if (!fin) {
    cerr << "Unable to open"<<fname<<" for reading\n";
    return(-1);
  }
  while(!fin.eof()) {
    buffer.ReadLine(fin);
    if (mDebugLevel) {
      cout << buffer.Data() << endl;
    }
    nTabs += ReadCanvasDefinition(buffer);
    nLines++;
  }
  if(mDebugLevel) {
    cout<<"number of lines: "<<nLines<<endl;
    cout<<"number of canvases: "<<nTabs<<endl;
  }
  fin.close();
  return 0;
}

int EvpUtil::ReadCanvasDefinition(TString line) {
  TObjArray tokens;
  TString canvasDescription;
  TString canvasCondition;
  int tab =0;
  int subTab = 0;
    //
    //Convention here: first symbol / means commentary
    //
    if(line.BeginsWith("/"))  return 0;
    if(mDebugLevel) {
      cout << line.Data()<< endl;
    }
    //
    // '//' is comment 
    //
    int pos = line.First("//");
    if ( pos!=-1) {
      cout << line.Data() << endl;
      line.Remove(pos,line.Length()-pos);
      cout << line.Data() << endl;
    }

    // 
    // canvas conditions are makred as '[triggerCond|detectorCond]'
    //
    canvasCondition = line;
    pos = canvasCondition.First('[');
    canvasCondition.Remove( 0,pos);
    pos = canvasCondition.First(']');
    canvasCondition.Remove( pos+1, canvasCondition.Length()-pos);
    //cout << canvasCondition.Data() << endl;    

    canvasDescription = line;
    canvasDescription.ReplaceAll(canvasCondition,""); 
    canvasDescription.ReplaceAll(",,",",");
    //cout << canvasDescription.Data() << endl;    

    
    //
    // Another convention here:
    // All tokens are separated by commas. Hence, no commas in the title!
    //
    int nTokens =0;
    nTokens = ParseString(canvasDescription.Data(),tokens);


    if(mDebugLevel) {
      cout<<"Tokens: "<<nTokens<<endl;
    }
    //Check if this is an empty string
    if(nTokens == 0) return 0;
    if(nTokens<5) {
      cerr<<" too few tokens : "<<nTokens<<endl;
      return 0;
    }


    tab = atoi( ((TObjString *)tokens[0])->String().Data() );
    subTab = atoi( ((TObjString *)tokens[1])->String().Data() );
    mNumberOfTabs = max(mNumberOfTabs,tab+1);
    mNumberOfSubTabs[tab] = max(mNumberOfSubTabs[tab],subTab+1);
 
    //title = ((TObjString *)tokens[2])->String().Data();
    nx[tab][subTab] = atoi( ((TObjString *)tokens[2])->String().Data() );
    ny[tab][subTab] = atoi( ((TObjString *)tokens[3])->String().Data() );
    nHist[tab][subTab] = atoi( ((TObjString *)tokens[4])->String().Data() );
 
    int last = 0;
    last = tokens.GetLast();
    if(mDebugLevel) {
      cout<<"last element "<<last<<endl;
    }
    //Pure sanity check
    if(last != 4+nHist[tab][subTab]) {
      cerr << canvasDescription.Data() << endl;
      cerr << (last -4+nHist[tab][subTab]) << "  missing histogram descriptions! " <<endl;
    }
    // Now read names of histograms which will be displayed in this subtab
    TString t;
    
    for (int i=0; i<nHist[tab][subTab]; i++) {
	t = ((TObjString *)tokens[5+i])->GetString();
	hNames[tab][subTab][i] = t.Data();
	hHist[tab][subTab][i] = 0;
	//cout <<hNames[tab][subTab][i]<< "***" << endl;
	//printf("%d %d %d %s \n",tab,subTab,i,hNames[tab][subTab][i].Data());
    }

    ReadCanvasCondition(canvasCondition,canvasTriggerBits[tab][subTab],canvasDetectorBits[tab][subTab]);

    if(mDebugLevel) {
      cout<<"Tab ="<<tab<<" subTab="<<subTab<<endl;
      cout<<"nx="<<nx[tab][subTab]<<" ny="<<ny[tab][subTab]<<" nHist="<<nHist[tab][subTab]<<endl;
      cout<<"triggerBits="<<hex<<canvasTriggerBits[tab][subTab]<<" detectorBits="<<hex<<canvasDetectorBits[tab][subTab]<< endl;
      cout<<"Tab ="<<tab<<" subTab="<<subTab;
      cout<<"  triggerBits="<<canvasTriggerBits[tab][subTab]<<" detectorBits="<<canvasDetectorBits[tab][subTab]<< endl;
    }

    tokens.Delete();
  return 1;
}


int EvpUtil::ReadCanvasCondition(TString line,unsigned int& trg, unsigned int& det) {
  char buf[1024];
  char ctrg[1024];
  char cdet[1024];
  sprintf(buf,"%s",line.Data());
  const char* delims = "[|]";
  //cout << __PRETTY_FUNCTION__ << "  " << line;
  char *result = NULL;
  result = strtok( buf, delims );
  sprintf(ctrg,"%s",result);
  result = strtok(NULL,delims);
  sprintf(cdet,"%s",result);
  //  cout << ctrg << " " << " " << cdet << endl;
  trg = str2evpgroupmask( ctrg );
  det = str2detmask( cdet );
  if (!trg) trg=0xffffffff;  // ToDo: keep this only until jeff fixed the bits
  if (!det) det=0xffffffff;  // ToDo: keep this only until jeff fixed the bits
  //cout << " = " << hex << trg << " " << hex << det << endl; 
return 1;
}


void EvpUtil::CheckCanvasDefinitions(TMapFile* mfile) {
  if ( !mfile ) {
    cerr << "### error ###  memory mapped file not available " << endl;
    exit(-1);
  }
  return;
  for ( int tab=0; tab<MAX_TABS; tab++) {
    for ( int subtab=0; subtab<MAX_SUBTABS; subtab++) {
      for ( int pad=0; pad<nHist[tab][subtab]; pad++) {
	if ( hNames[tab][subtab][pad]!="" ) {
	  if ( !GetHistoFromMapFile(mfile,hNames[tab][subtab][pad].Data())  ) { // memory leak
	    cerr << "### error ### could not find histogram: " <<  hNames[tab][subtab][pad].Data()  << endl;
	    //exit(-1);
	  }
	} else {
	  cerr << "### warning ### no histogram defined for tab,subtab,pad:  " << tab <<"," << subtab << "," << pad << endl;
	}
      }
    }
  }
}


//_____________________________________________________________________
int EvpUtil::ParseString (const TString &tChain, TObjArray &Opt) {

  Int_t nParsed = 0;
  Ssiz_t begin, index, end;
  begin = index = end  = 0;
  //Comma separated tokens
  TRegexp separator("[^,|]+");
  TString Tag, opt, nopt;

  while ((begin < tChain.Length()) && (index != kNPOS) )
    {
      // loop over given Chain options
      index = tChain.Index(separator,&end,begin);

      if (index >= 0)
	{
	  //csp TString substring(tChain(index,end));
	  index = end;
	  TString substring(tChain(begin,end));

	  if(mDebugLevel) {
	    cout<<"begin:"<<begin<<" end:"<<end<<" index:"<<index<<" T:"<<substring.Data()<<endl;
	  }

	  Opt.Add(new TObjString(substring.Data()));
	  nParsed++;
	}
      begin += end+1;
    }
  return nParsed;
}


//_____________________________________________________________________
int EvpUtil::GetSizeOfMappedObjects(TMapFile * MapFile) {

  //////////////////////////////////////////////////////////////////////////////
  // Name:           TMrbAnalyze::GetSizeOfMappedObjects
  // Purpose:        Determine total size of all objects mapped so far
  // Arguments:      TMapFile * MapFile      -- current map file
  // Results:        Int_t NofBytes          -- number of bytes allocated
  // Exceptions:     
  // Description:    Loops thru objects in mapfile, calculates total size.
  // Keywords:       
  //////////////////////////////////////////////////////////////////////////////

  TMapRec * mr = MapFile->GetFirst();
  if(!mr){
    cerr << "### error ### No records" << endl;
    cerr << "### error ### Eval::GetSizeOfMappedObjects" << endl;;
    return(0);
  }
  Int_t size = 0;
  Bool_t ok = kTRUE;
  int nObj=0;
  int s = 0;
  while (MapFile->OrgAddress(mr)) {
    if(!mr) break;
    s =  mr->GetBufSize();
    if(s <= 0)ok = kFALSE;
    size += s;
    nObj++;
    mr = mr->GetNext();
  }
  if (mDebugLevel) {
    cout << nObj << "  " << s << " " << size << " " << (double)size/MapFile->GetSize() << "%" << endl;
  }
  if(!ok){
    cerr << "### error ### MapFile too small: "<< size << endl;
    cerr << "### error ### Eval::GetSizeOfMappedObjects" << endl;;
    MapFile->Print();
    return(0);  
  }
  //cout << " press any key to continue" << endl; cin.ignore();
  if (mDebugLevel) {
    cout << "Eval::GetSizeOfMappedObjects Size used on Mapfile (KBytes): " << size / 1024 << endl;
  }
  return(size);
}


int EvpUtil:: Map2Root(const char* map , const char* root) {
  TMapFile* mFile = TMapFile::Create(map);
  if ( !mFile) {
    cerr << "### error ### Can not create file : " << map << endl;
    mFile->Close();
    return -1;
  }
  TMapRec * mr = mFile->GetFirst();
  if(!mr){
    cerr << "### error ### No records" << endl;
    cerr << "### error ### Eval::GetSizeOfMappedObjects" << endl;;
    return -2;
  }

  TFile* f = new TFile(root,"recreate");
  if (!f) {
    cerr << " ### error ### opening file :" << root << endl;
    return -3;
  }

  TObject* o = 0;
  RunStatus* rs;
  TH1* hi;

  while (mFile->OrgAddress(mr)) {
    if(!mr) break;
    //cout << mr->GetName() << endl;
    o = mFile->Get( mr->GetName(), o );
    hi = dynamic_cast<TH1*>(o);
    rs = dynamic_cast<RunStatus*>(o);
    if (hi) hi->Write();
    if (rs) rs->Write();
    mr = mr->GetNext();
  }
  f->Close();
  mFile->Close();
  return 0;
}

int EvpUtil:: Map2Root(TMapFile* mFile, const char* root) {
  if ( !mFile) {
    cerr << "### error ### file pointer is 0 " << endl;
    return -1;
  }
  TMapRec * mr = mFile->GetFirst();
  if(!mr){
    cerr << "### error ### No records" << endl;
    cerr << "### error ### Eval::GetSizeOfMappedObjects" << endl;;
    return -2;
  }

  TFile* f = new TFile(root,"recreate");
  if (!f) {
    cerr << " ### error ### opening file :" << root << endl;
    return -3;
  }

  TObject* o = 0;
  RunStatus* rs;
  TH1* hi;

  while (mFile->OrgAddress(mr)) {
    if(!mr) break;
    //cout << mr->GetName() << endl;
    o = mFile->Get( mr->GetName(), o );
    hi = dynamic_cast<TH1*>(o);
    rs = dynamic_cast<RunStatus*>(o);
    if (hi) hi->Write();
    if (rs) rs->Write();
    mr = mr->GetNext();
  }
  f->Close();
  return 0;
}
//_____________________________________________________________________
TObject* EvpUtil::GetObjectFromMapFile(TMapFile* file, const char* name, TObject* o) {
  o = file->Get(name,o);
  return o;
}
//_____________________________________________________________________
TObject* EvpUtil::GetObjectFromGenericFile(GenericFile* gen, const char* name, TObject* o) {
  o = gen->Get(name,o);
  return o;
}
//_____________________________________________________________________
TH1* EvpUtil::GetHistoFromMapFile(TMapFile* file, const char* name, TH1* h) {
  if ( h!=hUnknown ) {  // in order to not delete hUnknown 
    h = (TH1*) GetObjectFromMapFile(file,name,h);
  } else { 
    h = (TH1*) GetObjectFromMapFile(file,name,0);
  }
  if (h)  {
    h->SetDirectory(0);
    return h;
  }
  cerr << " ### error ### Could not find histogram : " << name << endl;
  return hUnknown;
}
//_____________________________________________________________________
TH1* EvpUtil::GetHistoFromGenericFile(GenericFile* file, const char* name, TH1* h) {
  if ( h!=hUnknown ) {  // in order to not delete hUnknown 
    h = (TH1*) GetObjectFromGenericFile(file,name,h);
  } else { 
    h = (TH1*) GetObjectFromGenericFile(file,name,0);
  }
  if (h) {
    h->SetDirectory(0);
    return h;
  }
  cerr << " ### error ### Could not find histogram : " << name << endl;
  return hUnknown;
}
//_____________________________________________________________________
int EvpUtil::Root2Map(const char* root, const char* map) {
  TFile* rFile = new TFile(root);
  if ( !rFile) {
    cerr << "### error ### Can not open file : " << root << endl;
    return 1;
  }
  TMapFile* mFile = TMapFile::Create(map,"RECREATE", EvpUtil::mSharedMemorySize);
  if ( !mFile) {
    cerr << "### error ### Can not create file : " << map << endl;
    rFile->Close();
    return 2;
  }
  TList* list = rFile->GetListOfKeys();
  if ( !list || list->GetEntries()==0 ) {
    cerr << "### error ### Can not find any entries in file : " << root << endl;
    rFile->Close();
    mFile->Close();
    return 1;
  }
  
  int n = list->GetEntries();
  for ( int i=0; i<n; i++) {
    TObject* t = list->At(i);
    TObject* t1 = rFile->Get(t->GetName());
    mFile->Add( t1,t1->GetName());
  }
  mFile->Update();
  mFile->Close();
  rFile->Close();
  return 0;
}

//-------------------------------------------------------------
void EvpUtil::Draw(TH1*h, const char* options) {
  if ( !h ) return;
  double max = h->GetBinContent( h->GetMaximumBin() );
  if ( max<=0 ) {
    h->SetMaximum(1);
  }
  h->Draw(options);
}

//-------------------------------------------------------------
bool  EvpUtil::DisplayOneCanvas(GenericFile* gFile , TPad* gcc, const int i, const int j, bool doClear) {
// get rid of red boarder around current pad on a canvas
  static TLegend* bunchCounterLegend = new TLegend(0.7, 0.6, 0.95, 0.95);
  static TLegend* bunchCounterLegend2 = new TLegend(0.7, 0.6, 0.95, 0.95);
  static TH1 * hh[MAX_PADS];// local histogram for one canvas
  bunchCounterLegend->Clear();
  bunchCounterLegend2->Clear();

  //gcc->SetHighLightColor(0);
  gcc->cd(); 
  gcc->Clear();
  if (doClear) gcc->Draw();
  gcc->Divide(nx[i][j], ny[i][j]);

  //Check if LogY is requested for the pad
  int mLogY = GetLogY(i,j);
  //Check if LogY is requested for the pad
  int mLogZ = GetLogZ(i,j);

  gcc->SetLogy(mLogY);
  gcc->SetLogz(mLogZ);
  gStyle->SetPalette(1,0);
  
  //==============================
  //      EMC histograms
  //==============================

  // EEMC
  if(i>=10 && i<=12 ) {
    //    printf("\n\n EEMC display i=%d j=%d\n\n",i,j);
        eePlot(i,j,gFile, gcc, gEnv->GetValue("Online.eemcMask","eemcTwMask.dat"));
	goto ret;
  }
  
  // BEMC
  if (i == 5) {
    BEMCPlotsPresenter::displayTab(i - 5, j - 1, gFile, gcc, 0);
    goto ret;
    
  }
  // End EMC

  //Special footwork for FTPC charge histoes
  //
  //Careful here! Introduces a lot of dependencies on ListOfHist and
  //CanvasDescription
  //02/26/2004 SP
  //
  if(i==6 && j==3) {
    gcc->SetLogz(0);
    gStyle->SetOptStat(0);
    //h11_ftp_evsize
    // Main dependence is here.
    //
    // Hist indexing starts with 0
    bool bad = false;    
    
    if ( hNames[1][2][2].Data()=="" ) {
      cout << "hNames[1][2][2].Data()==\"\"" << endl;
      bad = true;
    }
    if ( hNames[6][3][0].Data()=="" ) {
      cout << "hNames[6][3][0].Data()!=0" << endl;
      bad = true;
    }
    if ( hNames[6][3][1].Data()=="" ) {
      cout << "hNames[6][3][1].Data()!=0" << endl;
      bad = true;
    } 
    
    if ( bad ) { 
      cerr << " ### error ### non existing histograms " << endl;
      goto ret;
    }
    
    
    
    hHist[1][2][2] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[1][2][2].Data(), hHist[1][2][2]);
    //h338_ftp_west it's 2d
    hHist[6][3][0] = (TH2 *)GetHistoFromGenericFile(gFile,hNames[6][3][0].Data(), hHist[6][3][0]);
    //h339_ftp_east
    hHist[6][3][1] = (TH2 *)GetHistoFromGenericFile(gFile,hNames[6][3][1].Data(), hHist[6][3][1]);
    
    int nEntries = (int) hHist[1][2][2]->GetEntries();
    //cout<<"h11 Entries "<<nEntries<<endl;
    
    //To avoid division by zero check number of entries
    //If OK proceed. Needed when FTPC is not in the run
    float ScaleFactor = 1.0;
    
    if(nEntries>0) ScaleFactor /= (nEntries);
    //cout<<"ScaleFactor:"<<ScaleFactor<<endl;
    gcc->cd(1); hHist[6][3][0]->Scale(ScaleFactor);  hHist[6][3][0]->SetMaximum(600);  Draw(hHist[6][3][0],"colz");
    gcc->cd(2); hHist[6][3][1]->Scale(ScaleFactor);  hHist[6][3][1]->SetMaximum(600);  Draw(hHist[6][3][1],"colz");
    goto ret;
  }
  
  // Very special footwork for time of flight
  // if(i==7 && j==2)
  //     {
  //         // Actual get of the histogram
  //         //82
  //         cc->cd(1);
  //         //
  //         hh[0] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][0].Data(), hh[0]);
  //         //69
  //         hh[1] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][1].Data(), hh[1]);
  
  //         hh[0]->SetFillColor(0);
  //         hh[0]->Draw();
  //         hh[1]->SetFillColor(0);
  //         hh[1]->SetLineColor(2);
  //         Draw(hh[1],"same");
  
  //         cc->cd(2);
  //         //83
  //         hh[2] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][2].Data(), hh[2]);
  //         //70
  //         hh[3] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][3].Data(), hh[3]);
  
  //         hh[2]->SetFillColor(0);
  //         hh[2]->Draw();
  //         hh[3]->SetFillColor(0);
  //         hh[3]->SetLineColor(2);
  //         Draw(hh[3]->Draw("same");
  //         goto ret;
  //     }
  
  // Very special footwork for Trigger Word
  //     if(i==0 && j==5)
  //     {
  //         // Put here by sp instead of very top of the method
  //         TLegend *legend = new TLegend(0.7, 0.6, 0.95, 0.95);
  //         gStyle->SetOptStat(0);
  //         // gStyle->SetStatW(0);
  //         // gStyle->SetStatH(0);
  //         cc->cd(1);
  //         hh[0] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][0].Data(), hh[0]);
  //         //hh[0]->SetDirectory(0);
  //         hh[11] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][11].Data(), hh[11]);
  //         //hh[11]->SetDirectory(0);
  //         Draw(hh[0]->Draw();
  //         TH1 *htemp[10];
  //         for(int k=0; k<10; k++)
  //         {
  //             int color=k+1;
  //             if (color>9)
  //             {color+=1;}
  //             //
  //             // This cloning causes memory leak in the initial version from Akio
  //             //  hCleanUp list is introduced in order to remove cloned histograms
  //             // It's filled here and cleaning is happening in DoLiveButton
  //             //  SP 05/17/2003
  //             htemp[k]=(TH1F*) hh[0]->Clone("htemp");
  
  //             htemp[k]->SetFillColor(color);
  //             htemp[k]->SetLineColor(color);
  //             htemp[k]->SetAxisRange(k,k+1,"x");
  //             htemp[k]->Draw("SAME");
  //             hCleanUp->Add(htemp[k]);
  //             int trgid = int(hh[11]->GetBinContent(k));
  //             printf("trgid %d\n",trgid);
  //             if(trgid!=0)
  //             {
  //                 char title[100];
  //                 GetTriggerId(trgid,title);
  //                 legend->AddEntry(htemp[k],title,"F");
  //             }
  //         }
  //         legend->Draw();
  
  //         cc->cd(2);
  //         double hmax, maxmax=0.0;
  //         int maxh=1;
  //         for(int k=0; k<10; k++)
  //         {
  //             int color=k+1;
  //             if (color>9)
  //             {color+=2;}
  //             hh[k+1] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][k+1].Data(), hh[k+1]);
  //             hh[k+1]->SetFillColor(0);
  //             hh[k+1]->SetLineColor(color);
  //             hmax = hh[k+1]->GetMaximum();
  //             cout<<"hmax["<<k+1<<"]= "<<hmax<<endl;
  //             if(maxmax<hmax)
  //             { maxmax=hmax; maxh=k+1; }
  //         }
  //         Draw(hh[maxh]->Draw();
  //         for(int k=0; k<10; k++)
  //         {
  //             if(k+1 != maxh)
  //                 Draw(hh[k+1]->Draw("SAME");
  //         }
  //         goto ret;
  //     }
  
  // Special footwork for bunch counter
  if(i==1 && j==4)
    {
      gStyle->SetOptStat(0);
      
      gcc->cd(1);
  
      hh[0] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][0].Data(),hh[0]);
      hh[0]->SetFillColor(1); hh[0]->SetLineColor(1);
      Draw(hh[0]); bunchCounterLegend->AddEntry(hh[0],"Events","F");
      hh[1] = (TH1 *)GetHistoFromGenericFile(gFile,"h442_bunch_yellow_fill",hh[1]);
      hh[1]->SetFillColor(3); hh[1]->SetLineColor(3);
      Draw(hh[1],"SAME"); bunchCounterLegend->AddEntry(hh[1],"Y Filled","F");
      hh[2] = (TH1 *)GetHistoFromGenericFile(gFile,"h443_bunch_yellow_up",hh[2]);
      hh[2]->SetFillColor(2); hh[2]->SetLineColor(2);
      Draw(hh[2],"SAME"); bunchCounterLegend->AddEntry(hh[2],"Y Up","F");
      hh[3] = (TH1 *)GetHistoFromGenericFile(gFile,"h444_bunch_yellow_down",hh[3]);
      hh[3]->SetFillColor(4); hh[3]->SetLineColor(4);
      Draw(hh[3],"SAME"); bunchCounterLegend->AddEntry(hh[3],"Y Down","F");
      hh[4] = (TH1 *)GetHistoFromGenericFile(gFile,"h445_bunch_yellow_unpol",hh[4]);
      hh[4]->SetFillColor(5); hh[4]->SetLineColor(5);
      Draw(hh[4],"SAME"); bunchCounterLegend->AddEntry(hh[4],"Y Unpol","F"); 
      bunchCounterLegend->Draw();

      gcc->cd(2); 

      hh[5] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][0].Data(),hh[5]);
      hh[5]->SetFillColor(1); hh[5]->SetLineColor(1);
      Draw(hh[5]); bunchCounterLegend2->AddEntry(hh[5],"Events","F");
      hh[6] = (TH1 *)GetHistoFromGenericFile(gFile,"h446_bunch_blue_fill",hh[6]);
      hh[6]->SetFillColor(3); hh[6]->SetLineColor(3);
      Draw(hh[6],"SAME"); bunchCounterLegend2->AddEntry(hh[6],"B Filled","F");
      hh[7] = (TH1 *)GetHistoFromGenericFile(gFile,"h447_bunch_blue_up",hh[7]);
      hh[7]->SetFillColor(2); hh[7]->SetLineColor(2);
      Draw(hh[7],"SAME"); bunchCounterLegend2->AddEntry(hh[7],"B Up","F");
      hh[8] = (TH1 *)GetHistoFromGenericFile(gFile,"h448_bunch_blue_down",hh[8]);
      hh[8]->SetFillColor(4); hh[8]->SetLineColor(4);
      Draw(hh[8],"SAME"); bunchCounterLegend2->AddEntry(hh[8],"B Down","F");
      hh[9] = (TH1 *)GetHistoFromGenericFile(gFile,"h449_bunch_blue_unpol",hh[9]);
      hh[9]->SetFillColor(5); hh[9]->SetLineColor(5);
      Draw(hh[9],"SAME"); bunchCounterLegend2->AddEntry(hh[9],"B Unpol","F");
      bunchCounterLegend2->Draw();
      goto ret;      
    }
  //---------------------------------------------
  //   "Normal" histograms processed below
  //---------------------------------------------
  // Loop over specified tabs on a canvas
  for(int k=0; k<nHist[i][j]; k++)
    {

      if(mDebugLevel) {
	cout<<"Get histo:"<<hNames[i][j][k]<<endl;
      }
      // Actual get of the histogram
      //    hh[k] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][k].Data(), hh[k]);
      gPad->GetListOfPrimitives()->Remove( hh[k]);
      hHist[i][j][k] = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][k].Data(),hHist[i][j][k]);
      //hHist[i][j][k] = mHist[i][j][k];

      // Check if what we asked for exists
      if(hHist[i][j][k] == NULL)
	{
	  if(mDebugLevel)
	    cout<< "Histogram "<<hNames[i][j][k]<<" was not found!"<<endl;
	  continue;
	}
      if(mDebugLevel) {
	cout<< "Histogram "<<hNames[i][j][k]<<" was found! "<<hHist[i][j][k]<<endl;
      }

      // cd to pad
      gcc->cd(k+1);
      TString current_name = hHist[i][j][k]->GetName();
      //cout<<"Name "<<current_name.Data()<<endl;
      //set defaults no titles
      hHist[i][j][k]->SetXTitle(" ");
      hHist[i][j][k]->SetYTitle(" ");

      if(mDebugLevel) {
	cout<<"Canvas change OK"<<endl;
      }

      if(
	 (strstr(current_name.Data(),"h252_ssd_lad_occ"))||
	 (strstr(current_name.Data(),"h254_ssd_lad_sig_occ"))||
	 (strstr(current_name.Data(),"h256_ssd_lad_pulse"))
	 ) hHist[i][j][k]->SetXTitle("Real Ladder #");

      //Special treatment for 2d histograms.
      // They look better that way
      //  if(hHist[i][j][k]->IsA()->InheritsFrom("TH2"))
      if ( dynamic_cast<TH2*>(hHist[i][j][k]) ) {
	gStyle->SetOptStat(0);

	if(strstr(current_name.Data(),"h54_ctb_zdc"))
	  hHist[i][j][k]->SetNdivisions(5,"x");
	if(strstr(current_name.Data(),"h55_ctb_mwc"))
	  hHist[i][j][k]->SetNdivisions(5,"x");

	// Axis Title for the SSD 
	if((strstr(current_name.Data(),"h253_ssd_mod_occ"))||
	   (strstr(current_name.Data(),"h255_ssd_mod_sig_occ"))||
	   (strstr(current_name.Data(),"h257_ssd_mod_pulse"))){
	  hHist[i][j][k]->SetXTitle("Real Ladder #");
	  hHist[i][j][k]->SetYTitle("Real Module #");
	}
	//Axis Title for BEMC stuff

	if(strstr(current_name.Data(),"bemc_CratesStatus"))
	  hHist[i][j][k]->SetXTitle("TDC Status");
	if(strstr(current_name.Data(),"bsmd_Capacitor"))
	  {hHist[i][j][k]->SetXTitle("Fiber optics number (RDO)");hHist[i][j][k]->SetYTitle("Capacitor Number");}

	if(strstr(current_name.Data(),"h325_bemc_HT"))
	  {hHist[i][j][k]->SetXTitle("Trigger Tower Number");hHist[i][j][k]->SetYTitle("ADC");}

	if(strstr(current_name.Data(),"h326_bemc_PA"))
	  {hHist[i][j][k]->SetXTitle("Trigger Tower Number");hHist[i][j][k]->SetYTitle("ADC");}

	Draw(hHist[i][j][k],"colz");
      }
      else
	{
	  // 1d histoes are printed here

	  if(strstr(current_name.Data(),"bemc_Status"))
	    hHist[i][j][k]->SetXTitle("EMC Event Status");
	  if(strstr(current_name.Data(),"HTDistr"))
	    hHist[i][j][k]->SetXTitle("ADC");
	  if(strstr(current_name.Data(),"PADistr"))
	    hHist[i][j][k]->SetXTitle("ADC");

	  gStyle->SetOptStat(1);
	  Draw(hHist[i][j][k],"");
	  if(mDebugLevel)
	    cout<<"Drawing..."<<endl;
	}
      gPad->SetLogy(mLogY);
      gPad->SetLogz(mLogZ);
    }// k loop ends here


 ret:
  gcc->Update();
  if(mDebugLevel) {
    cout << "__PRETTY_FUNCTION__" << endl; 
  }
  return true;

}


bool EvpUtil::HasEntries(GenericFile* gFile , int i, int j) {
  for(int k=0; k<nHist[i][j]; k++) {
      TH1* h  = 0;
      h = (TH1 *)GetHistoFromGenericFile(gFile,hNames[i][j][k].Data(),h);
      if ( h ) {
	if ( h->GetEntries() ) {
	  if ( h->GetMean()>0 || h->GetRMS()>0 ) return true;
	}
      }
  }
  return false;       
}



/***************************************************************************
 *
 * $Id: EvpUtil.cxx,v 1.4 2009/01/29 20:32:28 dkettler Exp $
 *
 * Author: Frank Laue, laue@bnl.gov
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: EvpUtil.cxx,v $
 * Revision 1.4  2009/01/29 20:32:28  dkettler
 * BEMC changes
 *
 * Revision 1.3  2009/01/27 20:40:27  genevb
 * Unified environment variable Online.*
 *
 * Revision 1.2  2009/01/23 22:26:35  jeromel
 * Change config file location
 *
 * Revision 1.1  2009/01/23 16:11:04  jeromel
 * Import from online/RTS/src/
 *
 * Revision 1.13  2009/01/20 23:41:48  genevb
 * Modified function call to eePlot (from O. Grebenyuk)
 *
 * Revision 1.12  2009/01/13 21:10:55  fine
 * typo
 *
 * Revision 1.11  2009/01/13 00:44:02  fine
 * Add rootrc parameters
 *
 * Revision 1.10  2009/01/13 00:39:07  fine
 * Add rootrc parameters
 *
 * Revision 1.9  2008/12/19 17:09:15  fine
 * the first full compilation against of the new DAQ Reader
 *
 * Revision 1.8  2008/02/29 10:55:53  dkettler
 * *** empty log message ***
 *
 * Revision 1.7  2007/05/25 14:53:44  jml
 * blah
 *
 * Revision 1.6  2007/05/24 13:15:17  jml
 * blah
 *
 * Revision 1.5  2007/04/05 16:49:29  laue
 * *** empty log message ***
 *
 * Revision 1.4  2007/03/21 17:11:45  laue
 * Moved individual HistogramGroups to their own folder
 * Modified Makefile and server to allow profiling with gprof (i.e. must not exot with exit(...) function)
 *
 * Revision 1.3  2007/03/08 15:08:28  laue
 * *** empty log message ***
 *
 * Revision 1.2  2007/03/01 20:40:34  laue
 * Updates to print active HistogramGroups into ps/pdf files
 *
 * Revision 1.1  2007/02/27 15:23:37  laue
 * Initial version
 *
 * Revision 1.2  2006/10/27 17:43:21  laue
 * Resources folder added
 * histogram controll class OTH added
 *
 * Revision 1.1  2006/10/04 20:31:15  laue
 * Initial Version
 *
 *
 ***************************************************************************/

