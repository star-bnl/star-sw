/**********************************************************************
 *
 * $Id: StEStructCuts.cxx,v 1.13 2012/11/21 17:45:43 fisyak Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Abstract class for cuts. It does implement reading 
 *               of a cut file and building histograms. Specific
 *               cuts are done in derived classes
 *
 ***********************************************************************/
#include <stdlib.h>

#include "StEStructCuts.h"
#include "StString.h"
#include "Stiostream.h"
#include "Stsstream.h"
#include "TFile.h"
#include "TH1.h"
using namespace std;
ClassImp(StEStructCuts)

//-------------------------------------------------------------------
StEStructCuts::StEStructCuts(): mcutFileName(0), mMaxStore(100) { initVars();};

//-------------------------------------------------------------------
StEStructCuts::StEStructCuts(const char* cutFileName) : mcutFileName(0), mMaxStore(100) {

  initVars();
  if(cutFileName) setCutFile(cutFileName);
};

//-------------------------------------------------------------------
StEStructCuts::~StEStructCuts(){ if(mcutFileName) delete [] mcutFileName ; 

 deleteVars();

}

//--------------------------------------------

void StEStructCuts::printCuts(ostream& os, int i){
  os<<"<CutType=\""<<mcutTypeName<<"\">"<<endl;
  if(i>=0)os<<"<index>"<<i<<"</index>"<<endl;
  printCutStats(os);
  os<<"</CutType>"<<endl;
}


//-------------------------------------------------------------------
void StEStructCuts::initVars(){

  mvarName = new char*[mMaxStore];
  mvalues  = new float[mMaxStore];
  mminVal  = new float[mMaxStore];
  mmaxVal  = new float[mMaxStore];
  mvarHistsNoCut  = new TH1*[mMaxStore];
  mvarHistsCut    = new TH1*[mMaxStore];
  mnumVars=0;
  mDoFillHists = true;
}

//-------------------------------------------------------------------

void StEStructCuts::deleteVars() {


  for(int i=0;i<mnumVars; i++){
    delete [] mvarName[i];
    delete mvarHistsCut[i];
    delete mvarHistsNoCut[i];
  }

  delete [] mvarName; 
  delete [] mvalues;
  delete [] mminVal;
  delete [] mmaxVal;
  delete [] mvarHistsNoCut;
  delete [] mvarHistsCut;

}


//-------------------------------------------------------------------
void StEStructCuts::setCutFile(const char* cutFileName) {

  if(!cutFileName) return;
  if(mcutFileName) delete [] mcutFileName;

  mcutFileName=new char[strlen(cutFileName)+1];
  strcpy(mcutFileName,cutFileName);

  //  loadCuts();
  
};

//-------------------------------------------------------------------
bool StEStructCuts::loadCutDB() {
  // Loads pre-defined cuts from database

  bool flag = false;

  if (!strcmp(mcutFileName,"testdb")) {      // Some crazy cuts for testing... 
    loadBaseCuts("primaryVertexZ","-15","25");        
    loadBaseCuts("Pt","0.15","5");                    
    loadBaseCuts("Phi","-1","0.8");                   
    loadBaseCuts("Eta","-1.5","0.5");                 
    
    flag = true;
  }

  // ***** begin copy here ******* 
  if (!strcmp(mcutFileName,"template")) {   // example template for adding new cut systems to the db
    /***********************************************************************************************
     * Title:  example cuts for 200 GeV p-p correlation analysis
     * PA:  john doe
     * Date:  August 2004
     * fileCatalog:  production=P04XX,trgsetupname=productionCentral,filetype=daq_reco_MuDst,etc.
     * Notes:  Analysis for PRL paper "Transverse Momentum vs Unleaded Gasoline Prices at RHIC"
     *         results in ~johndoe/work/fakeanalysis/data
     *         Webpage at protected/estruct/johndoe/...
     ***********************************************************************************************/
    
    // syntax: loadBaseCuts("cut name", "min value", "max value")  
    //   min and max define the accepted region; e.g. loadBaseCut("mycut","0","1") cuts everything below 0 and above 1
    // Note: in a few special cases, some cuts may need to be called with loadUserCuts instead of loadBaseCuts

    // Event Cuts
    loadBaseCuts("primaryVertexZ","-50.","50");           // primary vertex cut
    
    // Track Cuts
    loadBaseCuts("Flag","0","2000");                      // track flag cut
    loadBaseCuts("Charge","-1","1");                      // charge cut
    loadBaseCuts("NFitPoints","15","50");                 // fit points cut
    loadBaseCuts("NFitPerNMax","0.52","1.0");             // fitpoints per possible cut
    loadBaseCuts("GlobalDCA","0.","3.0");                 // global DCA cut
    loadBaseCuts("Chi2","0.","3.0");                      // chi square cut
    loadBaseCuts("dPtByPt","0.","3.0");                   // sigma for determination of charge sign
    loadBaseCuts("Pt","0.15","15.45");                    // pt cut
    loadBaseCuts("Yt","0.1","2.");                        // yt cut
    loadBaseCuts("Phi","-1","1");                         // phi cut
    loadBaseCuts("Eta","-1.0","1.0");                     // eta cut
    loadBaseCuts("NSigmaElectron","0.0","1.5");           // num sigma electron cut

    // Pair Cuts
    loadBaseCuts("DeltaPhi","-.5","0.333");
    loadBaseCuts("DeltaEta","0.","0.15");
    loadBaseCuts("DeltaMt","-7.","0.250");
    loadBaseCuts("qInv","0.1","5.0");
    loadBaseCuts("EntranceSep","5.0","200");
    loadBaseCuts("ExitSep","0.0","600");
    loadBaseCuts("Quality","-0.5","0.75");
    loadBaseCuts("MidTpcSepLikeSign","5.","7.5");
    loadBaseCuts("MidTpcSepUnlikeSign","5.","7.5");
    
    flag = true;
  }
  // ******** end copy here ********

  //  Aya's Au-Au 130
  if (!strcmp(mcutFileName,"AuAu130")) {   // example template for adding new cut systems to the db
    /***********************************************************************************************
     * Title:  Aya's Cut System for Au-Au 130 
     * PA:  Aya Ishihara
     * fileCatalog:  n/a, both minbias and central datasets from year 2000 130 GeV
     * Notes:  Cut System used for all 130 analysis, publications include: nucl-ex/0411003,
     *            nucl-ex/0406035, nucl-ex/0408012, nucl-ex/0308033
     *         Webpages at protected/estruct/aya:  
     *            AxialCI/axialCI.html, AxialCD/detaxdphiCD.html, TransverseCI/mtxmtCI.html
     ***********************************************************************************************/

    // Event Cuts
    loadBaseCuts("primaryVertexZ","-75.","75");           // primary vertex cut

    // Track Cuts
    loadBaseCuts("Flag","0","2000");                      // track flag cut
    loadBaseCuts("Charge","-1","1");                      // charge cut
    loadBaseCuts("NFitPoints","10","50");                 // fit points cut
    loadBaseCuts("NFitPerNMax","0.52","1.0");             // fitpoints per possible cut
    loadBaseCuts("GlobalDCA","0.","3.0");                 // global DCA cut
    loadBaseCuts("Pt","0.15","2.0");                      // pt cut
    loadBaseCuts("Phi","-1","1");                         // phi cut
    loadBaseCuts("Eta","-1.3","1.3");                     // eta cut

    // Pair Cuts
    //loadBaseCuts("HBT",    "0.3","0.523","0.15","0.8");   // HBT
    //loadBaseCuts("Coulomb","0.3","0.523","0.15","0.8");   // Coulomb
    //loadBaseCuts("Merging","10","10");                    // Merging 
    //loadBaseCuts("Crossing","30","10","1.0");             // Crossing (aka Mid-TPC separation, LS & US splitting) 

    flag = true;
  }

  
  return flag;
}

//-------------------------------------------------------------------
bool StEStructCuts::loadCuts(){

  if(!isLoaded()) return false;
 
  if (loadCutDB()) {    // Search for entry in cut DB
    cout << "Using entry " << mcutFileName << " in cut DB." << endl;
    return true;
  }
  
  else {   // try to load from file 
    cout << "Loading file " << mcutFileName <<endl;
    ifstream from(mcutFileName);
    
    if(!from){ 
      cout<<" Cut file Not Found "<<endl; 
      return false;
    }
    
    bool done = false;
    
    char line[256], lineRead[256];
    char* puteol;
    char** val = new char*[100];
    int ival;
    
    while(!done) {
      if(from.eof()){
	done=true;
      } else {
	from.getline(lineRead,256);
	strcpy(line,lineRead);
	if( (line[0]=='#') )continue;
	if((puteol=strstr(line,"#")))*puteol='\0';
	ival=0;
	val[ival]=line;
      char* fcomma;
      while((fcomma=strstr(val[ival],","))){
        *fcomma='\0';
        fcomma++;
        ival++;
        val[ival]=fcomma;
      }
      if(ival<1) continue;
      const char* name=val[0];
      char** values=&val[1];
      if(!loadBaseCuts(name,(const char**)values,ival))loadUserCuts(name,(const char**)values,ival);
      }
    }

    from.close();
    delete [] val;
    
    return true;
  } // else

} 

//-------------------------------------------------------------------    
bool StEStructCuts::loadBaseCuts(const char* name,const char* val1,const char* val2,const char* val3,const char* val4) {
  // Overloaded function, takes up to 4 args and puts them in a string array for loadBaseCuts
  int count = 2;
  if (val3) count = 3;
  if (val4) count = 4;
  char** tmp = new char*[count];
  tmp[0] = new char[strlen(val1) + 1];
  tmp[1] = new char[strlen(val2) + 1];
  strcpy(tmp[0],val1);
  strcpy(tmp[1],val2);
  if (count>=3) {
    tmp[2] = new char[strlen(val3) + 1];
    strcpy(tmp[2],val3);
  }
  if (count==4) {
    tmp[3] = new char[strlen(val4) + 1];
    strcpy(tmp[3],val4);
  }
  bool retVal = loadBaseCuts(name, (const char**)tmp, count);
  delete [] tmp;
  return retVal;
}

//-------------------------------------------------------------------
void StEStructCuts::loadUserCuts(const char* name,const char* val1,const char* val2) {
  // Overloaded function
  char** tmp = new char*[2];
  tmp[0] = new char[strlen(val1) + 1];
  tmp[1] = new char[strlen(val2) + 1];
  strcpy(tmp[0],val1);
  strcpy(tmp[1],val2);
  loadUserCuts(name, (const char**)tmp, 2);
  delete [] tmp;
}

//-------------------------------------------------------------------          
int StEStructCuts::createCutHists(const char* name, float* range, int nvals){
 

  cout<<" Creating Cut Histogram for "<<name;
  cout<<" with range of cuts = ";
  for(int ii=0;ii<nvals;ii++)cout<<range[ii]<<",";
  cout<<endl;

  if(mnumVars==mMaxStore)resize();

  mvarName[mnumVars]=new char[strlen(name)+1];
  strcpy(mvarName[mnumVars],name);

  float delta=(range[1]-range[0])/2;
  float Max=range[1]+delta;
  float Min=range[0]-delta;

  StString hc; hc<<name<<"Cut";
  mvarHistsCut[mnumVars]=new TH1F((hc.str()).c_str(),(hc.str()).c_str(),200,Min,Max);

  StString hnc; hnc<<name<<"NoCut";
  mvarHistsNoCut[mnumVars]=new TH1F((hnc.str()).c_str(),(hnc.str()).c_str(),200,Min,Max);

  int retVal=mnumVars;
  mnumVars++;

  return retVal;
}


//------------------------------------------------------------------------
void StEStructCuts::addCutHists(TH1* before, TH1* after, const char* name){

  if(mnumVars==mMaxStore)resize();

  mvarHistsNoCut[mnumVars]=before;
  mvarHistsCut[mnumVars]=after;
  if(name){
   mvarName[mnumVars] = new char[strlen(name)+1];
   strcpy(mvarName[mnumVars],name);
  } else {
    mvarName[mnumVars]=new char[5];
    strcpy(mvarName[mnumVars],"none");
  } 
  mvalues[mnumVars]=-9999.;

  mnumVars++;
}

//------------------------------------------------------------------------
void StEStructCuts::fillHistogram(const char* name, float value, bool passed){

    if (!mDoFillHists) {
        return;
    }

  int i;
  for(i=0; i<mnumVars; i++)if(strstr(mvarName[i],name)) break;

  if(i==mnumVars) return;

  mvarHistsNoCut[i]->Fill(value);
  if(passed) mvarHistsCut[i]->Fill(value);

}
//------------------------------------------------------------------------
void StEStructCuts::fillHistogram(const char* name, float val1, float val2, bool passed){

    if (!mDoFillHists) {
        return;
    }

  // Add "2D" to name.
  std::string hName = name;   hName += "2D";
//  char *hName = (char *) malloc(strlen(name)+2);
//  sprintf(hName,"%s2D",name);
  int i;
  for(i=0; i<mnumVars; i++)if(strstr(mvarName[i],hName.c_str())) break;

  if(i==mnumVars) {
      return;
  }

  TAxis *x = mvarHistsNoCut[i]->GetXaxis();
  val1 = (val1 > x->GetXmin()) ? val1 : x->GetXmin()+0.5;
  val1 = (val1 < x->GetXmax()) ? val1 : x->GetXmax()-0.5;

  TAxis *y = mvarHistsNoCut[i]->GetYaxis();
  val2 = (val2 > y->GetXmin()) ? val2 : y->GetXmin()+0.5;
  val2 = (val2 < y->GetXmax()) ? val2 : y->GetXmax()-0.5;

  mvarHistsNoCut[i]->Fill(val1,val2);
  if(passed) mvarHistsCut[i]->Fill(val1,val2);

}
//------------------------------------------------------------------------
void StEStructCuts::fillHistogram(const char* name, float val1, float val2, float val3, bool passed){

    if (!mDoFillHists) {
        return;
    }

  // Add "3D" to name.
  string hName = name;   hName += "3D";
//  char *hName = (char *) malloc(strlen(name)+2);
//  sprintf(hName,"%s3D",name);
  int i;
  for(i=0; i<mnumVars; i++)if(strstr(mvarName[i],hName.c_str())) break;

  if(i==mnumVars) {
      return;
  }

  TAxis *x = mvarHistsNoCut[i]->GetXaxis();
  val1 = (val1 > x->GetXmin()) ? val1 : x->GetXmin()+0.5;
  val1 = (val1 < x->GetXmax()) ? val1 : x->GetXmax()-0.5;

  TAxis *y = mvarHistsNoCut[i]->GetYaxis();
  val2 = (val2 > y->GetXmin()) ? val2 : y->GetXmin()+0.5;
  val2 = (val2 < y->GetXmax()) ? val2 : y->GetXmax()-0.5;

  mvarHistsNoCut[i]->Fill(val1,val2);
  if(passed) mvarHistsCut[i]->Fill(val1,val2);

}

void StEStructCuts::fillHistograms(bool passed){

    if (!mDoFillHists) {
        return;
    }

  for(int i=0;i<mnumVars; i++){
    mvarHistsNoCut[i]->Fill(mvalues[i],1.0);
    if(passed)mvarHistsCut[i]->Fill(mvalues[i],1.0);
  }

}

void StEStructCuts::writeCutHists(TFile* tf){

  tf->cd();
  for(int i=0; i<mnumVars; i++)mvarHistsCut[i]->Write();
  for(int i=0; i<mnumVars; i++)mvarHistsNoCut[i]->Write(); 
}

void StEStructCuts::resize(){

    int newMax=2*mMaxStore;
    float* tmp=new float[newMax];
    memcpy(tmp,mvalues,mMaxStore*sizeof(float));
    delete [] mvalues;
    mvalues=tmp;

    char** tmpC=new char*[newMax];
    memcpy(tmpC,mvarName,mMaxStore*sizeof(char*));
    delete [] mvarName;
    mvarName=tmpC;

    TH1** tmpH = new TH1*[newMax];
    memcpy(tmpH,mvarHistsNoCut,mMaxStore*sizeof(TH1*));
    delete [] mvarHistsNoCut;
    mvarHistsNoCut=tmpH;

    tmpH = new TH1*[newMax];
    memcpy(tmpH,mvarHistsCut,mMaxStore*sizeof(TH1*));
    delete [] mvarHistsCut;
    mvarHistsCut=tmpH;

    mMaxStore=newMax;

};

void StEStructCuts::printCuts(const char* fileName){

  ofstream ofs(fileName);
  if(!ofs.is_open()) {
    cout<<" couldn't open file="<<fileName<<endl;
    return;
  }
  printCuts(ofs);
  ofs.close();
  
}

/***********************************************************************
 *
 * $Log: StEStructCuts.cxx,v $
 * Revision 1.13  2012/11/21 17:45:43  fisyak
 * add using namespace std for gcc 4.5.1
 *
 * Revision 1.12  2012/11/16 21:19:05  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.11  2012/06/11 14:35:32  fisyak
 * std namespace
 *
 * Revision 1.10  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.9  2010/09/02 21:20:08  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.8  2010/03/02 21:43:37  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.7  2008/12/02 23:35:32  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.6  2007/07/12 19:31:37  fisyak
 * use StString instead of sstream
 *
 * Revision 1.5  2006/04/04 22:05:03  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.4  2005/09/14 17:08:31  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.3  2005/02/09 23:08:44  porter
 * added method to add histograms directly instead of under
 * the control of the class. Useful for odd 2D hists that don't
 * fit the current model.
 *
 * Revision 1.2  2004/08/23 19:12:13  msd
 * Added pre-compiled cut database, minor changes to cut base class
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



