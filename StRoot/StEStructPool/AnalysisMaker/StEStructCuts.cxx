/**********************************************************************
 *
 * $Id: StEStructCuts.cxx,v 1.2 2004/08/23 19:12:13 msd Exp $
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
#include "StEStructCuts.h"
#include <sstream>
#include "Stiostream.h"
#include "Stsstream.h"
#include "TFile.h"
#include "TH1.h"


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

//-------------------------------------------------------------------
void StEStructCuts::initVars(){

  mvarName = new char*[mMaxStore];
  mvalues  = new float[mMaxStore];
  mvarHistsNoCut = new TH1F*[mMaxStore];
  mvarHistsCut   = new TH1F*[mMaxStore];
  mnumVars=0;
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

  if (!strcmp(mcutFileName,"test-1")) {
    // Some crazy cuts for testing... 
    loadBaseCuts("primaryVertexZ","-15","25");        
    loadBaseCuts("Pt","0.15","5");                    
    loadBaseCuts("Phi","-1","0.8");                   
    loadBaseCuts("Eta","-1.5","0.5");                 
    
    flag = true;
  }

  // ***** begin copy here ******* 
  if (!strcmp(mcutFileName,"template")) {   // example template for adding new cuts 
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
    // Note: some cuts (names begin with "good") need to be called with loadUserCuts instead of loadBaseCuts

    // Event Cuts
    loadBaseCuts("primaryVertexZ","-25.","25");           // primary vertex cut
    
    // Track Cuts
    loadBaseCuts("Flag","0","2000");                      // track flag cut
    loadBaseCuts("Charge","-1","1");                      // charge cut
    loadBaseCuts("NFitPoints","15","50");                 // fit points cut
    loadBaseCuts("NFitPerNMax","0.52","1.0");             // fitpoints per possible cut
    loadBaseCuts("GlobalDCA","0.","3.0");                 // global DCA cut
    loadBaseCuts("Chi2","0.","3.0");                      // chi square cut
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
  // ******** end copy here *******
  
  //if (!flag) cout << "No entry for " << mcutFileName << " found in database.  Looking for file..." << endl;    
  
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
bool StEStructCuts::loadBaseCuts(const char* name,const char* val1,const char* val2) {
  // Overloaded function
  char** tmp = new char*[2];
  tmp[0] = new char[strlen(val1) + 1];
  tmp[1] = new char[strlen(val2) + 1];
  strcpy(tmp[0],val1);
  strcpy(tmp[1],val2);
  if(loadBaseCuts(name, (const char**)tmp, 2)) return true;
  else return false;
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

  ostringstream hc; hc<<name<<"Cut";
  mvarHistsCut[mnumVars]=new TH1F((hc.str()).c_str(),(hc.str()).c_str(),200,Min,Max);

  ostringstream hnc; hnc<<name<<"NoCut";
  mvarHistsNoCut[mnumVars]=new TH1F((hnc.str()).c_str(),(hnc.str()).c_str(),200,Min,Max);

  int retVal=mnumVars;
  mnumVars++;

  return retVal;
}

void StEStructCuts::fillHistogram(const char* name, float value, bool passed){

  int i;
  for(i=0; i<mnumVars; i++)if(strstr(mvarName[i],name)) break;

  if(i==mnumVars) return;

  mvarHistsNoCut[i]->Fill(value);
  if(passed) mvarHistsCut[i]->Fill(value);

}

void StEStructCuts::fillHistograms(bool passed){

  for(int i=0;i<mnumVars; i++){
    mvarHistsNoCut[i]->Fill(mvalues[i]);
    if(passed)mvarHistsCut[i]->Fill(mvalues[i]);
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

    TH1F** tmpH = new TH1F*[newMax];
    memcpy(tmpH,mvarHistsNoCut,mMaxStore*sizeof(TH1F*));
    delete [] mvarHistsNoCut;
    mvarHistsNoCut=tmpH;

    tmpH = new TH1F*[newMax];
    memcpy(tmpH,mvarHistsCut,mMaxStore*sizeof(TH1F*));
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
 * Revision 1.2  2004/08/23 19:12:13  msd
 * Added pre-compiled cut database, minor changes to cut base class
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



