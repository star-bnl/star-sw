/**********************************************************************
 *
 * $Id: StEStructCuts.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
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
bool StEStructCuts::loadCuts(){

  if(!isLoaded()) return false;
 
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
} 
    
            
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
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/



