/**********************************************************************
 *
 * $Id: StEStructEventCuts.cxx,v 1.4 2005/09/14 17:08:33 msd Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for event level quantities
 *
 *
 ***********************************************************************/
#include "StEStructEventCuts.h"
#include "Stsstream.h"
#include <stdlib.h>

ClassImp(StEStructEventCuts)

StEStructEventCuts::StEStructEventCuts(): StEStructCuts(){ init(); };
StEStructEventCuts::StEStructEventCuts(const char* cutfileName): StEStructCuts(cutfileName) { init(); };

StEStructEventCuts::~StEStructEventCuts() {};

void StEStructEventCuts::init(){ 

  initCuts();
  initNames();
  if(isLoaded())loadCuts();

}

void StEStructEventCuts::initCuts(){

  mtWord[0]=mtWord[1]=0;
  mpVertexZ[0]=mpVertexZ[1]=0;
  mcentrality[0]=mcentrality[1]=0;
  mnumTracks[0]=mnumTracks[1]=0;

}

void StEStructEventCuts::initNames(){

  strcpy(mtWordName.name,"triggerWord");
  strcpy(mpVertexZName.name,"primaryVertexZ");
  strcpy(mcentralityName.name,"centrality");
  strcpy(mnumTracksName.name,"numTracks");  
}

bool StEStructEventCuts::loadBaseCuts(const char* name, const char** vals, int nvals){
  
  if (!strcmp(name,mtWordName.name)) {
    if (1 == nvals) {  
      sscanf(vals[0],"%s\t",mRunPeriod);
      //mtWordName.idx = 0;
      // check for valid name and create hists
      bool validRun = 0;
      if (!strcmp("AuAu200GeVMinBias2001",mRunPeriod)) {
	// For use with trgsetupname=ProductionMinBias; productions P02gc,P02gd,P02ge; recommended |Vz|<25
	mtWord[0] = 4000;  
	mtWord[1] = 4500;
	validRun = 1;
      }
      else if (!strcmp("AuAu200GeVMinBias2004",mRunPeriod)) {
	// trgsetupname=productionMinBias; production P05ia; recommended |Vz|<30 (15007), -10<Vz<50 (15003)
	//mtWord[0] = 15000;  mtWord[1] = 15010;  
	mtWord[0] = 0;  
	mtWord[1] = 15;  //for some reason, the l0triggerWord in the MuDST is not the trigger ID       
	validRun = 1;
      }
      else if (!strcmp("AuAu62GeVMinBias2004",mRunPeriod)) {
	// trgsetupname=production62Gev; productions P04id,P04ie,P05ic; recommended |Vz|<30
	mtWord[0] = 35000;
	mtWord[1] = 35020;
	validRun = 1;
      }
      else if (!strcmp("CuCu200GeVProductionMinBias2005",mRunPeriod)) {
	// ...
	mtWord[0] = 86000;  // untested
	mtWord[1] = 86020;
	validRun = 1; 
      }	
      else if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
	// ...
	mtWord[0] = 76000;  // untested
	mtWord[1] = 76020;
	validRun = 1; 
      }
      if (validRun) mtWordName.idx=createCutHists(name,mtWord);
      else cout << "  Warning: unknown run period " << name << endl;      
    } else {
      mtWord[0]=(unsigned int)atoi(vals[0]); 
      mtWord[1]=(unsigned int)atoi(vals[1]);
      if(mtWord[0]==mtWord[1]){ // histogram needs some help in this case.
	float arange[2];  
	arange[0]=mtWord[0]-0.05*mtWord[0];
	arange[1]=mtWord[0]+0.05*mtWord[0];
	mtWordName.idx=createCutHists(name,arange);
      } else {
	mtWordName.idx=createCutHists(name,mtWord);
      }
      return true;
    }
  }
    
  if(!strcmp(name,mpVertexZName.name)){
    mpVertexZ[0]=atof(vals[0]); 
    mpVertexZ[1]=atof(vals[1]);
    mpVertexZName.idx=createCutHists(name,mpVertexZ);
    return true;
  }    
    
  if(!strcmp(name,mcentralityName.name)){
    mcentrality[0]=(unsigned int)atoi(vals[0]);
    mcentrality[1]=(unsigned int)atoi(vals[1]);
    mcentralityName.idx=createCutHists(name,mcentrality);
    return true;
  }    

  if(!strcmp(name,mnumTracksName.name)){
    mnumTracks[0]=atoi(vals[0]); 
    mnumTracks[1]=atoi(vals[1]);
    mnumTracksName.idx=createCutHists(name,mnumTracks);
    return true;
  }    
    
    
  return false;
    
};


void StEStructEventCuts::printCuts(ostream& ofs){

  ofs<<"# ******************************************** "<<endl;
  ofs<<"# *************** Event Cuts ***************** "<<endl;
  ofs<<"# *** format = variable,minvalue,maxvalue  *** "<<endl;
  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  ofs<<mtWordName.name<<","<<mtWord[0]<<","<<mtWord[1]<<"\t\t\t"<<" # triggerWord cut"<<endl;
  ofs<<mpVertexZName.name<<","<<mpVertexZ[0]<<","<<mpVertexZ[1]<<"\t\t"<<" # primary vertex cut"<<endl;
  ofs<<mcentralityName.name<<","<<mcentrality[0]<<","<<mcentrality[1]<<"\t\t\t"<<" # centrality cut"<<endl;
  ofs<<mnumTracksName.name<<","<<mnumTracks[0]<<","<<mnumTracks[1]<<"\t\t\t"<<" # number of tracks per event passing track cuts"<<endl;
  ofs<<"# ******************************************** "<<endl<<endl;

}

/***********************************************************************
 *
 * $Log: StEStructEventCuts.cxx,v $
 * Revision 1.4  2005/09/14 17:08:33  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.3  2004/09/24 01:41:41  prindle
 * Allow for cuts to be defined by character strings. I use this to select trigger
 * cuts appropriate for run periods
 *
 * Revision 1.2  2004/04/15 18:45:35  msd
 * Removed hard-wired range of numTrack cut histograms
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/









