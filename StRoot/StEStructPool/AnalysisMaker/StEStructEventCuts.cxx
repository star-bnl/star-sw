/**********************************************************************
 *
 * $Id: StEStructEventCuts.cxx,v 1.7 2006/04/10 23:40:40 porter Exp $
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

  mtrgByRunPeriod=false;
  strcpy(mcutTypeName,"Event");
  initCuts();
  initNames();
  if(isLoaded())loadCuts();

}

void StEStructEventCuts::initCuts(){

  mtWord[0]=mtWord[1]=0;
  mpVertexZ[0]=mpVertexZ[1]=0;
  mcentrality[0]=mcentrality[1]=0;

}

void StEStructEventCuts::initNames(){

  strcpy(mtWordName.name,"triggerWord");
  strcpy(mpVertexZName.name,"primaryVertexZ");
  strcpy(mcentralityName.name,"centrality");  
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
	mtWord[0] = 66000;  // untested
	mtWord[1] = 66020;
	validRun = 1; 
      }	
      else if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
	// ...
	mtWord[0] = 76000;  // untested
	mtWord[1] = 76020;
	validRun = 1; 
      }
      if (validRun) {
         mtWordName.idx=createCutHists(name,mtWord);
         mtrgByRunPeriod=true;
      }
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
    }
    setRange(mtWordName.name,mtWord[0],mtWord[1]);
    return true;
  }
    
  if(!strcmp(name,mpVertexZName.name)){
    mpVertexZ[0]=atof(vals[0]); 
    mpVertexZ[1]=atof(vals[1]);
    mpVertexZName.idx=createCutHists(name,mpVertexZ);
    setRange(mpVertexZName.name,mpVertexZ[0],mpVertexZ[1]);
    return true;
  }    
    
  if(!strcmp(name,mcentralityName.name)){
    mcentrality[0]=atoi(vals[0]); 
    mcentrality[1]=atoi(vals[1]);
    mcentralityName.idx=createCutHists(name,mcentrality);
    setRange(mcentralityName.name,mcentrality[0],mcentrality[1]);
    return true;
  }    
    
    
  return false;
    
};


void StEStructEventCuts::printCutStats(ostream& ofs){

  //  ofs<<"# ******************************************** "<<endl;
  //  ofs<<"# *************** Event Cuts ***************** "<<endl;
  //  ofs<<"# *** format = variable,minvalue,maxvalue  *** "<<endl;
  //  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  ofs<<mtWordName.name<<","<<mtWord[0]<<","<<mtWord[1]<<"\t\t\t"<<" # triggerWord cut"<<endl;
  ofs<<mpVertexZName.name<<","<<mpVertexZ[0]<<","<<mpVertexZ[1]<<"\t\t"<<" # primary vertex cut"<<endl;
  ofs<<mcentralityName.name<<","<<mcentrality[0]<<","<<mcentrality[1]<<"\t\t\t"<<" # number events passing centrality cuts"<<endl;
  //  ofs<<"# ******************************************** "<<endl<<endl;

}

/***********************************************************************
 *
 * $Log: StEStructEventCuts.cxx,v $
 * Revision 1.7  2006/04/10 23:40:40  porter
 * Fixed the minbias trigger definition for the CuCu 200 2005 run
 *
 * Revision 1.6  2006/04/04 22:05:05  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2006/02/22 22:03:17  prindle
 * Removed all references to multRef
 *
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









