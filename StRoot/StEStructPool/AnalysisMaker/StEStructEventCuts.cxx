/**********************************************************************
 *
 * $Id: StEStructEventCuts.cxx,v 1.2 2004/04/15 18:45:35 msd Exp $
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

  if(!strcmp(name,mtWordName.name)){ 
    mtWord[0]=(unsigned int)atoi(vals[0]); 
    mtWord[1]=(unsigned int)atoi(vals[1]);
    if(mtWord[0]==mtWord[1]){ // historgam needs some help in this case.
      float arange[2];
      arange[0]=mtWord[0]-0.05*mtWord[0];
      arange[1]=mtWord[0]+0.05*mtWord[0];
      mtWordName.idx=createCutHists(name,arange);
    } else {
      mtWordName.idx=createCutHists(name,mtWord);
    }
    return true;
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
 * Revision 1.2  2004/04/15 18:45:35  msd
 * Removed hard-wired range of numTrack cut histograms
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/









