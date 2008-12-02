/**********************************************************************
 *
 * $Id: StEStructEventCuts.cxx,v 1.13 2008/12/02 23:35:33 prindle Exp $
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

void StEStructEventCuts::init() { 
    mtrgByRunPeriod=false;
    strcpy(mcutTypeName,"Event");
    initCuts();
    initNames();
    if (isLoaded()) {
        loadCuts();
    }
    badTrigger = 0;
}

void StEStructEventCuts::initCuts(){

  mtWord[0]=mtWord[1]=0;
  mpVertexZ[0]=mpVertexZ[1]=0;
  mcentrality[0]=mcentrality[1]=0;
  mZVertSep[0]=mZVertSep[1]=0;

}

void StEStructEventCuts::initNames(){

  strcpy(mtWordName.name,"triggerWord");
  strcpy(mpVertexZName.name,"primaryVertexZ");
  strcpy(mcentralityName.name,"centrality");  
  strcpy(mZVertSepName.name,"pileup");  
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
      else if (!strcmp("AuAu200GeVCentral2001",mRunPeriod)) {
        // For use with trgsetupname=productionCentral; productions P02gc,P02gd,P02ge; recommended |Vz|<25
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
      else if (!strcmp("2007ProductionMinBias",mRunPeriod)) {
	// For use with trgsetupname=2007ProductionMinBias; productions P07id; recommended |Vz|<10 (maybe 5)
	mtWord[0] = 200003;
	mtWord[1] = 200020;
	validRun = 1;
      }
      else if (!strcmp("2007LowLuminosity",mRunPeriod)) {
	// For use with trgsetupname=2007ProductionMinBias; productions P07id; recommended |Vz|<10 (maybe 5)
	mtWord[0] = 200003;
	mtWord[1] = 200020;
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
      else if (!strcmp("CuCu22GeVProductionMinBiasP05if",mRunPeriod)) {
	// ...
	mtWord[0] = 86000;  // untested. Believe this includes all triggers except
	mtWord[1] = 86033;  // zero-bias.
	validRun = 1; 
      }
      else if (!strcmp("CuCu62GeVProductionMinBias2007ic",mRunPeriod)) {
	// ...
	mtWord[0] = 76002;  // Don't actually use these do we?
	mtWord[1] = 76011;
	validRun = 1; 
      }
      else if (!strcmp("CuCu200GeVProductionMinBias2007ic",mRunPeriod)) {
	// ...
	mtWord[0] = 76000;  // Don't actually use these do we?
	mtWord[1] = 76020;
	validRun = 1; 
      }
      else if (!strcmp("dAu200GeVMinBias2003",mRunPeriod)) {
        // ...
        mtWord[0] = 2000; // untested  
        mtWord[1] = 2010;
        validRun = 1;
      }
      else if (!strcmp("ppProductionMinBias2005",mRunPeriod)) {
        // ...
        mtWord[0] =  96011; // untested  
        mtWord[1] = 106011;
        validRun = 1;
      }
      else if (!strcmp("pp400MinBias2005",mRunPeriod)) {
        // ...
        mtWord[0] =  96011; // untested  
        mtWord[1] = 106011;
        validRun = 1;
      }
      else if (!strcmp("pp2006MinBias2006",mRunPeriod)) {
        // ...
        mtWord[0] = 117001; // untested  
        mtWord[1] = 117001;
        validRun = 1;
      }
      else if (!strcmp("ppProductionMB622006",mRunPeriod)) {
        // ...
        mtWord[0] = 147001; // untested  
        mtWord[1] = 147001;
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
  
  if(!strcmp(name,mZVertSepName.name)){
    mZVertSep[0]=atoi(vals[0]); 
    mZVertSep[1]=atoi(vals[1]);
    mZVertSepName.idx=createCutHists(name,mZVertSep);
    setRange(mZVertSepName.name,mZVertSep[0],mZVertSep[1]);
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
  ofs<<mZVertSepName.name<<","<<mZVertSep[0]<<","<<mZVertSep[1]<<"\t\t\t"<<" # number events passing vertex separation pileup cuts"<<endl;
  //  ofs<<"# ******************************************** "<<endl<<endl;

}

/***********************************************************************
 *
 * $Log: StEStructEventCuts.cxx,v $
 * Revision 1.13  2008/12/02 23:35:33  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.12  2008/03/19 22:01:59  prindle
 * Updated some dataset definitions.
 *
 * Revision 1.11  2007/11/27 22:59:57  prindle
 * Added cucu22 GeV data set as possible input
 *
 * Revision 1.10  2007/11/26 19:52:23  prindle
 * Add cucu62, cucu200 2007ib production datasets.
 * Included vertex cuts for case of ranked vertices. (Pass muDst pointer to EventCuts)
 * Add n^(1/4) histograms to QAHists
 *
 * Revision 1.9  2006/10/02 22:14:08  prindle
 * Changed for QA histograms. Also addition of ppMinBiasYear5 as a data sample.
 *
 * Revision 1.8  2006/04/25 21:02:50  msd
 * Added AuAu200GeVCentral2001 and dAu200GeVMinBias2003
 *
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









