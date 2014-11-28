/**********************************************************************
 *
 * $Id: StEStructEventCuts.cxx,v 1.19 2012/11/16 21:19:06 prindle Exp $
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
#include <string>

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
  mpVPDVertex[0]=mpVPDVertex[1]=0;
  mpVertexRadius[0]=mpVertexRadius[1]=0;
  mcentrality[0]=mcentrality[1]=0;
  mgoodtoffraction[0]=mgoodtoffraction[1]=0;
  mgoodprimaryfraction[0]=mgoodprimaryfraction[1]=0;
  mZVertSep[0]=mZVertSep[1]=0;
  mZVertMatch[0]=mZVertMatch[1]=0;

}

void StEStructEventCuts::initNames(){

  strcpy(mtWordName.name,"triggerWord");
  strcpy(mpVertexZName.name,"primaryVertexZ");
  strcpy(mpVPDVertexName.name,"primaryVPDVertex");
  strcpy(mpVertexRadiusName.name,"primaryVertexRadius");
  strcpy(mcentralityName.name,"centrality");  
  strcpy(mgoodtoffractionName.name,"goodToFFraction");  
  strcpy(mgoodprimaryfractionName.name,"goodPrimaryFraction");  
  strcpy(mZVertSepName.name,"pileup");  
  strcpy(mZVertMatchName.name,"singleSideVertex");  
}

bool StEStructEventCuts::loadBaseCuts(const char* name, const char** vals, int nvals){

    if (!strcmp(name,mtWordName.name)) {
        if (1 == nvals) {  
            sscanf(vals[0],"%s\t",mRunPeriod);
            // check for valid name and define limits for histograms
            // For named triggerWord mtWord is used to define histogram ranges.
            bool validRun = 0;
            if (!strcmp("AuAu200GeVMinBias2001",mRunPeriod)) {
                // For use with trgsetupname=ProductionMinBias; productions P02gc,P02gd,P02ge; recommended |Vz|<25
                mtWord[0] = 4090;  
                mtWord[1] = 4140;
                validRun = 1;
            } else if (!strcmp("AuAu200GeVCentral2001",mRunPeriod)) {
                // For use with trgsetupname=productionCentral; productions P02gc,P02gd,P02ge; recommended |Vz|<25
                mtWord[0] = 4000;
                mtWord[1] = 4500;
                validRun = 1;
            } else if (!strcmp("AuAu200GeVProductionMinBias2004",mRunPeriod)  ||
                       !strcmp("AuAu200GeVProductionMinBiasA2004",mRunPeriod) ||
                       !strcmp("AuAu200GeVProductionMinBiasB2004",mRunPeriod)) {
               // trgsetupname=productionMinBias; production P05ic;
               // For period A the trigger page recommends -10 < Vz <30
               // I think we are better off always using -15 < Vz < 15
               //mtWord[0] = 15000;  mtWord[1] = 15010;  
               mtWord[0] = 15000; 
               mtWord[1] = 15050;
               validRun = 1;
            } else if (!strcmp("AuAu200GeVProductionLow2004",mRunPeriod)) {
               // trgsetupname=productionMinBias; production P05ia; recommended |Vz|<30
               //mtWord[0] = 15000;  mtWord[1] = 15010;  
               mtWord[0] = 15000; 
               mtWord[1] = 15050;
               validRun = 1;
            } else if (!strcmp("AuAu200GeVProductionMid2004",mRunPeriod)) {
               // trgsetupname=productionMinBias; production P05ia; recommended |Vz|<30
               //mtWord[0] = 15000;  mtWord[1] = 15010;  
               mtWord[0] = 15000; 
               mtWord[1] = 15050;
               validRun = 1;
            } else if (!strcmp("AuAu200GeVProductionHigh2004",mRunPeriod)) {
               // trgsetupname=productionMinBias; production P05ia; recommended |Vz|<30
               //mtWord[0] = 15000;  mtWord[1] = 15010;  
               mtWord[0] = 15000; 
               mtWord[1] = 15050;
               validRun = 1;
            } else if (!strcmp("AuAu200GeVMinBias2010",mRunPeriod)) {
               // trgsetupname=AuAu200_production
               mtWord[0] = 260000; 
               mtWord[1] = 260500;
               validRun = 1;
            } else if (!strcmp("AuAu200GeVMinBias2011",mRunPeriod)) {
               // trgsetupname=AuAu200_production
               mtWord[0] = 350000; 
               mtWord[1] = 350500;
               validRun = 1;
            } else if (!strcmp("2007ProductionMinBias",mRunPeriod)) {
               // For use with trgsetupname=2007ProductionMinBias; productions P08ic; recommended |Vz|<10 (maybe 5)
               mtWord[0] = 200000;
               mtWord[1] = 200050;
               validRun = 1;
            } else if (!strcmp("2007Production2",mRunPeriod)) {
               // For use with trgsetupname=2007Production2; productions P08ic; recommended |Vz|<10 (maybe 5)
               mtWord[0] = 200000;
               mtWord[1] = 200050;
               validRun = 1;
            } else if (!strcmp("2007LowLuminosity",mRunPeriod)) {
               // For use with trgsetupname=2007ProductionMinBias; productions P07id; recommended |Vz|<10 (maybe 5)
               mtWord[0] = 200000;
               mtWord[1] = 200050;
               validRun = 1;
            } else if (!strcmp("AuAu62GeVMinBias2010",mRunPeriod)) {
               // trgsetupname=AuAu62_production
               mtWord[0] = 270000;
               mtWord[1] = 270050;
               validRun = 1;
            } else if (!strcmp("AuAu62GeVMinBias2004",mRunPeriod)) {
               // trgsetupname=production62Gev; productions P04id,P04ie,P05ic; recommended |Vz|<30
               mtWord[0] = 35000;
               mtWord[1] = 35050;
               validRun = 1;
            } else if (!strcmp("AuAu39GeVMinBias2010",mRunPeriod)) {
               // trgsetupname=AuAu30_production; productions P10ih; Not sure about recommended V_z cut yet.
               mtWord[0] = 28000;
               mtWord[1] = 28050;
               validRun = 1;
            } else if (!strcmp("AuAu27GeVMinBias2011",mRunPeriod)) {
               // trgsetupname=AuAu27_production
               mtWord[0] = 360000;
               mtWord[1] = 360050;
               validRun = 1;
            } else if (!strcmp("AuAu19GeVMinBias2011",mRunPeriod)) {
               // trgsetupname=AuAu19_production
               mtWord[0] = 340000;
               mtWord[1] = 340050;
               validRun = 1;
            } else if (!strcmp("AuAu11GeVMinBias2010",mRunPeriod)) {
               // trgsetupname=AuAu11_production; productions P10ih; Not sure about recommended V_z cut yet.
               // Not sure I have the right triggerid.
               mtWord[0] = 31000;
               mtWord[1] = 31050;
               validRun = 1;
            } else if (!strcmp("AuAu7GeVMinBias2010",mRunPeriod)) {
               // trgsetupname=AuAu7_production; productions P10ih; Not sure about recommended V_z cut yet.
               mtWord[0] = 29000;
               mtWord[1] = 29050;
               validRun = 1;
            } else if (!strcmp("CuCu200GeVProductionMinBias2005",mRunPeriod)) {
               // ...
               mtWord[0] = 66000;  // untested
               mtWord[1] = 66050;
               validRun = 1;
            } else if (!strcmp("CuCu200GeVProductionMinBias2007ic",mRunPeriod)) {
               // ...
               mtWord[0] = 76000;
               mtWord[1] = 76050;
               validRun = 1; 
            } else if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
               // ...
               mtWord[0] = 76000;  // untested
               mtWord[1] = 76050;
               validRun = 1;
            } else if (!strcmp("CuCu62GeVProductionMinBias2007ic",mRunPeriod)) {
               // ...
               mtWord[0] = 76002;
               mtWord[1] = 76011;
               validRun = 1; 
            } else if (!strcmp("CuCu22GeVProductionMinBiasP05if",mRunPeriod)) {
               // ...
               mtWord[0] = 86000;  // untested. Believe this includes all triggers except
               mtWord[1] = 86050;  // zero-bias.
               validRun = 1; 
            } else if (!strcmp("dAu200GeVMinBias2003",mRunPeriod)) {
               // ...
              mtWord[0] = 2000; // untested  
              mtWord[1] = 2050;
              validRun = 1;
            } else if (!strcmp("ppProductionMinBias2005",mRunPeriod)) {
              // ...
              mtWord[0] =  96011; // untested  
              mtWord[1] = 106011;
              validRun = 1;
            } else if (!strcmp("pp400MinBias2005",mRunPeriod)) {
                // ...
                mtWord[0] =  96011; // untested  
                mtWord[1] = 106011;
                validRun = 1;
            } else if (!strcmp("pp2006MinBias2006",mRunPeriod)) {
                // ...
                mtWord[0] = 117000; // untested  
                mtWord[1] = 117050;
                validRun = 1;
            } else if (!strcmp("pp2pp_VPDMB_2009",mRunPeriod)) {
                // ...
                mtWord[0] = 250100; // untested  
                mtWord[1] = 250150;
                validRun = 1;
            } else if (!strcmp("ppProductionMB622006",mRunPeriod)) {
                // ...
                mtWord[0] = 147000; // untested  
                mtWord[1] = 147050;
                validRun = 1;
            } else if (!strcmp("production2009_200GeV_Single",mRunPeriod)) {
                // ...
                mtWord[0] = 240000; // untested  
                mtWord[1] = 240050;
                validRun = 1;
            }
            if (validRun) {
                string hName;
                hName = mtWordName.name;  hName += "NoCut";
                TH1F *noCutHist = new TH1F(hName.c_str(),hName.c_str(),mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                hName = mtWordName.name;  hName += "Cut";
                TH1F *cutHist = new TH1F(hName.c_str(),hName.c_str(),mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                addCutHists(noCutHist,cutHist,mtWordName.name);
                hName = mtWordName.name;  hName += "NoCut2D";
                TH2F *noCutHist2D = new TH2F(hName.c_str(),hName.c_str(),mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                hName = mtWordName.name;  hName += "Cut2D";
                TH2F *cutHist2D = new TH2F(hName.c_str(),hName.c_str(),mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                hName = mtWordName.name;  hName += "2D";
                addCutHists(noCutHist2D,cutHist2D,hName.c_str());
/*
                char *hName;
                hName = new char[strlen(mtWordName.name)+5];
                sprintf(hName,"%sNoCut",mtWordName.name);
                TH1F *noCutHist = new TH1F(hName,hName,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                delete [] hName;
                hName = new char[strlen(mtWordName.name)+3];
                sprintf(hName,"%sCut",mtWordName.name);
                TH1F *cutHist = new TH1F(hName,hName,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                addCutHists(noCutHist,cutHist,mtWordName.name);
                delete [] hName;
                hName = new char[strlen(mtWordName.name)+7];
                sprintf(hName,"%sNoCut2D",mtWordName.name);
                TH2F *noCutHist2D = new TH2F(hName,hName,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                delete [] hName;
                hName = new char[strlen(mtWordName.name)+5];
                sprintf(hName,"%sCut2D",mtWordName.name);
                TH2F *cutHist2D = new TH2F(hName,hName,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5,mtWord[1]-mtWord[0]+3,mtWord[0]-1.5,mtWord[1]+1.5);
                delete [] hName;
                hName = new char[strlen(mtWordName.name)+2];
                sprintf(hName,"%s2D",mtWordName.name);
                addCutHists(noCutHist2D,cutHist2D,hName);
                delete [] hName;
 */
                mtrgByRunPeriod=true;
            } else {
                cout << "  Warning: unknown run period " << name << endl;
            }
        } else {
            mtWord[0]=(unsigned int)atoi(vals[0]); 
            mtWord[1]=(unsigned int)atoi(vals[1]);
            if (mtWord[0]==mtWord[1]) { // histogram needs some help in this case.
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

    if (!strcmp(name,mpVertexZName.name)) {
        mpVertexZ[0]=atof(vals[0]); 
        mpVertexZ[1]=atof(vals[1]);
        mpVertexZName.idx=createCutHists(name,mpVertexZ);
        setRange(mpVertexZName.name,mpVertexZ[0],mpVertexZ[1]);
        return true;
    }

    if (!strcmp(name,mpVPDVertexName.name)) {
        mpVPDVertex[0]=atof(vals[0]); 
        mpVPDVertex[1]=atof(vals[1]);
        mpVPDVertexName.idx=createCutHists(name,mpVPDVertex);
        setRange(mpVPDVertexName.name,mpVPDVertex[0],mpVPDVertex[1]);
        return true;
    }

    if (!strcmp(name,mpVertexRadiusName.name)) {
        mpVertexRadius[0]=atof(vals[0]); 
        mpVertexRadius[1]=atof(vals[1]);
        mpVertexRadiusName.idx=createCutHists(name,mpVertexRadius);
        setRange(mpVertexRadiusName.name,mpVertexRadius[0],mpVertexRadius[1]);
        return true;
    }

    if (!strcmp(name,mcentralityName.name)) {
        mcentrality[0]=atoi(vals[0]); 
        mcentrality[1]=atoi(vals[1]);
        mcentralityName.idx=createCutHists(name,mcentrality);
        setRange(mcentralityName.name,mcentrality[0],mcentrality[1]);
        return true;
    }

    if (!strcmp(name,mgoodtoffractionName.name)) {
        mgoodtoffraction[0]=atof(vals[0]); 
        mgoodtoffraction[1]=atof(vals[1]);
        mgoodtoffractionName.idx=createCutHists(name,mgoodtoffraction);
        setRange(mgoodtoffractionName.name,mgoodtoffraction[0],mgoodtoffraction[1]);
        return true;
    }

    if (!strcmp(name,mgoodprimaryfractionName.name)) {
        mgoodprimaryfraction[0]=atof(vals[0]); 
        mgoodprimaryfraction[1]=atof(vals[1]);
        mgoodprimaryfractionName.idx=createCutHists(name,mgoodprimaryfraction);
        setRange(mgoodprimaryfractionName.name,mgoodprimaryfraction[0],mgoodprimaryfraction[1]);
        return true;
    }

    if (!strcmp(name,mZVertSepName.name)) {
        mZVertSep[0]=atoi(vals[0]); 
        mZVertSep[1]=atoi(vals[1]);
        mZVertSepName.idx=createCutHists(name,mZVertSep);
        setRange(mZVertSepName.name,mZVertSep[0],mZVertSep[1]);
        return true;
    }

    if (!strcmp(name,mZVertMatchName.name)) {
        mZVertMatch[0]=atoi(vals[0]); 
        mZVertMatch[1]=atoi(vals[1]);
        mZVertMatchName.idx=createCutHists(name,mZVertMatch);
        setRange(mZVertMatchName.name,mZVertMatch[0],mZVertMatch[1]);
        return true;
    }

    return false;

};

bool StEStructEventCuts::goodTrigger(StMuDst* muDst) {

    StMuEvent* muEvent = muDst->event();
    if (mtrgByRunPeriod) {

        if (!strcmp("CuCu22GeVProductionMinBiasP05if",mRunPeriod)) {
            // Accept all triggers.
            return true;
        } else if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(76007) ||
                muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
                return true;
            }
        } else if (!strcmp("CuCu200GeVProductionMinBias2005",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(66007)) {
                return true;
            }
        } else if (!strcmp("CuCu62GeVProductionMinBias2007ic",mRunPeriod)) {
            // Assume vertex characteristics essentially the same
            // as the 200GeV CuCu data.
            if (muEvent->runNumber()<6069077) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(76002) ||
                    muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
                    return goodVertexTopology(muDst);
                }
            } else {
                if (muEvent->triggerIdCollection().nominal().isTrigger(76007) ||
                    muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
                    return goodVertexTopology(muDst);
                }
            }
        } else if (!strcmp("CuCu200GeVProductionMinBias2007ic",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(66007)) {
                return goodVertexTopology(muDst);
            }
        } else if (!strcmp("2007ProductionMinBias",mRunPeriod)) {
            // This is 200GeV ProductionMinBias AuAu data from 2007
            // First triggerId has killer bits on, second killer bits off.
            // I don't know what killer bits are. Are they important?
            // Trigger page suggests ID 200001 should be in mbvpd.
            // Multiplicity distribution of that id is cleary wrong.
            if (muEvent->triggerIdCollection().nominal().isTrigger(200001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200003) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200020)) {
                badTrigger++;
            }
            if (muEvent->triggerIdCollection().nominal().isTrigger(200013)) {
                return goodVertexTopology(muDst);
            }
        } else if (!strcmp("2007Production2",mRunPeriod)) {
            // This is 200GeV Production2 AuAu data from 2007
            // I think this trigger had the same trigger bits as 2007ProductionMinBias but
            // was active during different runs. May be an issue of pre-scaling or splittin
            // in express streams.
            if (muEvent->triggerIdCollection().nominal().isTrigger(200001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200003) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200020)) {
                badTrigger++;
            }
            if (muEvent->triggerIdCollection().nominal().isTrigger(200013)) {
                return goodVertexTopology(muDst);
            }
        } else if (!strcmp("2007LowLuminosity",mRunPeriod)) {
            // This is 200GeV LowLuminosity AuAu data from 2007
            if (muEvent->triggerIdCollection().nominal().isTrigger(200001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200003) ||
                muEvent->triggerIdCollection().nominal().isTrigger(200013)) {
                badTrigger++;
            }
            if (muEvent->triggerIdCollection().nominal().isTrigger(200020)) {
                return goodVertexTopology(muDst);
            }
        } else if (!strcmp("AuAu200GeVProductionMinBias2004",mRunPeriod)) {
            if (muEvent->runNumber() < 5023099) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15003)) {
                    return true;
                }
            } else if (muEvent->runNumber() > 503098) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15007)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVProductionMinBiasA2004",mRunPeriod)) {
            if (muEvent->runNumber() < 5023099) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15003)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVProductionMinBiasB2004",mRunPeriod)) {
            if (muEvent->runNumber() > 503098) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15007)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVProductionLow2004",mRunPeriod)) {
            if (muEvent->runNumber() > 5042040) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15007)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVProductionMid2004",mRunPeriod)) {
            if (muEvent->runNumber() > 5042040) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15007)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVProductionHigh2004",mRunPeriod)) {
            if (muEvent->runNumber() > 5042040) {
                if (muEvent->triggerIdCollection().nominal().isTrigger(15007)) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVMinBias2010",mRunPeriod)) {
            // I think all these vpd-mb triggers were for different parts of the run.
            // I don't know why they have different numbers though.
            if (muEvent->triggerIdCollection().nominal().isTrigger(260001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(260011) ||
                muEvent->triggerIdCollection().nominal().isTrigger(260021) ||
                muEvent->triggerIdCollection().nominal().isTrigger(260031)) {
                return true;
//                return goodVertexTopology(muDst);
            }
if (0) {
            // The triggers ending in 4 are called vpd-mb-slow
            // Don't know what 9 is, must be during setup.
            if (muEvent->triggerIdCollection().nominal().isTrigger(9)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260001)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260011)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260014)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260021)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260024)) {
                return true;
            } else if (muEvent->triggerIdCollection().nominal().isTrigger(260031)) {
                return true;
            }
}
        } else if (!strcmp("AuAu200GeVMinBias2011",mRunPeriod)) {
            // These vpd-mb triggers were for different run numbers, use catalog query if you want specific trigger.
            // I don't know why they have different numbers though.
            if (muEvent->triggerIdCollection().nominal().isTrigger(350003) ||
                muEvent->triggerIdCollection().nominal().isTrigger(350013) ||
                muEvent->triggerIdCollection().nominal().isTrigger(350023) ||
                muEvent->triggerIdCollection().nominal().isTrigger(350033) ||
                muEvent->triggerIdCollection().nominal().isTrigger(350043)) {
                return true;
            }
        } else if (!strcmp("AuAu62GeVMinBias2010",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(270001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(270011) ||
                muEvent->triggerIdCollection().nominal().isTrigger(270021)) {
                return true;
            }
        } else if (!strcmp("AuAu62GeVMinBias2004",mRunPeriod)) {
            if (((muEvent->triggerIdCollection().nominal().isTrigger(35004) ||
                  muEvent->triggerIdCollection().nominal().isTrigger(35007))
                ||
                ((muEvent->triggerIdCollection().nominal().isTrigger(35001) ||
                  muEvent->triggerIdCollection().nominal().isTrigger(35009) )
                  && muEvent->ctbMultiplicity()>15))) {
                return true;
            }
        } else if (!strcmp("AuAu39GeVMinBias2010",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(280001)) {
                return true;
            }
        } else if (!strcmp("AuAu27GeVMinBias2011",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(360001)) {
                return true;
            }
        } else if (!strcmp("AuAu19GeVMinBias2011",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(340001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(340011) ||
                muEvent->triggerIdCollection().nominal().isTrigger(340021)) {
                return true;
            }
        } else if (!strcmp("AuAu11GeVMinBias2010",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(310014)) {
                return true;
            }
        } else if (!strcmp("AuAu7GeVMinBias2010",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(290001) ||
                muEvent->triggerIdCollection().nominal().isTrigger(290004)) {
                return true;
            }
        } else if (!strcmp("AuAu200GeVMinBias2001",mRunPeriod)) {
            StMuL3EventSummary l3 = muEvent->l3EventSummary();
            if (l3.unbiasedTrigger()) {
                unsigned int t = muEvent->l0Trigger().triggerWord();
                if ( 0x1000 == t ) {
                    return true;
                }
            }
        } else if (!strcmp("AuAu200GeVCentral2001",mRunPeriod)) {
            StMuL3EventSummary l3 = muEvent->l3EventSummary();
            if (l3.unbiasedTrigger()) {
    	        unsigned int t = muEvent->l0Trigger().triggerWord();
                if ( 0x1100 == t ) {
    	            return true;
                }
           }
        } else if (!strcmp("dAu200GeVMinBias2003",mRunPeriod)) {
            if (muEvent->triggerIdCollection().nominal().isTrigger(2001) ||
    	        muEvent->triggerIdCollection().nominal().isTrigger(2003)) {
                return true;
            }
        } else if(!strcmp("ppMinBias",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(8192)) {
                return true;
            }
        } else if(!strcmp("ppProductionMinBias2005",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(96011) ||
               muEvent->triggerIdCollection().nominal().isTrigger(106011)) {
                   return true;
            }
        } else if(!strcmp("pp400MinBias2005",mRunPeriod)){
            // Not 100% sure of these triggers. Small data set, probably not worth spending much time on.
            if(muEvent->triggerIdCollection().nominal().isTrigger(96011) ||
               muEvent->triggerIdCollection().nominal().isTrigger(106011)) {
                   return true;
            }
        } else if(!strcmp("pp2006MinBias2006",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(117001)) {
                   return true;
            }
        } else if(!strcmp("pp2pp_VPDMB_2009",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(7) ||
               muEvent->triggerIdCollection().nominal().isTrigger(250107)) {
                   return true;
            }
        } else if(!strcmp("production2009_200GeV_Single",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(240025) &&
               muDst->primaryVertex()->ranking()>0) {
                   return true;
            }
        } else if(!strcmp("ppProductionMB622006",mRunPeriod)){
            if(muEvent->triggerIdCollection().nominal().isTrigger(147001)) {
                // Not completely sure this is an optimal cut for reducing pileup.
                if (muDst->primaryVertex()->ranking()>0) {
                    return true;
                }
            }
        }
    } else {
        unsigned int t = muEvent->l0Trigger().triggerWord();
        mvalues[mtWordName.idx] = (float)t;
        return ( (mtWord[0]==mtWord[1] && mtWord[0]==0) ||
                 (t>=mtWord[0] && t<=mtWord[1])  ) ;
    }
    return false;
}
// Only early runs have ctbMultiplicity.
// Simply cut on ranking.
bool StEStructEventCuts::goodVertexTopology(StMuDst* muDst) {
//    if (muDst->event()->refMult() >= 17) {
//        if (fabs(muDst->primaryVertex()->meanDip())/muDst->event()->ctbMultiplicity() < (0.8/800)) {
//            return true;
//        }
//    } else {
        if (muDst->primaryVertex()->ranking()>-2.5) {
            return true;
        }
//    }
    return false;
}

void StEStructEventCuts::printCutStats(ostream& ofs){

  //  ofs<<"# ******************************************** "<<endl;
  //  ofs<<"# *************** Event Cuts ***************** "<<endl;
  //  ofs<<"# *** format = variable,minvalue,maxvalue  *** "<<endl;
  //  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  ofs<<mtWordName.name<<","<<mtWord[0]<<","<<mtWord[1]<<"\t\t\t"<<" # triggerWord cut"<<endl;
  ofs<<mpVertexZName.name<<","<<mpVertexZ[0]<<","<<mpVertexZ[1]<<"\t\t"<<" # primary vertex cut"<<endl;
  ofs<<mpVPDVertexName.name<<","<<mpVPDVertex[0]<<","<<mpVPDVertex[1]<<"\t\t"<<" # primary vertex - vpd position cut"<<endl;
  ofs<<mpVertexRadiusName.name<<","<<mpVertexRadius[0]<<","<<mpVertexRadius[1]<<"\t\t"<<" # primary vertex radius cut"<<endl;
  ofs<<mcentralityName.name<<","<<mcentrality[0]<<","<<mcentrality[1]<<"\t\t\t"<<" # number events passing centrality cuts"<<endl;
  ofs<<mgoodtoffractionName.name<<","<<mgoodtoffraction[0]<<","<<mgoodtoffraction[1]<<"\t\t\t"<<" # number events passing centrality cuts"<<endl;
  ofs<<mgoodprimaryfractionName.name<<","<<mgoodprimaryfraction[0]<<","<<mgoodprimaryfraction[1]<<"\t\t\t"<<" # number events passing centrality cuts"<<endl;
  ofs<<mZVertSepName.name<<","<<mZVertSep[0]<<","<<mZVertSep[1]<<"\t\t\t"<<" # number events passing vertex separation pileup cuts"<<endl;
  ofs<<mZVertMatchName.name<<","<<mZVertMatch[0]<<","<<mZVertMatch[1]<<"\t\t\t"<<" # number events passing sigle sided vertex cuts"<<endl;
  //  ofs<<"# ******************************************** "<<endl<<endl;

}

/***********************************************************************
 *
 * $Log: StEStructEventCuts.cxx,v $
 * Revision 1.19  2012/11/16 21:19:06  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.18  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.17  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.16  2010/06/23 22:29:41  prindle
 *   Hadd typo of 2004B instead of B2004 in EventCuts.cxx
 *   Added a couple of histograms in QAHists.
 *
 * Revision 1.15  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.14  2009/05/08 00:04:22  prindle
 * Just putting Yuri's TMath back in
 *
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









