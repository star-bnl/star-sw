/**********************************************************************
 *
 * $Id: StEStructTrackCuts.cxx,v 1.4 2008/12/02 23:35:35 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for track level quantities
 *
 *
 ***********************************************************************/
#include "StEStructTrackCuts.h"
#include <math.h>
#include <stdlib.h>

ClassImp(StEStructTrackCuts)

StEStructTrackCuts::StEStructTrackCuts(): StEStructCuts(){ init(); };
StEStructTrackCuts::StEStructTrackCuts(const char* cutfileName): StEStructCuts(cutfileName) { init(); };

StEStructTrackCuts::~StEStructTrackCuts() {};

void StEStructTrackCuts::init(){ 

  strcpy(mcutTypeName,"Track");
  initCuts();
  initNames();
  if(isLoaded())loadCuts();

}

void StEStructTrackCuts::initCuts(){

   mflag[0]=mflag[1]=0;
   mcharge[0]=mcharge[1]=0;
   mnfitpoints[0]=mnfitpoints[1]=0;
   mnfitnmax[0]=mnfitnmax[1]=0;
   mglobalDCA[0]=mglobalDCA[1]=0;
   mchi2[0]=mchi2[1]=0;
   mdPtByPt[0]=mdPtByPt[1]=0;
   mpt[0]=mpt[1]=0;
   myt[0]=myt[1]=0;
   mxt[0]=mxt[1]=0;
   mphi[0]=mphi[1]=0;
   meta[0]=meta[1]=0;  
   mnsigmaE[0]=mnsigmaE[1]=0;
   mnsigmaPi[0]=mnsigmaPi[1]=0;
   mnsigmaK[0]=mnsigmaK[1]=0;
   mnsigmaP[0]=mnsigmaP[1]=0;
 
}

void StEStructTrackCuts::initNames(){

  strcpy(mflagName.name,"Flag");
  strcpy(mchargeName.name,"Charge");
  strcpy(mnfitpointsName.name,"NFitPoints");
  strcpy(mnfitnmaxName.name,"NFitPerNMax");
  strcpy(mglobalDCAName.name,"GlobalDCA");
  strcpy(mchi2Name.name,"Chi2");
  strcpy(mdPtByPtName.name,"dPtByPt");
  strcpy(mptName.name,"Pt");
  strcpy(mytName.name,"Yt"); 
  strcpy(mxtName.name,"Xt") ;
  strcpy(mphiName.name,"Phi");
  strcpy(metaName.name,"Eta");
  strcpy(mnsigmaEName.name,"NSigmaElectron");
  strcpy(mnsigmaPiName.name,"NSigmaPion");
  strcpy(mnsigmaKName.name,"NSigmaKaon");
  strcpy(mnsigmaPName.name,"NSigmaProton");

}
     
bool StEStructTrackCuts::loadBaseCuts(const char* name, const char** vals, int nvals){

  if(!strcmp(name,mflagName.name)){ 
    mflag[0]=atoi(vals[0]); mflag[1]=atoi(vals[1]);
    mflagName.idx = createCutHists(name,mflag);
    return true;
  }

  if(!strcmp(name,mchargeName.name)){ 
    mcharge[0]=atoi(vals[0]); mcharge[1]=atoi(vals[1]);
    mchargeName.idx = createCutHists(name,mcharge);
    return true;
  }

  if(!strcmp(name,mnfitpointsName.name)){ 
    mnfitpoints[0]=atoi(vals[0]); mnfitpoints[1]=atoi(vals[1]);
    mnfitpointsName.idx = createCutHists(name,mnfitpoints);
    setRange(mnfitpointsName.name,mnfitpoints[0],mnfitpoints[1]);
    return true;
  }

  if(!strcmp(name,mnfitnmaxName.name)){ 
    mnfitnmax[0]=atof(vals[0]); mnfitnmax[1]=atof(vals[1]);
    mnfitnmaxName.idx = createCutHists(name,mnfitnmax);
    return true;
  }

  if(!strcmp(name,mglobalDCAName.name)){ 
    mglobalDCA[0]=atof(vals[0]); mglobalDCA[1]=atof(vals[1]);
    mglobalDCAName.idx = createCutHists(name,mglobalDCA);
    return true;
  }

  if(!strcmp(name,mchi2Name.name)){ 
    mchi2[0]=atof(vals[0]); mchi2[1]=atof(vals[1]);
    mchi2Name.idx = createCutHists(name,mchi2);
    return true;
  }

  if(!strcmp(name,mdPtByPtName.name)){ 
    mdPtByPt[0]=atof(vals[0]); mdPtByPt[1]=atof(vals[1]);
    mdPtByPtName.idx = createCutHists(name,mdPtByPt);
    return true;
  }

  if(!strcmp(name,mptName.name)){ 
    mpt[0]=atof(vals[0]); mpt[1]=atof(vals[1]);
    mptName.idx = createCutHists(name,mpt);
    setRange(mptName.name,mpt[0],mpt[1]);
    return true;
  }

  if(!strcmp(name,mytName.name)){ 
    myt[0]=atof(vals[0]); myt[1]=atof(vals[1]);
    mytName.idx = createCutHists(name,myt);
    setRange(mytName.name,myt[0],myt[1]);
    return true;
  }

  if(!strcmp(name,mxtName.name)){
    mxt[0]=atof(vals[0]); mxt[1]=atof(vals[1]);
    mxtName.idx = createCutHists(name,mxt);
    setRange(mxtName.name,mxt[0],mxt[1]);
    return true;
  }

  if(!strcmp(name,mphiName.name)){ 
    mphi[0]=(float)(M_PI*atof(vals[0])); mphi[1]=(float)(M_PI*atof(vals[1]));
    mphiName.idx = createCutHists(name,mphi);
    setRange(mphiName.name,mphi[0],mphi[1]);
    return true;
  }

  if(!strcmp(name,metaName.name)){ 
    meta[0]=atof(vals[0]); meta[1]=atof(vals[1]);
    metaName.idx = createCutHists(name,meta);
    setRange(metaName.name,meta[0],meta[1]);
    return true;
  }

  if(!strcmp(name,mnsigmaEName.name)){ 
    mnsigmaE[0]=atof(vals[0]); mnsigmaE[1]=atof(vals[1]);
    mnsigmaEName.idx = createCutHists(name,mnsigmaE);
    return true;
  }

  if(!strcmp(name,mnsigmaPiName.name)){ 
    mnsigmaPi[0]=atof(vals[0]); mnsigmaPi[1]=atof(vals[1]);
    mnsigmaPiName.idx = createCutHists(name,mnsigmaPi);
    return true;
  }

  if(!strcmp(name,mnsigmaKName.name)){ 
    mnsigmaK[0]=atof(vals[0]); mnsigmaK[1]=atof(vals[1]);
    mnsigmaKName.idx = createCutHists(name,mnsigmaK);
    return true;
  }

  if(!strcmp(name,mnsigmaPName.name)){ 
    mnsigmaP[0]=atof(vals[0]); mnsigmaP[1]=atof(vals[1]);
    mnsigmaPName.idx = createCutHists(name,mnsigmaP);
    return true;
  }


  return false;

}


void StEStructTrackCuts::printCutStats(ostream& ofs){

  //  ofs<<"# ******************************************** "<<endl;
  //  ofs<<"# *************** Track Cuts ***************** "<<endl;
  //  ofs<<"# *** format = variable,minvalue,maxvalue  *** "<<endl;
  //  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  ofs<<mflagName.name<<","<<mflag[0]<<","<<mflag[1]<<"\t\t\t"<<" # track flag cut"<<endl;
  ofs<<mchargeName.name<<","<<mcharge[0]<<","<<mcharge[1]<<"\t\t\t"<<" # charge cut"<<endl;
  ofs<<mnfitpointsName.name<<","<<mnfitpoints[0]<<","<<mnfitpoints[1]<<"\t\t"<<" # fit points cut"<<endl;
  ofs<<mnfitnmaxName.name<<","<<mnfitnmax[0]<<","<<mnfitnmax[1]<<"\t\t"<<" # fitpoints per possible cut"<<endl;
  ofs<<mglobalDCAName.name<<","<<mglobalDCA[0]<<","<<mglobalDCA[1]<<"\t\t\t"<<" # global DCA cut"<<endl;
  ofs<<mchi2Name.name<<","<<mchi2[0]<<","<<mchi2[1]<<"\t\t\t"<<" # chi square cut"<<endl;
  ofs<<mdPtByPtName.name<<","<<mdPtByPt[0]<<","<<mdPtByPt[1]<<"\t\t\t"<<" # sigma for determination of sign of charge"<<endl;
  ofs<<mptName.name<<","<<mpt[0]<<","<<mpt[1]<<"\t\t\t"<<" # pt cut"<<endl;
  ofs<<mytName.name<<","<<myt[0]<<","<<myt[1]<<"\t\t\t"<<" # yt cut"<<endl;
  ofs<<mphiName.name<<","<<mphi[0]/M_PI<<","<<mphi[1]/M_PI<<"\t\t\t"<<" # phi cut in factor of pi"<<endl;
  ofs<<metaName.name<<","<<meta[0]<<","<<meta[1]<<"\t\t\t"<<" # eta cut"<<endl;
  ofs<<mnsigmaEName.name<<","<<mnsigmaE[0]<<","<<mnsigmaE[1]<<"\t\t"<<" # num sigma electron cut"<<endl;
  ofs<<mnsigmaPiName.name<<","<<mnsigmaPi[0]<<","<<mnsigmaPi[1]<<"\t\t\t"<<" # num sigma Pion cut"<<endl;
  ofs<<mnsigmaKName.name<<","<<mnsigmaK[0]<<","<<mnsigmaK[1]<<"\t\t\t"<<" # num sigma Kaon cut"<<endl;
  ofs<<mnsigmaPName.name<<","<<mnsigmaP[0]<<","<<mnsigmaP[1]<<"\t\t"<<" # num sigma proton cut"<<endl;
  //  ofs<<"# ******************************************** "<<endl<<endl;

}


/***********************************************************************
 *
 * $Log: StEStructTrackCuts.cxx,v $
 * Revision 1.4  2008/12/02 23:35:35  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.3  2006/04/04 22:05:07  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.2  2005/09/14 17:08:36  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




