/**********************************************************************
 *
 * $Id: StEStructTrackCuts.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
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
   mpt[0]=mpt[1]=0;
   myt[0]=myt[1]=0;
   mphi[0]=mphi[1]=0;
   meta[0]=meta[1]=0;  
 
}

void StEStructTrackCuts::initNames(){

  strcpy(mflagName.name,"Flag");
  strcpy(mchargeName.name,"Charge");
  strcpy(mnfitpointsName.name,"NFitPoints");
  strcpy(mnfitnmaxName.name,"NFitPerNMax");
  strcpy(mglobalDCAName.name,"GlobalDCA");
  strcpy(mchi2Name.name,"Chi2");
  strcpy(mptName.name,"Pt");
  strcpy(mytName.name,"Yt");
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

  if(!strcmp(name,mptName.name)){ 
    mpt[0]=atof(vals[0]); mpt[1]=atof(vals[1]);
    mptName.idx = createCutHists(name,mpt);
    return true;
  }
  if(!strcmp(name,mytName.name)){ 
    myt[0]=atof(vals[0]); myt[1]=atof(vals[1]);
    mytName.idx = createCutHists(name,myt);
    return true;
  }

  if(!strcmp(name,mphiName.name)){ 
    mphi[0]=(float)(M_PI*atof(vals[0])); mphi[1]=(float)(M_PI*atof(vals[1]));
    mphiName.idx = createCutHists(name,mphi);
    return true;
  }

  if(!strcmp(name,metaName.name)){ 
    meta[0]=atof(vals[0]); meta[1]=atof(vals[1]);
    metaName.idx = createCutHists(name,meta);
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


void StEStructTrackCuts::printCuts(ostream& ofs){

  ofs<<"# ******************************************** "<<endl;
  ofs<<"# *************** Track Cuts ***************** "<<endl;
  ofs<<"# *** format = variable,minvalue,maxvalue  *** "<<endl;
  ofs<<"# ******************************************** "<<endl;
  ofs<<endl;
  ofs<<mflagName.name<<","<<mflag[0]<<","<<mflag[1]<<"\t\t\t"<<" # track flag cut"<<endl;
  ofs<<mchargeName.name<<","<<mcharge[0]<<","<<mcharge[1]<<"\t\t\t"<<" # charge cut"<<endl;
  ofs<<mnfitpointsName.name<<","<<mnfitpoints[0]<<","<<mnfitpoints[1]<<"\t\t\t"<<" # fit points cut"<<endl;
  ofs<<mnfitnmaxName.name<<","<<mnfitnmax[0]<<","<<mnfitnmax[1]<<"\t\t\t"<<" # fitpoints per possible cut"<<endl;
  ofs<<mglobalDCAName.name<<","<<mglobalDCA[0]<<","<<mglobalDCA[1]<<"\t\t\t"<<" # global DCA cut"<<endl;
  ofs<<mchi2Name.name<<","<<mchi2[0]<<","<<mchi2[1]<<"\t\t\t"<<" # chi square cut"<<endl;
  ofs<<mptName.name<<","<<mpt[0]<<","<<mpt[1]<<"\t\t\t\t"<<" # pt cut"<<endl;
  ofs<<mytName.name<<","<<myt[0]<<","<<myt[1]<<"\t\t\t\t"<<" # yt cut"<<endl;
  ofs<<mphiName.name<<","<<mphi[0]/M_PI<<","<<mphi[1]/M_PI<<"\t\t\t\t"<<" # phi cut in factor of pi"<<endl;
  ofs<<metaName.name<<","<<meta[0]<<","<<meta[1]<<"\t\t\t\t"<<" # eta cut"<<endl;
  ofs<<mnsigmaEName.name<<","<<mnsigmaE[0]<<","<<mnsigmaE[1]<<"\t\t"<<" # num sigma electron cut"<<endl;
  ofs<<mnsigmaPiName.name<<","<<mnsigmaPi[0]<<","<<mnsigmaPi[1]<<"\t\t\t"<<" # num sigma Pion cut"<<endl;
  ofs<<mnsigmaKName.name<<","<<mnsigmaK[0]<<","<<mnsigmaK[1]<<"\t\t\t"<<" # num sigma Kaon cut"<<endl;
  ofs<<mnsigmaPName.name<<","<<mnsigmaP[0]<<","<<mnsigmaP[1]<<"\t\t"<<" # num sigma proton cut"<<endl;
  ofs<<"# ******************************************** "<<endl<<endl;

}


/***********************************************************************
 *
 * $Log: StEStructTrackCuts.cxx,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




