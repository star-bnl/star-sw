/***************************************************************************
 *
 * $Id: StEstMaker.cxx,v 1.2 2001/01/25 17:54:17 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Main methods for StEstMaker
 *
 ***************************************************************************
 *
 * $Log: StEstMaker.cxx,v $
 * Revision 1.2  2001/01/25 17:54:17  lmartin
 * Divorced from the real tracking code.
 * Initialize the StEstParams and StEstSegments objects controling the tracking.
 * Instantiate a StEstTracker object and calls the initializing, running and saving
 * methods of this object
 *
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#include "StEstMaker.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_egr_egrpar_Table.h"
/* #include "tables/St_g2t_vertex_Table.h" */
#include "tables/St_tpt_track_Table.h" 
#include "tables/St_tcl_tphit_Table.h" 
#include "tables/St_tte_eval_Table.h" 
/* #include "tables/St_tte_mctrk_Table.h" */
#include "tables/St_stk_track_Table.h" 
#include "tables/St_sgr_groups_Table.h" 
#include "tables/St_svm_evt_match_Table.h" 

ClassImp(StEstMaker)

StEstMaker::StEstMaker(const char* name):StMaker(name) {
  cout<<"-------------------------------------------------------------> StEstMaker Constructor "<<endl;
}

StEstMaker::~StEstMaker() {
  cout<<"-------------------------------------------------------------> StEstMaker Destructor "<<endl;
}

Int_t StEstMaker::Finish() {
  cout<<"StEstMaker::Finish() ****START****"<<endl;
  return StMaker::Finish();
  cout<<"StEstMaker::Finish() ****STOP****"<<endl;
}

Int_t StEstMaker::Init(){
  cout<<"-------------------------------------------------------------> StEstMaker Init "<<endl;
  int i,j;

  cout<<"**** StEstMaker::StEstMaker() ****Init"<<endl;

  // creating the egr_par table.
  m_egr_egrpar = new St_egr_egrpar("egr_egrpar",1); {
    egr_egrpar_st row;
    //
    memset(&row,0,m_egr_egrpar->GetRowSize());
    row.debug[0]         =          1; // flags for debug printing ;
    row.debug[1]         =          0;
    row.debug[2]         =          0;
    row.debug[3]         =          0;
    row.debug[4]         =          0;
    row.debug[5]         =          0;
    row.debug[6]         =          0;
    row.debug[7]         =          0;
    row.debug[8]         =          0;
    row.minfit   =          2; // min no. of points on track ;
    row.mxtry    =         10; // max no. of attempts to fit ;
    row.useglobal = 2; // set if to usematching to be used ;
    row.usetpc   =  1; // set if TPC used in refit ;
    row.useemc   =          0; // set if EMC used in refit ;
    row.usesvt   =          0; // set if SVT used in refit ;
    row.usetof   =          0; // set if TOF used in refit ;
    row.usevert  =          0; // Set if primary vertex used in refit ;
    row.prob[0]  =        -200; // probability cut in fit ;
    row.prob[1]  =        -100;
    //    row.prob[0]  =        10; // probability cut in fit ;
    //    row.prob[1]  =        10;
//     row.prob[0]  =        -200; // probability cut in fit ;
    //     row.prob[1]  =        -100;
    row.svtchicut        =  0; // SVT chi2 cut for adding SVT-only tracks ;
    m_egr_egrpar->AddAt(&row,0);
  }
  
  AddRunCont(m_egr_egrpar);
 
  m_egrpar_h = new table_head_st[1];
  m_egrpar_h[0].maxlen=1;
  m_egrpar_h[0].nok=1;


  mNPass = 5;
  //  mNPass = 1;

 // ideal tracking: mIdealTracking = 1
  mIdealTracking = 1;
  mDebugLevel = 0;

  mParams = new StEstParams*[mNPass];
  for (i=0;i<mNPass;i++) mParams[i] = new StEstParams;

  // default settings, may be overridden by SetParams:
  for (i=0;i<mNPass;i++) {
    mParams[i]->debug = 2;
   // Cant have ideal without MC info
    if( mIdealTracking &&  mParams[i]->debug <2 ) mParams[i]->debug = 2;
    for (j=0;j<4;j++) {
      mParams[i]->nneighbours[j] = 2;
      mParams[i]->ntotbranch[j] = 6;
      mParams[i]->onoff[j] = 1;
      mParams[i]->share[j] = 5;
    }
    //    mParams[i]->onoff[3] = 0;
    mParams[i]->nbranch[3] = 3;
    mParams[i]->nbranch[2] = 2;
    //    mParams[i]->nbranch[3] = 1;
    //    mParams[i]->nbranch[2] = 1;
    mParams[i]->nbranch[1] = 1;
    mParams[i]->nbranch[0] = 1;

    mParams[i]->maxtpchits=50;
    mParams[i]->maxsvthits=8;    
    mParams[i]->maxbranches=100;     

//     mParams[i]->phibin = 5;
//     mParams[i]->zbin = 2;
//     mParams[i]->nphibins = 72;
//     mParams[i]->nzbins = 36;
    mParams[i]->phibin = 10;
    mParams[i]->zbin = 4;
    mParams[i]->nphibins = 36;
    mParams[i]->nzbins = 18;
    
    mParams[i]->lrad[0][0] = 6.125;
    mParams[i]->lrad[0][1] = 7.185;
    mParams[i]->lrad[1][0] = 10.185;
    mParams[i]->lrad[1][1] = 11.075;
    mParams[i]->lrad[2][0] = 13.995;
    mParams[i]->lrad[2][1] = 14.935;
    mParams[i]->lrad[3][0] = 23;
    mParams[i]->lrad[3][1] = 23;
  }	
    
  mParams[4]->ptmin = 0.1;
  mParams[4]->ptmax = 0.2;
  mParams[3]->ptmin = 0.2;
  mParams[3]->ptmax = 0.4;
  mParams[2]->ptmin = 0.4;
  mParams[2]->ptmax = 0.7;
  mParams[1]->ptmin = 0.7;
  mParams[1]->ptmax = 1.0;  
  mParams[0]->ptmin = 1.0;
  mParams[0]->ptmax = 100;

  mParams[0]->geomcutl[3] = 1.0;
  mParams[0]->geomcutl[2] = 0.5;
  mParams[0]->geomcutl[1] = 0.2;
  mParams[0]->geomcutl[0] = 0.2;
  mParams[0]->geomcutw[3] = 1.0;
  mParams[0]->geomcutw[2] = 0.5;
  mParams[0]->geomcutw[1] = 0.2;
  mParams[0]->geomcutw[0] = 0.2;

  mParams[1]->geomcutl[3] = 1.0;
  mParams[1]->geomcutl[2] = 0.5;
  mParams[1]->geomcutl[1] = 0.2;
  mParams[1]->geomcutl[0] = 0.2;
  mParams[1]->geomcutw[3] = 1.0;
  mParams[1]->geomcutw[2] = 0.5;
  mParams[1]->geomcutw[1] = 0.2;
  mParams[1]->geomcutw[0] = 0.2;
  
  mParams[2]->geomcutl[3] = 1.5;
  mParams[2]->geomcutl[2] = 0.5;
  mParams[2]->geomcutl[1] = 0.2;
  mParams[2]->geomcutl[0] = 0.2;
  mParams[2]->geomcutw[3] = 1.5;
  mParams[2]->geomcutw[2] = 0.5;
  mParams[2]->geomcutw[1] = 0.2;
  mParams[2]->geomcutw[0] = 0.2;
  
  mParams[3]->geomcutl[3] = 3.0;
  mParams[3]->geomcutl[2] = 0.5;
  mParams[3]->geomcutl[1] = 0.2;
  mParams[3]->geomcutl[0] = 0.2;
  mParams[3]->geomcutw[3] = 3.0;
  mParams[3]->geomcutw[2] = 0.5;
  mParams[3]->geomcutw[1] = 0.2;
  mParams[3]->geomcutw[0] = 0.2;
  
  mParams[4]->geomcutl[3] = 5.;
  mParams[4]->geomcutl[2] = 0.5;
  mParams[4]->geomcutl[1] = 0.2;
  mParams[4]->geomcutl[0] = 0.2;
  mParams[4]->geomcutw[3] = 5.;
  mParams[4]->geomcutw[2] = 0.5;
  mParams[4]->geomcutw[1] = 0.2;
  mParams[4]->geomcutw[0] = 0.2;


  // superpass settings

  mNSuperPass = 1;
  mSegments = new StEstSegments*[mNSuperPass];
  for (i=0;i<mNSuperPass;i++) mSegments[i] = new StEstSegments;

  mSegments[0]->chisqcut = 300;
  mSegments[0]->minhits=4;
  mSegments[0]->rminTPC=500;
  mSegments[0]->minTPChits=0;
  mSegments[0]->slay[3]=2;
  mSegments[0]->slay[2]=2;
  mSegments[0]->slay[1]=2;
  mSegments[0]->slay[0]=2;
  
//   mSegments[1]->chisqcut = 100;
//   mSegments[1]->minhits=3;
//   mSegments[1]->rminTPC=500;
//   mSegments[1]->minTPChits=0;
//   mSegments[1]->slay[3]=1;
//   mSegments[1]->slay[2]=1;
//   mSegments[1]->slay[1]=1;
//   mSegments[1]->slay[0]=1;

//   mSegments[2]->chisqcut = 30;
//   mSegments[2]->minhits=1;
//   mSegments[2]->rminTPC=500;
//   mSegments[2]->minTPChits=0;
//   mSegments[2]->slay[3]=1;
//   mSegments[2]->slay[2]=1;
//   mSegments[2]->slay[1]=1;
//   mSegments[2]->slay[0]=1;
  

//   mSegments[3]->chisqcut = 30;
//   mSegments[3]->minhits=2;
//   mSegments[3]->rminTPC=500;
//   mSegments[3]->minTPChits=0;
//   mSegments[3]->slay[3]=1;
//   mSegments[3]->slay[2]=1;
//   mSegments[3]->slay[1]=1;
//   mSegments[3]->slay[0]=1;

//   mSegments[4]->chisqcut = 40;
//   mSegments[4]->minhits=2;
//   mSegments[4]->rminTPC=100;
//   mSegments[4]->minTPChits=0;
//   mSegments[4]->slay[3]=1;
//   mSegments[4]->slay[2]=1;
//   mSegments[4]->slay[1]=1;
//   mSegments[4]->slay[0]=1;
  
  

  PrintSettings();
  cout<<"**** StEstMaker::StEstMaker() **** INIT STOP"<<endl;
  return kStOK;

}

void StEstMaker::PrintSettings() {
  // Print the tracking parameter settings.

  long j,k;

  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"\t\t\t SETTINGS OF THE Pt PASSES\t\t\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"number of passes "<<mNPass;
  for (j=0;j<mNPass;j++) cout<<"\t| "<<j;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"ptmin\t\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ptmin;
  cout<<"\t|"<<endl;
  cout<<"ptmax\t\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ptmax;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" lin. geom. cut";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->geomcutl[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" cir. geom. cut";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->geomcutw[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"layer 0123 on(1)/off(0)";
  for (j=0;j<mNPass;j++) {
    cout<<"\t| ";
    for (k=0;k<4;k++) cout<<mParams[j]->onoff[k];
  }
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" ring of neighb.";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nneighbours[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" branching";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nbranch[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" total branching";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->ntotbranch[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  for (k=0;k<4;k++)
    {
      cout<<"layer "<<k<<" hit sharing";
      for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->share[k];
      cout<<"\t|"<<endl;
    }
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"max tpc hits in tracks";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxtpchits;
  cout<<"\t|"<<endl;
  cout<<"max svt hits in tracks";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxsvthits;
  cout<<"\t|"<<endl;
  cout<<"max number of branches";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->maxbranches;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<"phi bin size\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->phibin;
  cout<<"\t|"<<endl;
  cout<<"z bin size\t";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->zbin;
  cout<<"\t|"<<endl;
  cout<<"number of phi bins";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nphibins;
  cout<<"\t|"<<endl;
  cout<<"number of z bins";
  for (j=0;j<mNPass;j++) cout<<"\t| "<<mParams[j]->nzbins;
  cout<<"\t|"<<endl;
  cout<<"-----------------------------------------------------------------"<<endl;
  cout<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"\t\t SETTINGS OF THE SEGMENT SUPERPASSES\t\t\t\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"number of superpasses "<<mNSuperPass;
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<j;
  cout<<"\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"chisq cut\t";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->chisqcut;
  cout<<"\t|"<<endl;
  cout<<"min TPC hits\t";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->minTPChits;
  cout<<"\t|"<<endl;
  cout<<"TPCtrack max orig.(x,y)";
  for (j=0;j<mNSuperPass;j++) cout<<"\t| "<<mSegments[j]->rminTPC;
  cout<<"\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
  cout<<"min SVT/SSD hits  ";
  for (j=0;j<mNSuperPass;j++) {
    cout<<"\t| ";
    cout<<mSegments[j]->minhits;
    }
  cout<<"\t|"<<endl;
  cout<<"hit in layer 0123";
  for (j=0;j<mNSuperPass;j++)
    {
    cout<<"\t| ";
    for (k=0;k<4;k++) cout<<mSegments[j]->slay[k];
    }
  cout<<"\t|"<<endl;
  cout<<"requiered (2) possible(1) excluded (0) \t\t\t\t\t\t|"<<endl;
  cout<<"-------------------------------------------------------------------------------"<<endl;
}


Int_t StEstMaker::Make() {
  PrintInfo();
  cout<<"-------------------------------------------------------------> StEstMaker Make "<<endl;

  const Int_t maxNofTracks = 50000;

  // Creating the output tables
  St_stk_track     *svttrk     = new St_stk_track("EstSvtTrk",maxNofTracks);
  AddData(svttrk);
  St_sgr_groups     *svtgrps     = new St_sgr_groups("EstGroups",maxNofTracks*4);
  AddData(svtgrps);
  St_svm_evt_match     *EstMatch     = new St_svm_evt_match("EstMatch",maxNofTracks);
  AddData(EstMatch);

  // Getting the input tables
  St_dst_vertex  *preVertex =0;
  preVertex = (St_dst_vertex *)GetDataSet("preVertex/.data/preVertex"); 

  St_DataSetIter       local(GetInputDB("svt"));
  // Getting the geometry and configuration tables
  St_svg_geom*   Stsvggeom  =0;
  
  St_DataSet *svt = GetDataSet("StSvtAnalResults");
  StSvtHybridCollection* SvtCluColl =0;
  if( svt)
    SvtCluColl = (StSvtHybridCollection*)(svt->GetObject());
  
  if( SvtCluColl){
    if(  !strncmp(SvtCluColl->getConfiguration(), "Y1L", strlen("Y1L"))){
      Stsvggeom        = (St_svg_geom  *) local("svgpars/geomy1l");
    }
  }
  
  if(!Stsvggeom)  Stsvggeom = (St_svg_geom *)local("svgpars/geom");
  if (!Stsvggeom) return kStWarn;
  cout<<"svggeom="<<Stsvggeom->GetNRows()<<endl;

  St_svg_shape*   Stsvgshape =0;
  Stsvgshape = (St_svg_shape *)local("svgpars/shape");
  if (!Stsvgshape) return kStWarn;
  cout<<"svgshape="<<Stsvgshape->GetNRows()<<endl;
  
  St_svg_config*   Stsvgconf =0;
  Stsvgconf = (St_svg_config *)local("svgpars/config");
  if (!Stsvgconf) return kStWarn;
  cout<<"svgconf="<<Stsvgconf->GetNRows()<<endl;

  //Getting the SVT/SDD hit table
  St_scs_spt*   Stscsspt=0;
  svt  = GetInputDS("svt_hits");

  Stscsspt = (St_scs_spt *)svt->Find("scs_spt");
  if (!Stscsspt) { 
    cout<<"No SVT/SSD hits !"<<endl;
    return kStWarn;
  }
  cout<<"scs_spt="<<Stscsspt->GetNRows()<<endl;
  
  //Getting the TPC hit table
  St_tcl_tphit*   Sttphit=0;
  St_DataSet* tpc  = GetInputDS("tpc_hits");

  Sttphit = (St_tcl_tphit *)tpc->Find("tphit");
  if (!Sttphit) return kStWarn;

  cout<<"tphit="<<Sttphit->GetNRows()<<endl;
  //Getting the TPC track table
  St_tpt_track*   Sttptrack=0;
  tpc  = GetInputDS("tpc_tracks");

  Sttptrack = (St_tpt_track *)tpc->Find("tptrack");
  if (!Sttptrack) return kStWarn;

  cout<<"tptrack="<<Sttptrack->GetNRows()<<endl;

  //Getting the TPC evaluation table
  St_tte_eval*   Stevaltrk=0;
  St_g2t_track*   Stg2ttrack=0;
  St_g2t_vertex*   Stg2tvertex=0;
  if (mIdealTracking==1) {
    Stevaltrk = (St_tte_eval *)tpc->Find("evaltrk");
    cout<<"tteeval="<<Stevaltrk->GetNRows()<<endl;
    St_DataSet *geant  = GetInputDS("geant");
    Stg2ttrack = (St_g2t_track *)geant->Find("g2t_track");
    Stg2tvertex = (St_g2t_vertex *)geant->Find("g2t_vertex");
  }
  if ((!Stevaltrk||!Stg2ttrack||!Stg2tvertex)&&mIdealTracking==1) {
    cout<<"Warning Evaluation table(s) not found ! IdealTracking reset to 0"<<endl;
    mIdealTracking=0;
  }

  cout<<"mNPass="<<mNPass<<endl;
  cout<<"mNSuperPass="<<mNSuperPass<<endl;
  cout<<"mIdealTracking="<<mIdealTracking<<endl;
  cout<<"mDebugLevel="<<mDebugLevel<<endl;

  // Making a Tracker
  StEstTracker* Tracker = new StEstTracker(mNPass,
					   mNSuperPass,
					   mIdealTracking,
					   mDebugLevel,
					   mParams,
					   mSegments,
					   m_egr_egrpar,
					   m_egrpar_h);
  // Making the StEstWafer and StEstHit objects
  Tracker->SVTInit(Stsvggeom,
		   Stsvgshape,
		   Stsvgconf,
		   Stscsspt);
  // Making the StEstVertex object
  Tracker->VertexSetup(preVertex);

  // Making the StTrack and StTPCTrack objects
  Tracker->TPCInit(Sttptrack,Sttphit);
  Tracker->BranchInit();
  // Preparing the evaluation information
  if (mIdealTracking==1) Tracker->SetupMc(Stscsspt,
					  Stevaltrk,
					  Stg2ttrack,
					  Stg2tvertex);
  // Doing the tracking
  Tracker->DoTracking();
  // Saving the tracks formed into tables
  Tracker->EsttoGlobtrk(svttrk,
			svtgrps,
			EstMatch);
  cout<<"svttrk="<<svttrk->GetNRows()<<endl;
  cout<<"svtgrps="<<svtgrps->GetNRows()<<endl;
  cout<<"EstMatch="<<EstMatch->GetNRows()<<endl;

  // Cleaning and deleting the objects
  Tracker->CleanUp();
  delete Tracker;
  return kStOK;

}
