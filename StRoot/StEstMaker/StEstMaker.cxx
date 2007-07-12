/***************************************************************************
 *
 * $Id: StEstMaker.cxx,v 1.27 2007/07/12 19:33:46 fisyak Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Main methods for StEstMaker
 *
 ***************************************************************************
 *
 * $Log: StEstMaker.cxx,v $
 * Revision 1.27  2007/07/12 19:33:46  fisyak
 * Add includes for ROOT 5.16
 *
 * Revision 1.26  2004/09/16 02:22:12  perev
 * More defence for no geometry
 *
 * Revision 1.25  2004/04/15 16:28:16  jeromel
 * No functional changes, messages prepended with Class name.
 *
 * Revision 1.24  2004/03/18 02:05:08  caines
 * Maker sure tracker gest deleted before every return from make - wasnt happening if errors were found so created memory leak on the odd occasion this happend
 *
 * Revision 1.23  2004/02/11 23:25:17  caines
 * Avoid crash for missing SVT events by quiting earlier
 *
 * Revision 1.22  2003/10/11 03:16:18  perev
 * Cleanup+bugfix: test for zer pointer, initialization added.
 *
 * Revision 1.21  2003/09/02 17:58:04  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.20  2003/04/30 20:36:54  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.19  2003/04/14 18:31:10  munhoz
 * allow 1 hit tracks
 *
 * Revision 1.18  2002/11/21 23:02:48  caines
 * Fix helicity initialization for TPC tracks and no longer use assumed vertex if one isnt there
 *
 * Revision 1.17  2002/10/11 21:25:08  caines
 * Fix min radius cut to larger value
 *
 * Revision 1.16  2002/05/21 20:33:28  caines
 *  Fix radii determination
 *
 * Revision 1.15  2002/04/30 22:49:19  caines
 * Make est work with shifted SVT geom, change search radii to 1cm
 *
 * Revision 1.14  2002/02/20 17:22:02  caines
 * Comment out some of the print statements
 *
 * Revision 1.13  2002/01/31 21:10:00  caines
 * Open est cuts up
 *
 * Revision 1.12  2001/07/15 20:31:30  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.11  2001/04/25 15:05:55  lmartin
 * Retuned tracking parameters (mostly geometrical cuts).
 *
 * Revision 1.10  2001/04/23 12:20:33  lmartin
 * ptmax of the first pass set to a large value to allow tracking of no field events.
 *
 * Revision 1.9  2001/04/20 07:52:46  lmartin
 * Looking for the geantBranch dataset when the geant dataset is absent in case
 * of a chain done in two steps.
 *
 * Revision 1.8  2001/03/26 12:34:36  lmartin
 * SVT hits, TPC hits and tracks tables and datasets checked to prevent the maker from crashing.
 *
 * Revision 1.7  2001/03/02 16:03:11  lmartin
 * Finish method written to print out the cumulated tracking performances.
 * Call to the CumulEval method.
 * Second superpass (2 hit segment) switched on.
 *
 * Revision 1.6  2001/02/16 15:17:43  lmartin
 * SSD off by default. cout replaced by gMessMgr.
 *
 * Revision 1.5  2001/02/07 19:16:29  caines
 * Fix sun non compilation for non-fixed size array
 *
 * Revision 1.4  2001/01/31 16:52:19  lmartin
 * mParams[]->debug replaced by mDebug.
 * phi and z params for StEstIndexGeom remove from StEstParams.
 *
 * Revision 1.3  2001/01/31 15:05:58  caines
 * Added check to BFChain evaluation option to decide wehter to set Idealtracking on
 *
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
#include "StMessMgr.h"
#include "StEstMaker.h"
#include "StEstTracker.h"
#include "StEstParams.hh"
#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtGeometry.hh"
#include "StSvtClassLibrary/StSvtWaferGeometry.hh"
#include "tables/St_dst_vertex_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_egr_egrpar_Table.h"
#include "tables/St_tpt_track_Table.h" 
#include "tables/St_tcl_tphit_Table.h" 
#include "tables/St_tte_eval_Table.h" 
#include "tables/St_stk_track_Table.h" 
#include "tables/St_sgr_groups_Table.h" 
#include "tables/St_svm_evt_match_Table.h" 
#include "TMath.h"
ClassImp(StEstMaker)

StEstMaker::StEstMaker(const char* name):StMaker(name) {

}

StEstMaker::~StEstMaker() {

  //  delete m_egrpar_h[0];
  delete[] m_egrpar_h;

  for (int i=0;i<mNPass;i++) delete mParams[i] ;
  delete[] mParams;
}

Int_t StEstMaker::Finish() {

  float Efficiency,EfficiencyPrim,EfficiencySeco;
  float Purity,PurityPrim,PuritySeco;
  Efficiency=0;
  EfficiencyPrim=0;
  EfficiencySeco=0;
  Purity=0;
  PurityPrim=0;
  PuritySeco=0;

  gMessMgr->Info()<<"******************** EST Evaluation Summary ************************"<<endm;
  gMessMgr->Info()<<" Total number of events :\t"<<mCumulNEvents<<"\t\t\t"<<endm;
  gMessMgr->Info()<<" Total number of ideal tracks \t"<<mCumulNIdealPrim+mCumulNIdealSeco
		  <<"\t"<<mCumulNIdealPrim
		  <<"\t"<<mCumulNIdealSeco
		  <<"\t (all/prim/seco)"<<endm;
  double cum = (mCumulNEvents) ? mCumulNEvents:1;
  gMessMgr->Info()<<" Number of ideal tracks/event \t"<<(mCumulNIdealPrim+mCumulNIdealSeco)/cum
		  <<"\t"<<mCumulNIdealPrim/cum
		  <<"\t"<<mCumulNIdealSeco/cum
		  <<"\t (all/prim/seco)"<<endm;
  gMessMgr->Info()<<"  Total number of good tracks \t"<<mCumulNGoodPrim+mCumulNGoodSeco
		  <<"\t"<<mCumulNGoodPrim
		  <<"\t"<<mCumulNGoodSeco
		  <<"\t (all/prim/seco)"<<endm;
  gMessMgr->Info()<<"  Number of good tracks/event \t"<<double(mCumulNGoodPrim+mCumulNGoodSeco)/cum
		  <<"\t"<<mCumulNGoodPrim/cum
		  <<"\t"<<mCumulNGoodSeco/cum
		  <<"\t (all/prim/seco)"<<endm;
  gMessMgr->Info()<<"   Total number of bad tracks \t"<<mCumulNBadPrim+mCumulNBadSeco
		  <<"\t"<<mCumulNBadPrim
		  <<"\t"<<mCumulNBadSeco
		  <<"\t (all/prim/seco)"<<endm;
  gMessMgr->Info()<<"   Number of bad tracks/event \t"<<(mCumulNBadPrim+mCumulNBadSeco)/cum
		  <<"\t"<<mCumulNBadPrim/cum
		  <<"\t"<<mCumulNBadSeco/cum
		  <<"\t (all/prim/seco)"<<endm;
  if ((mCumulNIdealPrim+mCumulNIdealSeco)!=0)
    Efficiency=100.*(mCumulNGoodPrim+mCumulNGoodSeco)/((mCumulNIdealPrim+mCumulNIdealSeco+1.e-10));
  if (mCumulNIdealPrim!=0)
    EfficiencyPrim=100.*mCumulNGoodPrim/(mCumulNIdealPrim+1.e-10);
  if (mCumulNIdealSeco!=0)
    EfficiencySeco=100.*mCumulNGoodSeco/(mCumulNIdealSeco+1.e-10);
  if ((mCumulNGoodPrim+mCumulNGoodSeco+mCumulNBadPrim+mCumulNBadSeco)!=0)
    Purity=100.*(mCumulNGoodPrim+mCumulNGoodSeco)/((mCumulNGoodPrim+mCumulNGoodSeco+mCumulNBadPrim+mCumulNBadSeco)+1.e-10);
  if ((mCumulNGoodPrim+mCumulNBadPrim)!=0)
    PurityPrim=100.*(mCumulNGoodPrim)/((mCumulNGoodPrim+mCumulNBadPrim)+1.e-10);
  if ((mCumulNGoodSeco+mCumulNBadSeco)!=0)
    PuritySeco=100.*(mCumulNGoodSeco)/((mCumulNGoodSeco+mCumulNBadSeco)+1.e-10);

  gMessMgr->Info()<<"      Tracking efficiency (%) \t"<<Efficiency
		  <<"\t"<<EfficiencyPrim
		  <<"\t"<<EfficiencySeco
		  <<"\t (all/prim/seco)"<<endm;
  gMessMgr->Info()<<"                   Purity (%) \t"<<Purity
		  <<"\t"<<PurityPrim
		  <<"\t"<<PuritySeco
		  <<"\t (all/prim/seco)"<<endm;
  return StMaker::Finish();

}

Int_t StEstMaker::Init(){
  int i,j;

  gMessMgr->Info("StEstMaker::Init START");

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
  mIdealTracking=0;

 // ideal tracking: mIdealTracking = 1
  mIdealTracking = 0;
  mDebugLevel = 1;

  mParams = new StEstParams*[mNPass];
  for (i=0;i<mNPass;i++) mParams[i] = new StEstParams;

  // default settings, may be overridden by SetParams:
  for (i=0;i<mNPass;i++) {
    for (j=0;j<4;j++) {
      mParams[i]->nneighbours[j] = 2;
      mParams[i]->ntotbranch[j] = 6;
      mParams[i]->onoff[j] = 1;
      mParams[i]->share[j] = 5;
    }
    mParams[i]->onoff[3] = 0;
    mParams[i]->nbranch[3] = 1;
    mParams[i]->nbranch[2] = 5;
    mParams[i]->nbranch[1] = 2;
    mParams[i]->nbranch[0] = 1;

    mParams[i]->maxtpchits=50;
    mParams[i]->maxsvthits=8;    
    mParams[i]->maxbranches=100;     

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
  mParams[0]->ptmax = 10000000;

  mParams[0]->geomcutl[3] = 1.0;
  mParams[0]->geomcutw[3] = 1.0;
  mParams[1]->geomcutw[3] = 1.0;
  mParams[1]->geomcutl[3] = 1.0;
  mParams[2]->geomcutl[3] = 1.0;
  mParams[2]->geomcutw[3] = 1.0;
  mParams[3]->geomcutl[3] = 1.0;
  mParams[3]->geomcutw[3] = 1.0;
  mParams[4]->geomcutl[3] = 1.0;
  mParams[4]->geomcutw[3] = 1.0;

  mParams[0]->geomcutl[2] = 1.0;
  mParams[0]->geomcutl[1] = 1.;
  mParams[0]->geomcutl[0] = 1.;
  mParams[0]->geomcutw[2] = 1.0;
  mParams[0]->geomcutw[1] = 1.;
  mParams[0]->geomcutw[0] = 1.;

  mParams[1]->geomcutl[2] = 1.0;
  mParams[1]->geomcutl[1] = 1.;
  mParams[1]->geomcutl[0] = 1.;
  mParams[1]->geomcutw[2] = 1.0;
  mParams[1]->geomcutw[1] = 1.;
  mParams[1]->geomcutw[0] = 1.;
  
  mParams[2]->geomcutl[2] = 1.0;
  mParams[2]->geomcutl[1] = 1.;
  mParams[2]->geomcutl[0] = 1.;
  mParams[2]->geomcutw[2] = 1.0;
  mParams[2]->geomcutw[1] = 1.;
  mParams[2]->geomcutw[0] = 1.;
  
  mParams[3]->geomcutl[2] = 1.0;
  mParams[3]->geomcutl[1] = 1.;
  mParams[3]->geomcutl[0] = 1.;
  mParams[3]->geomcutw[2] = 1.0;
  mParams[3]->geomcutw[1] = 1.;
  mParams[3]->geomcutw[0] = 1.;
  
  mParams[4]->geomcutl[2] = 1.0;
  mParams[4]->geomcutl[1] = 1.;
  mParams[4]->geomcutl[0] = 1.;
  mParams[4]->geomcutw[2] = 1.0;
  mParams[4]->geomcutw[1] = 1.;
  mParams[4]->geomcutw[0] = 1.;


  // superpass settings

  mNSuperPass = 3;
  mSegments = new StEstSegments*[mNSuperPass];
  for (i=0;i<mNSuperPass;i++) mSegments[i] = new StEstSegments;

  mSegments[0]->chisqcut = 300;
  mSegments[0]->minhits=3;
  mSegments[0]->rminTPC=500;
  mSegments[0]->minTPChits=0;
  mSegments[0]->slay[3]=0;
  mSegments[0]->slay[2]=2;
  mSegments[0]->slay[1]=2;
  mSegments[0]->slay[0]=2;
  
  mSegments[1]->chisqcut = 100;
  mSegments[1]->minhits=2;
  mSegments[1]->rminTPC=2000;
  mSegments[1]->minTPChits=0;
  mSegments[1]->slay[3]=0;
  mSegments[1]->slay[2]=1;
  mSegments[1]->slay[1]=1;
  mSegments[1]->slay[0]=1;

   mSegments[2]->chisqcut = 100;
   mSegments[2]->minhits=1;
   mSegments[2]->rminTPC=2000;
   mSegments[2]->minTPChits=0;
   mSegments[2]->slay[3]=0;
   mSegments[2]->slay[2]=1;
   mSegments[2]->slay[1]=1;
   mSegments[2]->slay[0]=1;
  

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

  mCumulNIdealPrim=0;
  mCumulNIdealSeco=0;
  mCumulNGoodPrim=0;
  mCumulNGoodSeco=0;
  mCumulNBadPrim=0;
  mCumulNBadSeco=0;
  mCumulNEvents=0;


  gMessMgr->Info("StEstMaker::Init STOP");
  return kStOK;

}

void StEstMaker::PrintSettings() {
  // Print the tracking parameter settings.

  long j;

  gMessMgr->Info()<<"*********** Main Est Settings ************** \t\t\t\t"<<endm;
  gMessMgr->Info()<<"layer 0123 on(1)/off(0) : "
		  <<mParams[0]->onoff[0]<<" "
		  <<mParams[0]->onoff[1]<<" "
		  <<mParams[0]->onoff[2]<<" "
		  <<mParams[0]->onoff[3]<<"\t\t\t\t\t"<<endm;
  gMessMgr->Info()<<"number of passes : "<<mNPass<<"\t\t\t\t\t\t\t"<<endm;
  gMessMgr->Info()<<"See StEstMaker.cxx for more details on the pass parameters \t\t"<<endm;
  gMessMgr->Info()<<"number of superpasses : "<<mNSuperPass<<"\t\t\t\t\t\t"<<endm;
  gMessMgr->Info()<<"pass min hits(SVT) hitpattern max chi min hits(TPC) max r0(TPC) \t"<<endm;

  for (j=0;j<mNSuperPass;j++) 
    gMessMgr->Info()<<"  "<<j<<"\t"
		    <<mSegments[j]->minhits<<"\t\t"
		    <<mSegments[j]->slay[0] 
		    <<mSegments[j]->slay[1] 
		    <<mSegments[j]->slay[2] 
		    <<mSegments[j]->slay[3]<<"\t"
		    <<mSegments[j]->chisqcut<<"\t\t"
		    <<mSegments[j]->minTPChits<<"\t"
		    <<mSegments[j]->rminTPC<<"\t\t"<<endm;
  gMessMgr->Info()<<"******************************************** \t\t\t\t"<<endm;
}


Int_t StEstMaker::Make() {
  PrintInfo();

  const Int_t maxNofTracks = 50000;

  gMessMgr->Info()<<"StEstMaker : Creating the output tables"<<endm;
  // Creating the output tables
  St_stk_track     *svttrk     = new St_stk_track("EstSvtTrk",maxNofTracks);
  AddData(svttrk);
  St_sgr_groups     *svtgrps   = new St_sgr_groups("EstGroups",maxNofTracks*4);
  AddData(svtgrps);
  St_svm_evt_match  *EstMatch  = new St_svm_evt_match("EstMatch",maxNofTracks);
  AddData(EstMatch);

  gMessMgr->Info()<<"StEstMaker : Getting the input tables"<<endm;
  // Getting the input tables
  St_dst_vertex  *preVertex =0;
  preVertex = (St_dst_vertex *)GetDataSet("preVertex/.data/preVertex"); 

  St_DataSetIter       local(GetInputDB("svt"));
  // Getting the geometry and configuration tables
  //St_svg_geom*   Stsvggeom  =0;
  
  St_DataSet *svt = GetDataSet("StSvtAnalResults");
  StSvtHybridCollection* SvtCluColl =0;
  if( svt)
    SvtCluColl = (StSvtHybridCollection*)(svt->GetObject());
  
  //  if( SvtCluColl){
  //    if(  !strncmp(SvtCluColl->getConfiguration(), "Y1L", strlen("Y1L"))){
  //      Stsvggeom        = (St_svg_geom  *) local("svgpars/geomy1l");
  //    }
  //  }
  
  //if(!Stsvggeom)  Stsvggeom = (St_svg_geom *)local("svgpars/geom");
  //if (!Stsvggeom) return kStWarn;

  //svg_geom_st* geom = Stsvggeom->GetTable();
  
  // get geometry
  St_DataSet* dataSet;
  dataSet = GetDataSet("StSvtGeometry");
  if(!dataSet) {
    gMessMgr->Error("StEstMaker : Failure to get SVT geometry - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    
    return kStWarn;
  }

  StSvtGeometry* m_geom = (StSvtGeometry*)dataSet->GetObject();
  if(!m_geom) {
    gMessMgr->Error("StEstMaker : Failure to get SVT geometry from  StSvtGeometry DataSet   - THINGS HAVE GONE SERIOUSLY WRONG!!!!! \n");
    return kStWarn;
  }
  
  StSvtWaferGeometry* waferGeom;

  //VPunused int index;

  for (int i=0;i<mNPass;i++){
    waferGeom = (StSvtWaferGeometry*)m_geom->at(6);
    mParams[i]->lrad[0][0] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(2);
    mParams[i]->lrad[0][1] =TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(41);
    mParams[i]->lrad[1][0] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(35);
    mParams[i]->lrad[1][1] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(115);
    mParams[i]->lrad[2][0] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(108);
    mParams[i]->lrad[2][1] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    /*
    waferGeom = (StSvtWaferGeometry*)m_geom->at(116);
    mParams[i]->lrad[3][0] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    waferGeom = (StSvtWaferGeometry*)m_geom->at(116);
    mParams[i]->lrad[3][1] = TMath::Sqrt(waferGeom->x(0)*waferGeom->x(0)+
				  waferGeom->x(1)*waferGeom->x(1));
    */
  }

  St_svg_shape*   Stsvgshape =0;
  Stsvgshape = (St_svg_shape *)local("svgpars/shape");
  if (!Stsvgshape) return kStWarn;
  
  St_svg_config*   Stsvgconf =0;
  Stsvgconf = (St_svg_config *)local("svgpars/config");
  if (!Stsvgconf) return kStWarn;

  //Getting the SVT/SDD hit table
  St_scs_spt*   Stscsspt=0;
  svt  = GetInputDS("svt_hits");
  if (!svt) {
    gMessMgr->Warning("StEstMaker : No svt_hits Dataset !");
    return kStWarn;
  }    

  Stscsspt = (St_scs_spt *)svt->Find("scs_spt");
  if (!Stscsspt) { 
    gMessMgr->Warning("StEstMaker : No SVT/SSD hits !");
    return kStWarn;
  }
  
  //Getting the TPC hit table
  St_tcl_tphit*   Sttphit=0;
  St_DataSet* tpc  = GetInputDS("tpc_hits");
  if (!tpc) {
    gMessMgr->Warning("StEstMaker : No tpc_hits Dataset !");
    return kStWarn;
  }    

  Sttphit = (St_tcl_tphit *)tpc->Find("tphit");
  if (!Sttphit) {
    gMessMgr->Warning("StEstMaker : No TPC hits !");
    return kStWarn;
  }
  //Getting the TPC track table
  St_tpt_track*   Sttptrack=0;
  tpc  = GetInputDS("tpc_tracks");
  if (!tpc) {
    gMessMgr->Warning("No tpc_tracks dataset !");
    return kStWarn;
  }

  Sttptrack = (St_tpt_track *)tpc->Find("tptrack");
  if (!Sttptrack) {
    gMessMgr->Warning("StEstMaker : No TPC tracks !");
    return kStWarn;
  }

  //Getting the TPC evaluation table
  St_tte_eval*   Stevaltrk=0;
  St_g2t_track*   Stg2ttrack=0;
  St_g2t_vertex*   Stg2tvertex=0;
  if (mIdealTracking==1) {
    Stevaltrk = (St_tte_eval *)tpc->Find("evaltrk");
    St_DataSet *geant  = GetInputDS("geant");
    if (geant) {
      Stg2ttrack = (St_g2t_track *)geant->Find("g2t_track");
      Stg2tvertex = (St_g2t_vertex *)geant->Find("g2t_vertex");
    }
    else {
      gMessMgr->Warning("StEstMaker : The geant dataset does not exist, looking for the geantBranch DataSet...");
      St_DataSet *geant2  = GetInputDS("geantBranch");
      Stg2ttrack = (St_g2t_track *)geant2->Find("g2t_track");
      Stg2tvertex = (St_g2t_vertex *)geant2->Find("g2t_vertex");
    }
  }
  if ((!Stevaltrk||!Stg2ttrack||!Stg2tvertex)&&mIdealTracking==1) {
    gMessMgr->Warning("Evaluation table(s) not found ! IdealTracking reset to 0");
    mIdealTracking=0;
  }

  gMessMgr->Info()<<"StEstMaker : IdealTracking="<<mIdealTracking<<" DebugLevel="<<mDebugLevel<<endm;

  gMessMgr->Info()<<"StEstMaker : Constructing a Tracker"<<endm;
  StEstTracker* Tracker = new StEstTracker(mNPass,
					   mNSuperPass,
					   mIdealTracking,
					   mDebugLevel,
					   mParams,
					   mSegments,
					   m_egr_egrpar,
					   m_egrpar_h);

  gMessMgr->Info()<<"StEstMaker : Making the Wafer and Hit objects"<<endm;
  //Tracker->SVTInit(Stsvggeom,
  //		   Stsvgshape,
  //		   Stsvgconf,
  //		   Stscsspt);
  int status;
  status = Tracker->SVTInit(m_geom,
		   Stsvgshape,
		   Stsvgconf,
		   Stscsspt);

  if( status ==1 ) {
    Tracker->CleanUp();
    delete Tracker;
    return kStOK;
  }

  gMessMgr->Info()<<"StEstMaker : Making a Vertex object"<<endm;
  status = Tracker->VertexSetup(preVertex);
  if( status ==1 ) {
    Tracker->CleanUp();
    delete Tracker;
    return kStOk;
  }

  gMessMgr->Info()<<"StEstMaker : Making the Track objects"<<endm;
  status = Tracker->TPCInit(Sttptrack,Sttphit);
  if( status ==1 ) {
    Tracker->CleanUp();
    delete Tracker;
    return kStOk;
  }
  Tracker->BranchInit();

  if (mIdealTracking==1) {
    gMessMgr->Info()<<"StEstMaker : Preparing the evaluation"<<endm;
    Tracker->SetupMc(Stscsspt,
		     Stevaltrk,
		     Stg2ttrack,
		     Stg2tvertex);
  }

  gMessMgr->Info()<<"StEstMaker : Doing the tracking"<<endm;
  Tracker->DoTracking();

  if (mIdealTracking==1) {
    gMessMgr->Info()<<"StEstMaker : Cumulating the evaluation"<<endm;
    Tracker->CumulEval(&mCumulNIdealPrim,
		       &mCumulNIdealSeco,
		       &mCumulNGoodPrim,
		       &mCumulNGoodSeco,
		       &mCumulNBadPrim,
		       &mCumulNBadSeco,
		       &mCumulNEvents);
  }
  gMessMgr->Info()<<"StEstMaker : Saving into tables"<<endm;
  Tracker->EsttoGlobtrk(svttrk,
			svtgrps,
			EstMatch);

  gMessMgr->Info()<<"StEstMaker : Cleaning up"<<endm;
  Tracker->CleanUp();
  delete Tracker;
  return kStOK;

}
