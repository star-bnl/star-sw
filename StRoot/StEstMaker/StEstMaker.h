/***************************************************************************
 *
 * $Id: StEstMaker.h,v 1.1 2000/12/07 11:14:21 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstMaker
 *
 ***************************************************************************
 *
 * $Log: StEstMaker.h,v $
 * Revision 1.1  2000/12/07 11:14:21  lmartin
 * First CVS commit
 *
 **************************************************************************/
#ifndef STAR_StEstMaker
#define STAR_StEstMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "TH1.h"
#include "TH2.h"
#include "StHelix.hh"
#include <stdlib.h>
#include "StEstConst.h"
#include "Infrastructure/StEstBranch.hh"
#include "Infrastructure/StEstWafer.hh"
#include "Infrastructure/StEstHit.hh"
#include "Infrastructure/StEstTrack.hh"
#include "Infrastructure/StEstTPCTrack.hh"

#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_svg_shape_Table.h"
#include "tables/St_svg_config_Table.h"
#include "tables/St_scs_spt_Table.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_tcl_tphit_Table.h"
#include "tables/St_tte_mctrk_Table.h"
#include "tables/St_tte_eval_Table.h"
#include "tables/St_egr_egrpar_Table.h"
#include "table_header.h"

struct StEstParams{  
  int   debug;          //debuging level 0=only critical errors 1=
  int	onoff[4]; 	//tracking for layer 1= tracking on for layer 0=off
  int	nneighbours[4];	//number of neighbours
  int	nbranch[4];	//number of branches for layer 0=no limit ??? 1=one branch ...
  int	ntotbranch[4];	//number of branches for track after layer ...
  int   maxtpchits;     //number of TPC hits taken to St_TPCTrack object
  int   maxsvthits;     //max number of SVT hits in the track
  int   maxbranches;    //number of branches for the track
  int   maxhitsproj;    //max hits taken in one projection
  int	share[4];	//number of branches for one hit 0=no limit
  double geomcutl[4];	//cut for length in [cm]
  double geomcutw[4];	//cut for width in [cm]
  double ptmin;
  double ptmax;
  double lrad[4][2];       //radii of the cylinders
  double phibin;
  int    nphibins;
  double zbin; 
  int    nzbins; 
};

struct StEstSegments{
  int    slay[4];       // 2=required 1=allowed 0=forbiden hit on super layer
  double chisqcut;      // cut on chi square
  int    minhits;       // minimal number of hits in segment
  double rminTPC;       // minimal r of 1st hit in TPC
  int    minTPChits;    // minimal number of TPC hits
};

struct StEstProjOut {
  int nwaf;                   /* number of wafers in fwafer array */
  int nhit;                   /* number of hits in hit array */
  StEstHit* hit[MAXHITPROJ];/* pointers to hits found in projection */
  double dist[MAXHITPROJ];    /* sqrt(distl^2+distw^2) */
  double distw[MAXHITPROJ];   /* distance between projection point and hit */
  double distl[MAXHITPROJ];   /* distance between projection point and hit */
};

struct StEstGtrk {
  long    nhit;                // ! number of his per track
  long    ipnt[220];          // ! pointers to hits belonging to trk
  long    pos[220];           //! position on a track
  long    det[220];           //! detector hit belongs to
  float   p[9];                 //! track parameters
  long    nfit;                 //! # of points used in fit
  long    flag;                 //! flags from the fitting 
  long    ntpc;                 //! Number of TPC hits
  long    nmax;                 //! # of max points
};

// class for preprojection
class StEstIndexGeom{
private:
  int*** nWaf; //number of wafers

  StEstWafer *****pWaf; 
  int nphibins;
  int nzbins;

public:
  StEstIndexGeom::StEstIndexGeom(int np, int nz) {

    nphibins=np;
    nzbins=nz;

    pWaf = new StEstWafer****[nphibins];
    nWaf = new int**[nphibins];
    for (int i=0;i<nphibins;i++) {
      pWaf[i] = new StEstWafer***[nzbins];
      nWaf[i] = new int*[nzbins];
      for (int j=0;j<nzbins;j++) {
	pWaf[i][j] = new StEstWafer**[4];
	nWaf[i][j] = new int[4];
	for (int k=0;k<4;k++) {
	  pWaf[i][j][k] = new StEstWafer*[MAXWAFINBIN];
	  nWaf[i][j][k] = 0;
	}
      }
    }
  }

  StEstIndexGeom::~StEstIndexGeom() {
    for (int i=0;i<nphibins;i++) {
      for (int j=0;j<nzbins;j++) {
	for (int k=0;k<4;k++) {
	  delete [] pWaf[i][j][k];
	}
	delete [] pWaf[i][j];
	delete [] nWaf[i][j];
      }
      delete [] pWaf[i];
      delete [] nWaf[i];
    }
    delete [] pWaf;
    delete [] nWaf;
  }
  
  int setWafTab(int phi, int z, int slay, StEstWafer* waf) { 
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return 1;
    if(nWaf[phi][z][slay]>=MAXWAFINBIN) return 2;
    pWaf[phi][z][slay][nWaf[phi][z][slay]++]=waf;
    return 0;
  }

  int getNWaf(int phi, int z, int slay) {
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return -1;
    return nWaf[phi][z][slay];
  }

  StEstWafer** getWafTab(int phi, int z, int slay) { 
    if(slay<0||slay>3||z<0||z>=nzbins||phi<0||phi>=nphibins) return NULL;
    return pWaf[phi][z][slay];
  }
  
}; 


class StEstMaker : public StMaker {

 protected:
  
  St_DataSet*        mEvent; //!
  St_DataSetIter*    mEventIter; //!
  StEstWafer*      mPreprojTable[MAXFINDWAF];//!
  StEstParams**    mParams;//!
  StEstSegments**  mSegments;//!
  StEstIndexGeom*  mIndexGeom;//!
  StEstWafer**     mIndexWaf;//!
  StEstHit**       mSvtHit;//!
  StEstHit*        mVertex;//!
  StEstTPCTrack**  mTPCTrack;//!
  StEstTrack**     mTrack;//!
  svg_shape_st*      mSvgShape;//!
  StEstProjOut     mProjOut;//!
  StEstGtrk*       gtrk; //!
  St_egr_egrpar*     m_egr_egrpar; //!
  
  long*              Eval_id_mctrk2est_Track;//!
  StEstHit***      Eval_mchits;//!

  long     mWafId2IndexWaf[9000];  //!
  long*    mTptIndex; //!
  
  
  long     mNTPCTrack;      //! number of TPC tracks
  long     mNTrack;         //! total number of tracks
  long     mNSvtHit;        //! number of SVT hits
  int      mPass;           //! current pass number
  int      mNPass;          //! number of passes
  int      mSuperPass;      //! current superpass 
  int      mNSuperPass;     //! number of superpasses
  int      mStartLayer;     //! start layer
  int      mEndLayer;       //! end layer
  int      mPreprojNumber;  //!
  int      mIdealTracking;  //!
  

  // histogram zone
  
  TH1F*    disthitip0; // distance between ideal hit and projection slay=0
  TH1F*    disthitip1; // distance between ideal hit and projection slay=1
  TH1F*    disthitip2; // distance between ideal hit and projection slay=2
  TH1F*    disthitip3; // distance between ideal hit and projection slay=3
  TH1F*    disthitip0w; // distance between ideal hit and projection slay=0
  TH1F*    disthitip1w; // distance between ideal hit and projection slay=1
  TH1F*    disthitip2w; // distance between ideal hit and projection slay=2
  TH1F*    disthitip3w; // distance between ideal hit and projection slay=3
  TH1F*    disthitip0l; // distance between ideal hit and projection slay=0
  TH1F*    disthitip1l; // distance between ideal hit and projection slay=1
  TH1F*    disthitip2l; // distance between ideal hit and projection slay=2
  TH1F*    disthitip3l; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p0; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p1; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p2; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p3; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p4; // distance between ideal hit and projection slay=3
  TH1F*    chisqib0; // chisq of the ideal branch slay=0
  TH1F*    chisqib1; // chisq of the ideal branch slay=1
  TH1F*    chisqib2; // chisq of the ideal branch slay=2
  TH1F*    chisqib3; // chisq of the ideal branch slay=3

  TH1F*    disthitip3_p; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p_p0; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p_p1; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p_p2; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p_p3; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_p_p4; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s_p0; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s_p1; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s_p2; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s_p3; // distance between ideal hit and projection slay=3
  TH1F*    disthitip3_s_p4; // distance between ideal hit and projection slay=3
  TH1F*    disthitipchisq; // chisq of the ideal track
  TH1F*    disthitipchisql; // linear chisq of the ideal track
  TH1F*    disthitipchisqw; // circular chisq of the ideal track

  TH2F*    Location3; //
  TH2F*    Location2; //
  TH2F*    Location1; //
  TH2F*    Location0; //

  TH1F*    dca_all; // 
  TH1F*    dca_pri; // 
  TH1F*    dca_sec; // 
  TH1F*    Initfitstatus; // 

  TH1F*    IdealPattern; // 
  TH1F*    IdealNHits; // 
  

  TH1F*    mChiLin;
  TH1F*    mChiCir;

  TH1D*    badmaxdist;
  TH1D*    goodmaxdist;  
  
  TH1D*    badchi;    // chisq of bad branches
  TH1D*    goodchi;   // chisq of good branches
  TH1D*    badStep;    // step of bad branches
  TH1D*    goodStep;   // step of good branches
  TH1D*    idealpt;   // pt distribution of ideal tracks
  TH1D*    idealpt_p;   // pt distribution of ideal tracks
  TH1D*    idealpt_s;   // pt distribution of ideal tracks
  TH1D*    Elidealpt;   // pt distribution of ideal electron tracks
  TH1D*    Elidealpt_p;   // pt distribution of ideal electron tracks
  TH1D*    Elidealpt_s;   // pt distribution of ideal electron tracks
  TH1D*    Muidealpt;   // pt distribution of ideal muon tracks
  TH1D*    Muidealpt_p;   // pt distribution of ideal muon tracks
  TH1D*    Muidealpt_s;   // pt distribution of ideal muon tracks
  TH1D*    Piidealpt;   // pt distribution of ideal pion tracks
  TH1D*    Piidealpt_p;   // pt distribution of ideal pion tracks
  TH1D*    Piidealpt_s;   // pt distribution of ideal pion tracks
  TH1D*    Kaidealpt;   // pt distribution of ideal kaon tracks
  TH1D*    Kaidealpt_p;   // pt distribution of ideal kaon tracks
  TH1D*    Kaidealpt_s;   // pt distribution of ideal kaon tracks
  TH1D*    Pridealpt;   // pt distribution of ideal proton tracks
  TH1D*    Pridealpt_p;   // pt distribution of ideal proton tracks
  TH1D*    Pridealpt_s;   // pt distribution of ideal proton tracks
  TH1D*    idealntpc;   // nTPC (number of points) distribution of ideal tracks
  TH1D*    idealrtpc;   // rTPC (radius of the innermost point) distribution of ideal tracks
  TH1D*    goodpt;    // pt distribution of good tracks
  TH1D*    goodpt_p;    // pt distribution of good tracks
  TH1D*    goodpt_s;    // pt distribution of good tracks
  TH1D*    Elgoodpt;    // pt distribution of good electron tracks
  TH1D*    Elgoodpt_p;    // pt distribution of good electron tracks
  TH1D*    Elgoodpt_s;    // pt distribution of good electron tracks
  TH1D*    Mugoodpt;    // pt distribution of good muon tracks
  TH1D*    Mugoodpt_p;    // pt distribution of good muon tracks
  TH1D*    Mugoodpt_s;    // pt distribution of good muon tracks
  TH1D*    Pigoodpt;    // pt distribution of good pion tracks
  TH1D*    Pigoodpt_p;    // pt distribution of good pion tracks
  TH1D*    Pigoodpt_s;    // pt distribution of good pion tracks
  TH1D*    Kagoodpt;    // pt distribution of good kaon tracks
  TH1D*    Kagoodpt_p;    // pt distribution of good kaon tracks
  TH1D*    Kagoodpt_s;    // pt distribution of good kaon tracks
  TH1D*    Prgoodpt;    // pt distribution of good proton tracks
  TH1D*    Prgoodpt_p;    // pt distribution of good proton tracks
  TH1D*    Prgoodpt_s;    // pt distribution of good proton tracks
  TH1D*    badpt;     // pt distribution of bad tracks
  TH1D*    badpt_p;     // pt distribution of bad tracks
  TH1D*    badpt_s;     // pt distribution of bad tracks
  TH1D*    Elbadpt;     // pt distribution of bad electron tracks
  TH1D*    Elbadpt_p;     // pt distribution of bad electron tracks
  TH1D*    Elbadpt_s;     // pt distribution of bad electron tracks
  TH1D*    Mubadpt;     // pt distribution of bad muon tracks
  TH1D*    Mubadpt_p;     // pt distribution of bad muon tracks
  TH1D*    Mubadpt_s;     // pt distribution of bad muon tracks
  TH1D*    Pibadpt;     // pt distribution of bad pion tracks
  TH1D*    Pibadpt_p;     // pt distribution of bad pion tracks
  TH1D*    Pibadpt_s;     // pt distribution of bad pion tracks
  TH1D*    Kabadpt;     // pt distribution of bad kaon tracks
  TH1D*    Kabadpt_p;     // pt distribution of bad kaon tracks
  TH1D*    Kabadpt_s;     // pt distribution of bad kaon tracks
  TH1D*    Prbadpt;     // pt distribution of bad proton tracks
  TH1D*    Prbadpt_p;     // pt distribution of bad proton tracks
  TH1D*    Prbadpt_s;     // pt distribution of bad proton tracks
  TH1D*    efficiency; // efficiency
  TH1D*    idealphi;     // phi distribution of ideal tracks
  TH1D*    goodphi;     // phi distribution of good tracks
  TH1D*    badphi;     // phi distribution of bad tracks
  

  // protected methods

  int  Preprojection(StEstBranch&, int);
  int  Tracking(int);
  int  RefitBranch2(StEstBranch *br, StEstHit* hit=NULL, int exclhit=-1, int usevertex=0, int *fitstatus=NULL);
  int  Projection(StEstBranch* branch, long slay);
  void ReadTables();
  void RemoveOneHitTracks();
  void RemoveOutliers(StEstTrack *tr, int overPass);
  void ChooseBestBranch(StEstTrack *tr, int overPass);
  void ChooseBestNBranches(StEstTrack *tr, int slay);
  void RemoveHitSharing();
  void RemoveHitSharingII();
  void ChooseSegment(int overPass,int layer);
  void BuildIdealBranches();
  void BuildFindableBranches();
  void PrintTrackDetails(int trackid);
  void TrackDebug();
  void Eval(int onoffmatrix, int nminhit);
  void EvalOnLine(int lay);
  void DebugOnLine(int lay);
  void FlagTPCTracksSP(int OverPass);
  void FinishFlag();
  void ReInitializeHelix();
  void PrintSettings();
  void CheckConsistency();

  int SVTInit();
  int TPCInit();
  void VertexSetup();
  void BranchInit();
  int SetupMc();
  
  void CleanUp();

  void EsttoGlobtrk();
  
 public:
  StEstMaker(const char* name = "est");
  ~StEstMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Setup();
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StEstMaker.h,v 1.1 2000/12/07 11:14:21 lmartin Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  Int_t Finish();

  void SetGeomParams(double parl[4][5], double parw[4][5]);

  ClassDef(StEstMaker, 1)
};
#endif






