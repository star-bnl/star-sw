/***************************************************************************
 *
 * $Id: StEstTracker.h,v 1.3 2001/02/01 12:59:02 lmartin Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTracker
 *
 ***************************************************************************
 *
 * $Log: StEstTracker.h,v $
 * Revision 1.3  2001/02/01 12:59:02  lmartin
 * Correction of the cvs keywords to get the log in the file header.
 * 
 **************************************************************************/
#ifndef STAR_StEstTracker
#define STAR_StEstTracker

#include "StEstConst.h"
#include "StEstMaker.h"
class StEstBranch;
class StEstWafer;
class StEstIndexGeom;
class StEstHit;
class StEstTrack;
class StEstTPCTrack;
class StEstParams;
class StEstSegments;
class St_dst_vertex;
class St_g2t_vertex;
class St_g2t_track;
class St_egr_egrpar;
class St_svg_geom;
class St_svg_shape;
class St_svg_config;
class St_scs_spt;
class St_tpt_track;
class St_tcl_tphit;
class St_tte_eval;
class St_stk_track;
class St_sgr_groups;
class St_svm_evt_match;
#include "table_header.h"


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


class StEstTracker {

 protected:
  // members transmitted by the maker
  int      mPass;           //! current pass number
  int      mNPass;          //! number of passes
  int      mSuperPass;      //! current superpass number 
  int      mNSuperPass;     //! number of superpasses
  int      mIdealTracking;  //! Perfect tracking and evaluation flag
  int      mDebugLevel;     //! Printing level of messages
  StEstParams**    mParams; //!
  StEstSegments**  mSegments;//!
  St_egr_egrpar*   m_egr_egrpar; //!
  table_head_st*   m_egrpar_h;//!
  // real data members
  StEstWafer*      mPreprojTable[MAXFINDWAF];//!
  StEstIndexGeom*  mIndexGeom;//! Objet mapping the wafer position in phi and z
  double mPhiBin; //! size in phi (degree) of a cell for StEstIndexGeom
  int    mNPhiBins; //! number of bins in phi for StEstIndexGeom
  double mZBin; //! size in z (cm) of a cell for StEstIndexGeom
  int    mNZBins; //! number of bins in phi for StEstIndexGeom
  StEstWafer**     mIndexWaf;//!
  StEstHit**       mSvtHit;//!
  StEstHit*        mVertex;//!
  StEstTPCTrack**  mTPCTrack;//!
  StEstTrack**     mTrack;//!
  StEstProjOut     mProjOut;//!
  StEstGtrk*       gtrk; //!
  
  long*            Eval_id_mctrk2est_Track;//!
  StEstHit***      Eval_mchits;//!

  long     mWafId2IndexWaf[9000];  //!
  long*    mTptIndex; //!
  
  
  long     mNTPCTrack;      //! number of TPC tracks
  long     mNTrack;         //! total number of tracks
  long     mNSvtHit;        //! number of SVT hits
  long     mNWafers;        //! number of wafers
  int      mPreprojNumber;  //!

  int  Preprojection(StEstBranch&, int);
  int  Tracking(int);
  int  RefitBranch(StEstBranch *br, StEstHit* hit=NULL, int exclhit=-1, int usevertex=0, int *fitstatus=NULL);
  int  Projection(StEstBranch* branch, long slay);
  void RemoveOneHitTracks();
  void ChooseBestBranch(StEstTrack *tr, int overPass);
  void ChooseBestNBranches(StEstTrack *tr, int slay);
  void RemoveHitSharing();
  void ChooseSegment(int overPass,int layer);
  void BuildIdealBranches();
  void BuildFindableBranches();
  void PrintTrackDetails(int trackid);
  void Eval(int onoffmatrix, int nminhit);
  void FlagTPCTracksSP(int OverPass);
  void FinishFlag();
  void ReInitializeHelix();

  


 public:
  StEstTracker(int mNPass,
	       int mNSuperPass,
	       int mIdealTracking,
	       int mDebugLevel,
	       StEstParams** mParams,
	       StEstSegments** mSegments,
	       St_egr_egrpar* m_egr_egrpar,
	       table_head_st* m_egrpar_h);
  ~StEstTracker();
  long GetNTrack();
  int DoTracking();
  int SVTInit(St_svg_geom*   Stsvggeom,
	      St_svg_shape*   Stsvgshape,
	      St_svg_config*   Stsvgconf,
	      St_scs_spt*   Stscsspt);
  int VertexSetup(St_dst_vertex *preVertex);
  int TPCInit(St_tpt_track* Sttptrack,
	      St_tcl_tphit* Sttphit);
  int BranchInit();
  int SetupMc(St_scs_spt* Stscsspt,
	      St_tte_eval* Stevaltrk,
	      St_g2t_track* Stg2ttrack,
	      St_g2t_vertex* Stg2tvertex);
  void EsttoGlobtrk(St_stk_track* Ststktrk,
		    St_sgr_groups* Stsgrgrps,
		    St_svm_evt_match* StEstMatch);
  void CleanUp();
  ClassDef(StEstTracker, 1)
};
inline long StEstTracker::GetNTrack() {return mNTrack;};
#endif
