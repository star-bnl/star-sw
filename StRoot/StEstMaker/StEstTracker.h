/***************************************************************************
 *
 * $Id: StEstTracker.h,v 1.11 2002/04/30 22:49:19 caines Exp $
 *
 * Author: PL,AM,LM,CR (Warsaw,Nantes)
 ***************************************************************************
 *
 * Description: Header file of StEstTracker
 *
 ***************************************************************************
 *
 * $Log: StEstTracker.h,v $
 * Revision 1.11  2002/04/30 22:49:19  caines
 * Make est work with shifted SVT geom, change search radii to 1cm
 *
 * Revision 1.10  2001/07/15 20:31:31  caines
 * Fixes from Insure++ debugging
 *
 * Revision 1.9  2001/04/25 17:33:36  perev
 * HPcorrs
 *
 * Revision 1.8  2001/03/19 16:06:18  lmartin
 * References to McTrack objects removed.
 *
 * Revision 1.7  2001/03/13 19:14:36  didenko
 * check commit
 *
 * Revision 1.6  2001/03/13 08:20:38  lmartin
 * essai
 *
 * Revision 1.5  2001/03/02 16:18:10  lmartin
 * Data members added to store the cumulated numbers of ideal,good and bad tracks.
 * CumulEval method added.
 *
 * Revision 1.4  2001/02/23 13:46:13  lmartin
 * Two arguments (hittmp,exclhit) of the RefitBranch method removed.
 *
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
class StSvtGeometry;
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
  int    nhit;                // ! number of his per track
  int    ipnt[220];          // ! pointers to hits beinting to trk
  int    pos[220];           //! position on a track
  int    det[220];           //! detector hit beints to
  float   p[9];                 //! track parameters
  int    nfit;                 //! # of points used in fit
  int    flag;                 //! flags from the fitting 
  int    ntpc;                 //! Number of TPC hits
  int    nmax;                 //! # of max points
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
  
  int*            Eval_id_mctrk2est_Track;//!
  StEstHit***      Eval_mchits;//!

  int     mWafId2IndexWaf[9000];  //!
  int*    mTptIndex; //!
  
  
  int     mNTPCTrack;      //! number of TPC tracks
  int     mNTrack;         //! total number of tracks
  int     mNSvtHit;        //! number of SVT hits
  int     mNWafers;        //! number of wafers
  int      mPreprojNumber;  //!
  
  int    mNIdealPrim; //! number of Ideal primary track in the event
  int    mNIdealSeco; //! number of Ideal primary track in the event
  int    mNGoodPrim; //! number of Good primary track in the event
  int    mNGoodSeco; //! number of Good primary track in the event
  int    mNBadPrim; //! number of Bad primary track in the event
  int    mNBadSeco; //! number of Bad primary track in the event

  int  Preprojection(StEstBranch*, int); //int  Preprojection(StEstBranch&, int);
  int  Tracking(int);
  int  RefitBranch(StEstBranch *br, int usevertex=0, int *fitstatus=NULL);
  int  Projection(StEstBranch* branch, int slay);
  void RemoveOneHitTracks();
  void ChooseBestBranch(StEstTrack *tr, int overPass);
  void ChooseBestNBranches(StEstTrack *tr, int slay);
  void RemoveHitSharing();
  void RemoveHitSharing2();
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
  int GetNTrack();
  int GetIdealPrim() {return mNIdealPrim;};
  int GetIdealSeco() {return mNIdealSeco;};
  int GetGoodPrim() {return mNGoodPrim;};
  int GetGoodSeco() {return mNGoodSeco;};
  int GetBadPrim() {return mNBadPrim;};
  int GetBadSeco() {return mNBadSeco;};
  float GetVertexZ(); 

  int DoTracking();
  //  int SVTInit(St_svg_geom*   Stsvggeom,
  //	      St_svg_shape*   Stsvgshape,
  //	      St_svg_config*   Stsvgconf,
  //	      St_scs_spt*   Stscsspt);
  int SVTInit(StSvtGeometry*   svggeom,
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
  void CumulEval(int* mCumulNIdealPrim,
		 int* mCumulNIdealSeco,
		 int* mCumulNGoodPrim,
		 int* mCumulNGoodSeco,
		 int* mCumulNBadPrim,
		 int* mCumulNBadSeco,
		 int* mCumulNEvents);
  void CleanUp();
  void AlignmentInfo();

  ClassDef(StEstTracker, 1)
};

inline int StEstTracker::GetNTrack() {return mNTrack;};
#endif
