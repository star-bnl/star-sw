#ifndef STAR_St_sce_Maker
#define STAR_St_sce_Maker
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_sce_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class TFile;
class TH1F;
class St_sdm_geom_par;
class St_svg_geom;
class St_sce_ctrl;

class St_sce_Maker : public StMaker {
 private:
            St_sdm_geom_par *m_geom_par;//!
	    St_svg_geom     *m_geom;//!
	    St_sce_ctrl     *m_ctrl;//!
 	    void resetSceStats(); //!
 	    void makeScfStats(); //!
 	    void showScfStats(); //!
 	    void makeScmStats(); //!
 	    void showScmStats(); //!
 	    void makeScmHistograms(); //!
 	    void writeScmHistograms(); //!

 protected:

	    TFile *ScmFile; //!
	    TH1F  *devXl0;  //! distribution of local length deviation.
	    TH1F  *devXl1;  //! distribution of local width deviation.
	    TH1F  *devNrg;  //! distribution of energy deviation.
	    TH1F  *devXg0;  //! distribution of global x deviation.
	    TH1F  *devXg1;  //! distribution of global y deviation.
	    TH1F  *devXg2;  //! distribution of global z deviation.
	    Int_t statCluster[3][2]; //! statistique sur les clusters.
	    Int_t statSpt[3][5]; //! statistique sur les space points.

 public: 
	       St_sce_Maker(const char *name="sce_dspt");
   virtual       ~St_sce_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   PrintInfo();
   ClassDef(St_sce_Maker, 1)   //StAF chain virtual base class for Makers
};
#endif
