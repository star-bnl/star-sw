#ifndef STAR_St_srs_Maker
#define STAR_St_srs_Maker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_srs_Maker virtual base class for Maker                            //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TH2.h"

class St_scs_spt;
class St_svg_config;
class St_svg_shape;
class St_svg_geom;
class St_srs_activea;
class St_srs_srspar;
class St_srs_direct;
class StSvtConfig;
class StSvtCoordinateTransform;
class StSvtHybridCollection;
class TObjectSet;
class TString;

class St_srs_Maker : public StMaker {
 private:
               St_svg_config  *m_config;//!
               St_svg_shape   *m_shape; //!
               St_svg_geom    *m_geom;  //!
               St_srs_activea *m_srs_activea; //!
               St_srs_srspar  *m_srs_srspar; //!
               St_srs_direct  *m_srs_direct; //!
	       StSvtConfig    *mConfig;      //!
	       StSvtHybridCollection *mSvtBadAnodes; //!
	       StSvtCoordinateTransform *mCoordTransform; //!
	       TString        mConfigString;
	       St_ObjectSet  *mSvtAnalSet; //!
	       StSvtHybridCollection *mSvtAnalColl; //!	  
 protected:
	       TH2F     *m_x_vs_y;  //! x vs y of Si points
               TH2F     *m_waf_no1;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no2;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no3;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no4;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no5;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no6;  //! ladder no vs z of Si hit
               TH2F     *m_waf_no7;  //! ladder no vs z of Si hit
 public: 
                  St_srs_Maker(const char *name="svt_hits");
   virtual       ~St_srs_Maker();
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t runnuber);
   virtual Int_t  Make();
   Int_t          setConfig(StSvtConfig* config);
   Int_t          setConfig(const char* config);
   Int_t          SetSvtAnalysis();
   Int_t          GetBadAnodes();
   Int_t          FillHist(St_scs_spt* scs_spt);
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_srs_Maker.h,v 1.12 2007/12/12 22:49:23 fisyak Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_srs_Maker,0)   // chain virtual base class for Makers
};

#endif


