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

class St_svg_config;
class St_svg_shape;
class St_svg_geom;
class St_srs_activea;
class St_srs_srspar;
class St_srs_direct;

class St_srs_Maker : public StMaker {
 private:
               St_svg_config  *m_config;//!
               St_svg_shape   *m_shape; //!
               St_svg_geom    *m_geom;  //!
               St_srs_activea *m_srs_activea; //!
               St_srs_srspar  *m_srs_srspar; //!
               St_srs_direct  *m_srs_direct; //!
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
   virtual Int_t  Make();
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: St_srs_Maker.h,v 1.7 1999/08/08 19:47:42 caines Exp $ built "__DATE__" "__TIME__ ; return cvs;}

   ClassDef(St_srs_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


