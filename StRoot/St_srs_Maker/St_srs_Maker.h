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
 public: 
                  St_srs_Maker(const char *name="svt_hits");
   virtual       ~St_srs_Maker();
   virtual Int_t  Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_srs_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


