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
class St_svg_config;
class St_svg_shape;
class St_svg_geom;
class St_srs_activea;
class St_srs_srspar;
class St_srs_direct;

class St_srs_Maker : public StMaker {
 private:
               Bool_t drawinit;
               St_svg_config  *m_config;//!
               St_svg_shape   *m_shape; //!
               St_svg_geom    *m_geom;  //!
               St_srs_activea *m_srs_activea; //!
               St_srs_srspar  *m_srs_srspar; //!
               St_srs_direct  *m_srs_direct; //!
 protected:
 public: 
                  St_srs_Maker();
                  St_srs_Maker(const char *name, const char *title);
   virtual       ~St_srs_Maker();
   virtual Int_t Init();
   virtual Int_t  Make();
   virtual void   PrintInfo();
   ClassDef(St_srs_Maker, 1)   //StAF chain virtual base class for Makers
};

#endif
