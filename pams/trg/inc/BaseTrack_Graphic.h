#ifndef  BASETRACKGRAPHIC
#define  BASETRACKGRAPHIC
#include "BaseTrack.h"

//
//    Base Track Graphic class
//
  
  class BaseTrack_Graphic : public BaseTrack {
  public:
	   void    Plot     ( int color, float label_scale, int label_pos ) ;
	   void    Plot_Fit ( float r_min, float r_max, int color ) ;
   } ;
#endif

