#ifndef TTRACKFIT_INC
#define TTRACKFIT_INC
// TTrackFit-includefile
//
//	This class contains all track-fitting-parameters and additional
//	methods to calculate parameters.
//	This class is derived from CObject to support collection in Lists.
//	Normally only one instance of this class is used in tracking. After
//	finding one track, this information is obsolete
//
//	Parameters:
//		int       nhit_tried ;    // Number of hits tried         */
//		int       nvol_tried ;    // Number of volumes tried      */
//		double    s11_xy  ;       // Fit Parameters
//		double    s12_xy  ;
//		double    s22_xy  ;
//		double    g1_xy   ;
//		double    g2_xy   ;       
//		double    s11_sz  ;
//		double    s12_sz  ;
//		double    s22_sz  ;
//		double    g1_sz   ;
//		double    g2_sz   ; 

//		double    dd_xy, a1_xy, a2_xy ;    /*fit par in xy */
//		double    dd_sz, a1_sz, a2_sz ;    /*fit par in sz */
//		double     strack ;

// some defines

// some includes

#include <stdlib.h>
#ifdef LEDA
#include "_memory.hpp"
#endif

// class declaration

class TTrackFit 
{
public:
// all parameters public
   int       nhit_tried ;    // Number of hits tried         */
   int       nvol_tried ;    // Number of volumes tried      */
   float    s11_xy  ;       // Fit Parameters
   float    s12_xy  ;
   float    s22_xy  ;
   float    g1_xy   ;
   float    g2_xy   ;       
   float    s11_sz  ;
   float    s12_sz  ;
   float    s22_sz  ;
   float    g1_sz   ;
   float    g2_sz   ; 

   float    a1_xy, a2_xy ;    /*fit par in xy */
   float    a1_sz, a2_sz ;    /*fit par in sz */
   float     strack ;
   void Reset() 
   {
      nhit_tried = nvol_tried = 0;
      s11_xy = s12_xy = s22_xy = g1_xy = g2_xy = s11_sz = s12_sz = s22_sz = g1_sz = g2_sz =
               a1_xy = a2_xy = a1_sz = a2_sz = 0.0F;
      strack = (float) 0.0;
   };
#ifdef LEDA
   LEDA_MEMORY(TTrackFit);
#endif
};

#endif
