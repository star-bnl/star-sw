#ifndef FTFGRAPHIC
#define FTFGRAPHIC
#include "BaseTrack_Graphic.h"
#include "FTF_Mc_Track_Graphic.h"
#include "FTF_Hit_Graphic.h"
   
   int const max_cylinders = 10 ;
   int const max_boxes     = 10 ;
//
//    Define cylinder class
//
   class FTF_Cylinder {
   public:
	   FTF_Cylinder ( ) ;
	   void Fill    ( float in_radius, float in_length ) ;
	   void Plot    ( char *view ) ;
   private:
	   float radius ;
	   float length ;
   } ;
//
//     Define box class
//
   class FTF_Box { 
   public:
	   FTF_Box ( ) ;
	   void Fill    ( float x1, float x2,
		              float y1, float y2,
				      float z1, float z2 ) ;
	   void Plot ( char *view ) ;
   private:	
	   float x1, x2 ;
	   float y1, y2 ;
	   float z1, z2 ;
   } ;
//
//       Graphic parameters                  
//
   class FTF_Graphic {
   public:

	void     Data_Set         ( )   ;
	void     Init             ( )   ;
	void     Pick_Point       ( int point_level )   ;
	void     Plot_Detector    ( char *lview, FTF_Para *para ) ;
	void     Plot_Fits        ( int   fst_track, int   no_tracks,      
							    float r_min,     float r_max,      int color ) ;
	void     Plot_Grid        ( char *view, FTF_Para *para ) ;
	void     Plot_Hits        ( )   ;
	void     Plot_Mc_Tracks   ( int fst_track, int no_tracks,     float pt_cut,
							    int color,     float label_scale, int label_pos  ) ;
    void     Plot_Mc_Tracks_Mc( int fst_track, int no_tracks,     float pt_cut,
							    int color,     int pid,           int   ppid ) ;
    void     Plot_Tracks      ( int fst_track, int no_tracks,      float pt_cut,
		   					    int color,     float label_scale,  int label_pos ) ;


	void     Reset           ( )   ;
	void     Select_Area     ( )   ;
	void	 Set_Phase_Space ( float etamin, float etamax,
     						   float phimin, float phimax ) ;
	void     Zoom            ( float factor ) ;
//
//   Some utilities
//
	void   Evaluation                ( ) ;
	void   Fill_Track_Ntuple         ( int hid, FTF_Track *track ) ;
	void   Fill_Performance_Ntuple   ( int hid ) ; 
//
//     Graphic dependent operations
//
	void     clear ( ) ;
	void     get_point            ( float *x, float *y ) ;
	void     define_size          ( float x1, float x2, float y1, float y2 ) ;
	void	 plot_curve           ( int no_hit, float *x, float *y, float *z, 
							        float *r, float *phi, float *eta ) ; 
	void     plot_polymarker      ( int   n,  float *x, float *y ) ;
	void     plot_polyline        ( int   n,  float *x, float *y ) ;
	void     plot_text            ( float *x, char *text, int color ) ;
	int      select               ( float x1, float y1, float x2, float y2 ) ;
	void     set_polymarker_color ( int color ) ;
	void     set_polyline_color   ( int color ) ;
	void	 set_text_height      ( float height ) ;
	

	char     view[5]       ;  /*   View control       */
	short    plot_grid     ;  /*   Plot grid flag     */
	int      kwktyp        ;  /*   Work station type  */
	int      kwkid1        ;  /*   Work station id    */

	float    phi_min       ; /* Minimum phi displayed */
	float    phi_max       ; /* Minimum phi displayed */
	float    eta_min       ; /* Minimum eta displayed */
	float    eta_max       ; /* Maximum eta displayed */
//
//---> View port coordinates
//
	float   xminvp  ;
	float   xmaxvp  ;
	float   yminvp  ;
	float   ymaxvp  ;

	float   xminwn ;
	float   xmaxwn ;
	float   yminwn ;
	float   ymaxwn ;
	float   zminwn ;
	float   zmaxwn ;
	float   rminwn ;
	float   rmaxwn ;

	short int     mhit_color     ;
    short int     hit_errors     ; 
	short int     detector_color ;
//
//      Cylinder properties
//
	int n_cylinders ;
	int n_boxes ;
    FTF_Cylinder cylinder[max_cylinders] ;
	FTF_Box      box     [max_boxes] ;

   };
#endif

