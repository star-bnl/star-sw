extern "C" void ftf_igloc2 ( int   kwkid,   int   ntin, 
				   float x1,      float y1, 
				   float x2,      float y2,
				   int   istat,   char  *un ) ;
extern "C" void ftf_ipl   ( int n, float *x, float *y ) ;
extern "C" void ftf_ipm   ( int n, float *x, float *y ) ;
extern "C" void ftf_ischh ( float height ) ;

extern "C" void ftf_iswn ( int id, float x1, float x2,
			   float y1, float y2 ) ;
extern "C" void  ftf_clear (  ) ; 

extern "C" void ftf_iselnt ( int id ) ;
extern "C" void ftf_isplci ( int color ) ;
extern "C" void ftf_ispmci ( int color ) ;
extern "C" void ftf_igarc ( float f1, float f2, float f3,
						    float f4, float f5, float f6 ) ;
extern "C" void ftf_igbox ( float f1, float f2,
			     float f3, float f4 ) ;
extern "C" void ftf_hfn ( int id, float *x ) ;
extern "C" void ftf_hf1 ( int id, float x, float w ) ;
extern "C" void ftf_irqlc ( int   kwkid,   int   lcdnr, 
					        int   istat,   int   *nt, 
						    float *xpick,  float *ypick ) ;
extern "C" void ftf_term ( ) ;
extern "C" void ftf_istxci ( int color ) ;
extern "C" void ftf_itx ( float x, float y, char *text ) ;
