extern "C" void TrgIgloc2 ( int   kwkid,   int   ntin, 
				   float x1,      float y1, 
				   float x2,      float y2,
				   int   istat,   char  *un ) ;
extern "C" void TrgIpl   ( int n, float *x, float *y ) ;
extern "C" void TrgIpm   ( int n, float *x, float *y ) ;
extern "C" void TrgIschh ( float height ) ;

extern "C" void TrgIswn ( int id, float x1, float x2,
			   float y1, float y2 ) ;
extern "C" void TrgIsvp ( int id, float x1, float x2,
                           float y1, float y2 ) ;
extern "C" void  TrgClear (  ) ; 

extern "C" void TrgIselnt ( int id ) ;
extern "C" void TrgIsplci ( int color ) ;
extern "C" void TrgIspmci ( int color ) ;
extern "C" void TrgIgarc ( float f1, float f2, float f3,
						    float f4, float f5, float f6 ) ;
extern "C" void TrgIgbox ( float f1, float f2,
			     float f3, float f4 ) ;
extern "C" void TrgHfn ( int id, float *x ) ;
extern "C" void TrgHf1 ( int id, float x, float w ) ;
extern "C" void TrgIrqlc ( int   kwkid,   int   lcdnr, 
					        int   istat,   int   *nt, 
						    float *xpick,  float *ypick ) ;
extern "C" void TrgTerm ( ) ;
extern "C" void TrgIstxci ( int color ) ;
extern "C" void TrgItx ( float x, float y, char *text ) ;
