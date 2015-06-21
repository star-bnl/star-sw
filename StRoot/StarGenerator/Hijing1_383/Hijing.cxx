#include "Hijing.h"

#define hijing F77_NAME(hijing, HIJING)
#define hijset F77_NAME(hijset, HIJSET)
#define ulmass F77_NAME(ulmass, ULMASS)
#define lucomp F77_NAME(lucomp, LUCOMP)


extern "C" {
  void hijing( const char *frame, float &bmin, float &bmax, int sframe );
  void hijset( float &root_s,  // energy                                       
	       const char  *frame,   // frame                                  
	       const char  *blue,    // projectile (blue beam)                 
	       const char  *yell,    // target (yellow beam)                   
	       int   &Ablue,   // A of blue beam                               
	       int   &Zblue,   // Z of blue beam                               
	       int   &Ayell,   // A of yellow beam                             
	       int   &Zyell,   // Z of yellow beam                             
	       int    sframe,   // number of characters in "frame"             
	       int    sblue,    // number of characters in "blue"              
	       int    syell );  // number of characters in "yell"   

  float ulmass( int &code );
  int   lucomp( int &code );

};


void Hijset( float E, string frame, string blue, string yell, int Ablue, int Zblue, int Ayell, int Zyell )
{
  int sframe = frame.size();
  int sblue = blue.size();
  int syell = yell.size();

  hijset( E, frame.c_str(), blue.c_str(), yell.c_str(), Ablue, Zblue, Ayell, Zyell, sframe, sblue, syell );
}

void Hijing( string frame, float bmin, float bmax )
{
  int sframe = frame.size();
  hijing( frame.c_str(), bmin, bmax, sframe );
}

float Ulmass( int code )
{
  return ulmass(code);
}

int Lucomp( int code )
{
  return lucomp(code);
};
