#include "Hijing.h"

#define hijing F77_NAME(hijing, HIJING)
#define hijset F77_NAME(hijset, HIJSET)
#define ulmass F77_NAME(ulmass, ULMASS)
#define lucomp F77_NAME(lucomp, LUCOMP)

#include <StMessMgr.h>

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


void Hijset() {
  Hijset(0,"init","the","generator",0,0,0,0);
};

void Hijset( float E, string frame, string blue, string yell, int Ablue, int Zblue, int Ayell, int Zyell )
{

  // Lazy initialization of hijing
  static int called = 0;
  static float  _E;
  static string _frame;
  static string _blue;
  static string _yell;
  static int    _ablue;
  static int    _zblue;
  static int    _ayell;
  static int    _zyell;
  
  if ( 0 == called ) {             // capture the arguement list on the first call
    _E     = E;
    _frame = frame;
    _blue  = blue;
    _yell  = yell;
    _ablue = Ablue;
    _zblue = Zblue;
    _ayell = Ayell;
    _ablue = Zblue;
    LOG_INFO << "Hijset captured " << _frame << " " << _blue << " " << _yell << endm;
    ++called;
  }
  else if ( 1 == called ) {        // perform actual initialization on subsequent call
    int sframe = _frame.size();
    int syell  = _yell.size();
    int sblue  = _blue.size();
    LOG_INFO << "Hijset configure E=" << _E << " " << _frame << " " << _blue << " " << _yell << endm;
    hijset( _E, _frame.c_str(), _blue.c_str(), _yell.c_str(), _ablue, _zblue, _ayell, _zyell, sframe, sblue, syell );
    ++called;
  } 
  else {                           // log additional calls, but don't change the generator state

    LOG_INFO << "Additional call to Hijset is ignored" << endm;

  }
  
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
