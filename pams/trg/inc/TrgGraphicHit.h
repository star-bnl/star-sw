#ifndef TRGGRAPHICHIT
#define TRGGRAPHICHIT
   
class TrgGraphic ;
class TrgGraphicTrack ;

class TrgGraphicHit {         
public:
    TrgGraphicHit ( ) : track(0) { } ;
    void plot ( TrgGraphic *gr, int color ) ;
    void print ( int level ) ;

    int    I_r ( ) { return i_r ; } ;
    float  X   ( ) { return x   ; } ;
    float  Y   ( ) { return y   ; } ;
    float  Z   ( ) { return z   ; } ;
    float  R   ( ) { return r   ; } ;
    float  Eta ( ) { return eta ; } ;
    float  Phi ( ) { return phi ; } ;

    void   set ( int iid, 
                 float  xx, float  yy, float  zz, 
                 float dxx, float dyy, float dzz, int rrow ) ;
    void   Track ( TrgGraphicTrack* tr ) { track = tr ; } ;
private:
    int              id    ;     // Primary key 
    short            i_r   ;     // Row #     
    TrgGraphicTrack* track ;     // Track
    float            dx    ;     // error on the x coordinate  
    float            dy    ;     // error on the y coordinate  
    float            dz    ;     // error on the z coordinate  
    float            s     ;     // Track trajectory           
    float            r     ;     // radius                     
    float            phi   ;     // azimuthal angle            
    float            eta   ;     // hit pseudorapidity         
    float            x     ;     // reconstructed x coordinate 
    float            y     ;     // reconstructed y coordinate 
    float            z     ;     // reconstructed z coordinate 
} ;
#endif

