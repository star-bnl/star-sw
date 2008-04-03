#ifndef __StiNodePars_h_
#define __StiNodePars_h_
class StiNodePars {
public:	
void reset(){memset(this,0,sizeof(StiNodePars));_cosCA=1;}
void ready(){_cosCA=cos(_eta);_sinCA=sin(_eta);_curv = _hz*_ptin;}
StiNodePars &merge(double wt,StiNodePars &other);
double  operator[](int idx) const {return P[idx];}
double &operator[](int idx)       {return P[idx];}
int     check(const char *pri=0) const;
void    print() const;

  enum {kX=0,kY,kZ,kEta,kCurv,kTanL};
  /// sine and cosine of cross angle
  double _cosCA;
  double _sinCA;
  union{double P[1];double _x;};
  /// local Y-coordinate of this track (reference plane)           
  double _y; 
  /// local Z-coordinate of this track (reference plane)
  double _z;
  /// (signed curvature)*(local Xc of helix axis - X current point on track)
  double _eta;
  /// signed invert pt [sign = sign(-qB)]
  double _ptin;  
  /// tangent of the track momentum dip angle
  double _tanl;
  /// signed curvature [sign = sign(-qB)]
  double _curv;  
  /// Z component magnetic field in units Pt(Gev) = Hz * RCurv(cm)
  double _hz;  
};
#endif
