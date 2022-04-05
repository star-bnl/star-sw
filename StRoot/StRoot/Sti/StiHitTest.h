/** 
 * @file  StiHitTestr.h
 */
#ifndef StiHitTestr_H
#define StiHitTestr_H 1

/** 
 * @class StiHitTest
 * @Hit set testing
 */
class StiHitTest 
{
public:
  StiHitTest(){reset();}
int     getN() const {return fN;}
void    reset();
      double  width (int idx=2);
const double *vector(int idx=2);
const double *center() const {return fX;}
void    add(double x,double y,double z);
void    add(double x[3]);
      double  yAngle () const;
      double  zAngle () const;
private:
  void doIt();
private:
  char fBeg[1];
  int fN;
  double fW[3];
  double fV[3][3];
  double fX[3];
  double fM[3][3];
  char fEnd[1];
  
};
class StiKalmanTrack;
class StiHftHits 
{
public:
static void hftHist(const char *tit, const StiKalmanTrack* tk);
};


#endif

