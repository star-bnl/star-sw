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
int    getN() const {return fN;}
void   reset();
double width();
void   add(double x,double y,double z);
void   add(double x[3]);
private:
  int fN;
  double fW;
  double fX[3];
  double fM[3][3];
  
};

#endif

