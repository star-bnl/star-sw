// StSsdWafer.cc
// Ludovic Gaudichet

# include <stdio.h>
# include "StSsdWafer.hh"
# include <math.h>

StSsdWafer::StSsdWafer()
{
  maxNumHits = 1; mCurrentHit = 0;
  mHits = new localPoint*[maxNumHits];
  mCurrentEvent = 0;
  mIsOn = 1;
 
  mDx = 0.; mDy = 0.; mDz = 0.;
  mAlpha = 0.; mBeta = 0.; mGamma = 0.;

  for (int i=0; i<=maxNumberOfEvents; i++)
    mEventFirstHit[i]=mHits;
};


StSsdWafer::~StSsdWafer()
{
  for(int i=0; i < mCurrentHit; i++) delete mHits[i];
  delete[] mHits; mHits=0;
};


void StSsdWafer::init( double *x, double *d, double *t, double *n,
		       int lay, int lad, int waf, double lX,double lY )
{
  mx0[0] = *x; mx0[1] = *(x+1); mx0[2] = *(x+2);
  md0[0] = *d; md0[1] = *(d+1); md0[2] = *(d+2);
  mt0[0] = *t; mt0[1] = *(t+1); mt0[2] = *(t+2);
  mn0[0] = *n; mn0[1] = *(n+1); mn0[2] = *(n+2);

  mx[0] = mx0[0]; mx[1] = mx0[1]; mx[2] = mx0[2];
  md[0] = md0[0]; md[1] = md0[1]; md[2] = md0[2];
  mt[0] = mt0[0]; mt[1] = mt0[1]; mt[2] = mt0[2];
  mn[0] = mn0[0]; mn[1] = mn0[1]; mn[2] = mn0[2];

  mWaferID  = waf;
  mLadderID = lad;
  mLayerID  = lay;
  mLX = lX;
  mLY = lY;
};


const double StSsdWafer::param(int par) const {
  double val;
  switch (par) {
  case 0 : val = mDx; break;
  case 1 : val = mDy; break;
  case 2 : val = mDz; break;
  case 3 : val = mAlpha; break;
  case 4 : val = mBeta; break;
  case 5 : val = mGamma;
  };
  return val;
};


void  StSsdWafer::shiftParam(int param, double val)
{
  switch (param) {
  case 0 : mDx += val; mx[0] = mx0[0] + mDx; break;
  case 1 : mDy += val; mx[1] = mx0[1] + mDy; break;
  case 2 : mDz += val; mx[2] = mx0[2] + mDz; break;
  case 3 : mAlpha += val; calculD(); calculT(); calculN(); break;
  case 4 : mBeta += val; calculD(); calculT(); calculN(); break;
  case 5 : mGamma += val; calculD(); calculN();
  };
};


void StSsdWafer::setDx(double dx)
{ mDx = dx; mx[0] = mx0[0] + mDx; };

void StSsdWafer::setDy(double dy)
{ mDy = dy; mx[1] = mx0[1] + mDy; };

void StSsdWafer::setDz(double dz)
{ mDz = dz; mx[2] = mx0[2] + mDz; };

void StSsdWafer::setAlpha(double alpha)
{ mAlpha = alpha; calculD(); calculT(); calculN(); };

void StSsdWafer::setBeta(double beta)
{ mBeta = beta; calculD(); calculT(); calculN(); };

void StSsdWafer::setGamma(double gamma)
{ mGamma = gamma; calculD(); calculN(); };

void StSsdWafer::calculD()
{
  double c1 = cos(mGamma)*cos(mAlpha)+sin(mGamma)*sin(mBeta)*sin(mAlpha);
  double c2 = sin(mGamma)*sin(mBeta)*cos(mAlpha)-cos(mGamma)*sin(mAlpha);
  double c3 = -sin(mGamma)*cos(mBeta);
  md[0] = c1*md0[0] + c2*mt0[0] + c3*mn0[0];
  md[1] = c1*md0[1] + c2*mt0[1] + c3*mn0[1];
  md[2] = c1*md0[2] + c2*mt0[2] + c3*mn0[2];
};

void StSsdWafer::calculT()
{
  double c1 = cos(mBeta)*sin(mAlpha);
  double c2 = cos(mBeta)*cos(mAlpha);
  double c3 = sin(mBeta);
  mt[0] = c1*md0[0] + c2*mt0[0] + c3*mn0[0];
  mt[1] = c1*md0[1] + c2*mt0[1] + c3*mn0[1];
  mt[2] = c1*md0[2] + c2*mt0[2] + c3*mn0[2];
};


void StSsdWafer::calculN()
{
  double c1 = sin(mGamma)*cos(mAlpha)-sin(mBeta)*sin(mAlpha)*cos(mGamma);
  double c2 = -cos(mGamma)*sin(mBeta)*cos(mAlpha)-sin(mGamma)*sin(mAlpha);
  double c3 = cos(mGamma)*cos(mBeta);
  mn[0] = c1*md0[0] + c2*mt0[0] + c3*mn0[0];
  mn[1] = c1*md0[1] + c2*mt0[1] + c3*mn0[1];
  mn[2] = c1*md0[2] + c2*mt0[2] + c3*mn0[2];
};


int StSsdWafer::addNewHit( localPoint *local )
{
  if  ((mCurrentHit == maxNumHits)&&(mIsOn))
    {      
      //localPoint *newPt[maxNumHits];
      maxNumHits = int(maxNumHits*1.2+10);
      localPoint **newPt = new localPoint *[maxNumHits];
      
      for (int i=0; i<mCurrentHit; i++)
	newPt[i] = mHits[i];

      for (int j=0; j<=maxNumberOfEvents; j++)
        mEventFirstHit[j] = newPt + (mEventFirstHit[j]-mHits);
      
      delete[] mHits;
      mHits = newPt;
      
      if (mCurrentHit==0)  mEventFirstHit[0] = newPt;
      
    };

  if ( mIsOn )
    {
      mHits[mCurrentHit] = new localPoint;
      mHits[mCurrentHit]->X = local->X;
      mHits[mCurrentHit]->Y = local->Y;
      mHits[mCurrentHit]->Z = local->Z;
      mCurrentHit ++;
      return 1;
    }
  else return 0;
};


int StSsdWafer::addNewHit( globalPoint *global )
{
  localPoint local;
  global2Local( global, &local);
  if ( ( local.X < mLX )&&( local.X > -mLX )&&
       ( local.Y < mLY )&&( local.Y > -mLY)&&
       ( local.Z < 0.1 )&&( local.Z > -0.1) )
    return ( addNewHit(&local) );
      else 
	return 0;
};


int StSsdWafer::global2Local( globalPoint* global , localPoint *local )
{
  double xl[3];
  
  xl[0] = global->x - mx[0];
  xl[1] = global->y - mx[1];
  xl[2] = global->z - mx[2];
  
  local->X = xl[0]*md[0] + xl[1]*md[1] + xl[2]*md[2];
  local->Y = xl[0]*mt[0] + xl[1]*mt[1] + xl[2]*mt[2];
  local->Z = xl[0]*mn[0] + xl[1]*mn[1] + xl[2]*mn[2]; 
  
  return 1;
};


int StSsdWafer::local2Global( localPoint *local, globalPoint* global)
{
  double xl[3];
  
  xl[0] = local->X;
  xl[1] = local->Y;
  xl[2] = local->Z;
  
global->x = mx[0] + xl[0]*md[0] + xl[1]*mt[0] + xl[2]*mn[0]; 
global->y = mx[1] + xl[0]*md[1] + xl[1]*mt[1] + xl[2]*mn[1];
global->z = mx[2] + xl[0]*md[2] + xl[1]*mt[2] + xl[2]*mn[2]; 

  return 1;
};


localPoint StSsdWafer::pointOnWafer( globalPoint *p1, globalPoint *p2 )
{
  localPoint P1, P2;
  global2Local( p1, &P1 );
  global2Local( p2, &P2 );

  localPoint P;
  P.Z = 0.;
  P.X = P1.X - (P2.X - P1.X)*P1.Z/(P2.Z - P1.Z);
  P.Y = P1.Y - (P2.Y - P1.Y)*P1.Z/(P2.Z - P1.Z);

  return P;
};


int StSsdWafer::isTouched( globalPoint *vertex, globalPoint *p2, localPoint *P )
{
  localPoint P1, P2;
  global2Local( vertex, &P1 );
  global2Local( p2, &P2 );

  P->Z = 0.;
  P->X = P1.X - (P2.X - P1.X)*P1.Z/(P2.Z - P1.Z);
  P->Y = P1.Y - (P2.Y - P1.Y)*P1.Z/(P2.Z - P1.Z);

  globalPoint pOnWafer;
  local2Global(P, &pOnWafer);

  int touched;
  double dist1 = (pOnWafer.x-p2->x)*(pOnWafer.x-p2->x) + (pOnWafer.y-p2->y)*(pOnWafer.y-p2->y) +
                 (pOnWafer.z-p2->z)*(pOnWafer.z-p2->z);
  double dist2 = (vertex->x-p2->x)*(vertex->x-p2->x)+(vertex->y-p2->y)*(vertex->y-p2->y) + 
                 (vertex->z-p2->z)*(vertex->z-p2->z);
  double dist3 = (vertex->x-pOnWafer.x)*(vertex->x-pOnWafer.x)+(vertex->y-pOnWafer.y)*(vertex->y-pOnWafer.y) + 
    (vertex->z-pOnWafer.z)*(vertex->z-pOnWafer.z);


  touched = ( ( fabs(P->X)<mLX )&&( fabs(P->Y)<mLY )&&
	      ( (dist1<dist2)||(dist1<dist3) )&& // vertex mustn't be between pOnWafer and p2
	      (mIsOn) );  // wafer have to be on
	   
return touched;
};


int  StSsdWafer::recordEvent()
{
  int exit = (mCurrentEvent<maxNumberOfEvents);
  if (exit)
    {
      mCurrentEvent++;
      mEventFirstHit[mCurrentEvent] = &mHits[mCurrentHit];
    };
  return exit;
};


const globalPoint StSsdWafer::hit(int event, int nhit)
{
  globalPoint p;
  if ((event<mCurrentEvent)&&(nhit<mCurrentHit))
    {
      local2Global( *(mEventFirstHit[event]+nhit), &p );
    };
  return p;
};


void StSsdWafer::speedHit(int event, int nhit, globalPoint *p)
{
  if ((event<mCurrentEvent)&&(nhit<mCurrentHit))
    {
      double xl0 = (*(mEventFirstHit[event]+nhit))->X;
      double xl1 = (*(mEventFirstHit[event]+nhit))->Y;
      double xl2 = (*(mEventFirstHit[event]+nhit))->Z;
      p->x = mx[0] + xl0*md[0] + xl1*mt[0] + xl2*mn[0]; 
      p->y = mx[1] + xl0*md[1] + xl1*mt[1] + xl2*mn[1];
      p->z = mx[2] + xl0*md[2] + xl1*mt[2] + xl2*mn[2]; 
    };
};


int StSsdWafer::clotherHit( int event, globalPoint* p1, globalPoint* p2, float threshold )
{
 localPoint P;
 P = pointOnWafer(p1, p2);
 double L, Lmin = 1000.;
 int iMin =-1;
 int i=0;

 for (localPoint **hit = mEventFirstHit[event]; hit < mEventFirstHit[event+1]; hit++)
    {
     L = (P.X - (*hit)->X)*(P.X - (*hit)->X) + (P.Y - (*hit)->Y)*(P.Y - (*hit)->Y) +
         (P.Z - (*hit)->Z)*(P.Z - (*hit)->Z);

     if (L<Lmin)
       {
        Lmin = L;
        iMin = i;
        };

     i++;
     };
 if ( Lmin<threshold*threshold ) return iMin; else return (-1);
 };


void StSsdWafer::printState()
{
  printf("wafer (barrel, ladder, wafer) = (%d,%d, %d)\n", mLayerID, mLadderID, mWaferID);
  printf("lx=%f ly=%f\n",mLX, mLY);
  printf("Number of event(s) : %d\n",mCurrentEvent);
  printf("Number of hit(s)   : %d\n\n",mCurrentHit);

  printf("first point  =       %.15f , %.15f , %.15f\n", hit(0, 0).x, hit(0, 0).y, hit(0, 0).z );
  printf("wafer center :       %.15f , %.15f , %.15f\n", mx[0], mx[1], mx[2]);
  printf("drift direction :    %.15f , %.15f , %.15f\n", md[0], md[1], md[2]);
  printf("transverse to drift: %.15f , %.15f , %.15f\n", mt[0], mt[1], mt[2]);
  printf("normal to wafer :    %.15f , %.15f , %.15f\n", mn[0], mn[1], mn[2]);
  printf("Dx = %f\n",mDx);
  printf("Dy = %f\n",mDy);
  printf("Dz = %f\n",mDz);
  printf("alpha = %f\n",mAlpha);
  printf("beta  = %f\n",mBeta);
  printf("gamma = %f\n\n",mGamma);
};
