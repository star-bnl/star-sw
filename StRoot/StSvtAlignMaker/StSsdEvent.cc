// StSsdEvent.cc
// Ludovic Gaudichet

#include "StSsdEvent.hh"
#include <math.h>
// #include "StSvtClassLibrary/StSvtConfig.hh"
// #include "StDbUtilities/StSvtCoordinateTransform.hh"
// #include "StDbUtilities/StSvtLocalCoordinate.hh"
// #include "StDbUtilities/StGlobalCoordinate.hh"


StSsdEvent::StSsdEvent()
{
  mNumTracks = 0;
  maxNumTracks = 1;
  mTracks = new track*[maxNumTracks];
  mVertex.z = 10000.;
};


StSsdEvent::~StSsdEvent()
{
  for(int i=0; i < mNumTracks; i++) delete mTracks[i];
  delete[] mTracks; mTracks=0;
};


int StSsdEvent::addTrack(track newOne)
{
  if (mNumTracks == maxNumTracks)
    {
      //track *newTr[3000];
      maxNumTracks = int(maxNumTracks*1.2+10);
      track **newTr= new track*[maxNumTracks];

      for (int i=0; i<mNumTracks; i++)
	newTr[i] = mTracks[i];
      delete[] mTracks;
      mTracks = newTr ;
    };

  int np = newOne.numberOfHits - 1;
  newOne.b.x = newOne.p[np].x - newOne.p[0].x;
  newOne.b.y = newOne.p[np].y - newOne.p[0].y;
  newOne.b.z = newOne.p[np].z - newOne.p[0].z;
  
  double lambda =  ( newOne.p[0].x*newOne.b.x + newOne.p[0].y*newOne.b.y )/
    ( newOne.b.x*newOne.b.x + newOne.b.y*newOne.b.y  );
  
  newOne.a.x = newOne.p[0].x - lambda*newOne.b.x;
  newOne.a.y = newOne.p[0].y - lambda*newOne.b.y;
  newOne.a.z = newOne.p[0].z - lambda*newOne.b.z;


  mTracks[mNumTracks] = new track;
  *mTracks[mNumTracks] = newOne;
  mNumTracks++;
  return 1;
};


track* StSsdEvent::getTrack(int trackNumber)
{
  if (trackNumber<mNumTracks)
    return mTracks[trackNumber];
  else return 0;
};


// defines a and b vectors with the help of least square method
// calculates the chi^2. :
double StSsdEvent::chi2ab_Np( globalPoint &aa, globalPoint &bb, globalPoint *pp0, int N,
			      globalPoint &p, int i_p)
{
  double l[16];
  double c = bb.x*bb.x + bb.y*bb.y, e, g, f, h;
  int i;

  globalPoint *pgp = pp0; 
  for ( i = 0; i<i_p; i++) {
    l[i] = (pgp->x*bb.x + pgp->y*bb.y)/c; pgp++;
  };
  l[i_p] = (p.x*bb.x + p.y*bb.y)/c; pgp++;
  for ( i = i_p+1; i<N; i++) {
    l[i] = (pgp->x*bb.x + pgp->y*bb.y)/c; pgp++;
  };

  c=0; e=0;
  for ( i = 0; i<i_p; i++) {
    c += l[i]*l[i]; e += l[i];
  };
  c += l[i_p]*l[i_p]; e += l[i_p];
  for ( i = i_p+1; i<N; i++) {
    c += l[i]*l[i]; e += l[i];
  };
  
  g = N*c - e*e;
//--
  pgp = pp0;
  f=0; h=0;
  for ( i=0; i<i_p; i++) {
      f += l[i]*pgp->z; h += pgp->z; pgp++;
    };
  f += l[i_p]*p.z;
  h += p.z;
  pgp++;
  for ( i=i_p+1; i<N; i++) {
      f += l[i]*pgp->z; h += pgp->z; pgp++;
    };
  aa.z = (c*h - f*e)/g;
  bb.z = (N*f - e*h)/g;
//--
  pgp = pp0;
  f=0; h=0;
  for ( i=0; i<i_p; i++) {
      f += l[i]*pgp->x; h += pgp->x; pgp++;
    };
  f += l[i_p]*p.x;
  h += p.x;
  pgp++;
  for ( i=i_p+1; i<N; i++) {
      f += l[i]*pgp->x; h += pgp->x; pgp++;
    };
  aa.x = (c*h - f*e)/g;
  bb.x = (N*f - e*h)/g;
//--
  pgp = pp0;
  f=0; h=0;
  for ( i=0; i<i_p; i++) {
      f += l[i]*pgp->y; h += pgp->y; pgp++;
    };
  f += l[i_p]*p.y;
  h += p.y;
  pgp++;
  for ( i=i_p+1; i<N; i++) {
      f += l[i]*pgp->y; h += pgp->y; pgp++;
    };
  aa.y = (c*h - f*e)/g;
  bb.y = (N*f - e*h)/g;


  c = (aa.x*bb.x + aa.y*bb.y)/(bb.x*bb.x + bb.y*bb.y);
  aa.x -= c*bb.x;  aa.y	-= c*bb.y;  aa.z -= c*bb.z;

  c = bb.x*bb.x + bb.y*bb.y;

  pgp = pp0; 
  for ( i = 0; i<i_p; i++) {
    l[i] = (pgp->x*bb.x + pgp->y*bb.y)/c; pgp++;
  };
  l[i_p] = (p.x*bb.x + p.y*bb.y)/c; pgp++;
  for ( i = i_p+1; i<N; i++) {
    l[i] = (pgp->x*bb.x + pgp->y*bb.y)/c; pgp++;
  };

  double chi2=0;
  pgp = pp0;
  for ( i = 0; i<i_p; i++)
    {
      chi2 += (aa.x + l[i]*bb.x - pgp->x)*(aa.x + l[i]*bb.x - pgp->x)+
	      (aa.y + l[i]*bb.y - pgp->y)*(aa.y + l[i]*bb.y - pgp->y)+
	      (aa.z + l[i]*bb.z - pgp->z)*(aa.z + l[i]*bb.z - pgp->z);
      pgp++;
    };
  chi2 += (aa.x + l[i_p]*bb.x - p.x)*(aa.x + l[i_p]*bb.x - p.x)+
          (aa.y + l[i_p]*bb.y - p.y)*(aa.y + l[i_p]*bb.y - p.y)+
          (aa.z + l[i_p]*bb.z - p.z)*(aa.z + l[i_p]*bb.z - p.z);
  pgp++;
  for ( i = i_p+1; i<N; i++)
    {
      chi2 += (aa.x + l[i]*bb.x - pgp->x)*(aa.x + l[i]*bb.x - pgp->x)+
	      (aa.y + l[i]*bb.y - pgp->y)*(aa.y + l[i]*bb.y - pgp->y)+
	      (aa.z + l[i]*bb.z - pgp->z)*(aa.z + l[i]*bb.z - pgp->z);
      pgp++;
    };

  return chi2;
};


int StSsdEvent::trackHitNumber(int track, int wafer)
{
  int result = -1;
  for (int i=0; (i<mTracks[track]->numberOfHits)&&(result==-1); i++)
    if (mTracks[track]->waferID[i] == wafer)
      result = i;

  return result;
};


int StSsdEvent::pointID(int track, int hitNumber)
{
  return ( mTracks[track]->pointID[hitNumber] );
};


double StSsdEvent::chi2DiffPerHit(int itrack)
{
  double val = (mTracks[itrack]->chi2_2 - mTracks[itrack]->chi2_1)/
    ((double)mTracks[itrack]->numberOfHits+1.); // +1 => vertex

  return val;
};


//--------------------------------------------------------------------------------------
// 'Au-Au alignment' : with primary vertex constraint


globalPoint StSsdEvent::getVertex()
{
  if ( mVertex.z > 1000.) processTracks();
  return mVertex;
};


int StSsdEvent::setChi2(int ichi2, int track, int hitnumber, globalPoint &p)
{
  double chi2 = chi2ab_Np( mTracks[track]->a, mTracks[track]->b,
			   &(mTracks[track]->p[0]), mTracks[track]->numberOfHits,
			   p, hitnumber );
  
  double ax = mVertex.x - mTracks[track]->a.x;
  double ay = mVertex.y - mTracks[track]->a.y;
  double az = mVertex.z - mTracks[track]->a.z;
  double bx = mTracks[track]->b.x;
  double by = mTracks[track]->b.y;
  double bz = mTracks[track]->b.z;
  chi2 += ax*ax + ay*ay + az*az - 
    (ax*bx + ay*by + az*bz)*(ax*bx + ay*by + az*bz)/(bx*bx + by*by + bz*bz);

  if (ichi2==1) mTracks[track]->chi2_1 = chi2;
  else  mTracks[track]->chi2_2 = chi2;
  return 1;
};


double StSsdEvent::processTracks()
{
  int i;
  for(i = 0; i < mNumTracks; i++)
    {
      mTracks[i]->chi2_1 = chi2ab_Np( mTracks[i]->a, mTracks[i]->b, &(mTracks[i]->p[0]), mTracks[i]->numberOfHits,
		 mTracks[i]->p[0], 0);
    };
//-
  globalPoint aa,bb;
  double c, e, g;

  const int ncase = 700; // intervalle de 2mm entre +-70cm
  const double VERTEX_LENGTH = 70.;
  int j=0, m=0, n[ncase]; 

  for (i=0; i<ncase; i++) n[i]=0;
  for ( i = 0; i<mNumTracks; i++ )
    if ( fabs(mTracks[i]->a.z)<VERTEX_LENGTH )
      n[ int(5.*(mTracks[i]->a.z+VERTEX_LENGTH)) ]++;
  
  for (i=0; i<ncase; i++)
    if (m<n[i]) {j=i; m=n[i];};
  double zv = - VERTEX_LENGTH + j/5.+ 0.1; //z est place au milieu de la case

    for ( i = 0; i<mNumTracks; i++ )
      {
	if (mTracks[i]->numberOfHits == 3)
	  m = ( mTracks[i]->chi2_1/3.<0.005 );//0.005
	else
	  m = ( mTracks[i]->chi2_1/4.<0.005 );
	mTracks[i]->flag = ( (m) && (fabs(mTracks[i]->a.z-zv)<0.22) );//0.22
      };
//-

    double c0=0., c1=0., c2=0., c3=0., c4=0., c5=0.;
    double d0=0., d1=0., d2=0.;

    for(i = 0; i < mNumTracks; i++)
      {	      
	if( ! mTracks[i]->flag ) continue;
	
	aa = mTracks[i]->a;
	bb = mTracks[i]->b;
	e = bb.x*bb.x + bb.y*bb.y + bb.z*bb.z;
	g = aa.x*bb.x + aa.y*bb.y + aa.z*bb.z;
	c0 += 1. - bb.z*bb.z/e;
	c1 -= bb.z*bb.x/e;
	c2 -= bb.z*bb.y/e;
	c3 += 1. - bb.x*bb.x/e;
	c4 -= bb.x*bb.y/e;
	c5 += 1. - bb.y*bb.y/e;
	d0 += aa.z - g*bb.z/e;
	d1 += aa.x - g*bb.x/e;
	d2 += aa.y - g*bb.y/e;
      };
    e = c0*c3*c5 + 2.*c1*c2*c4 - c0*c4*c4 - c3*c2*c2 - c5*c1*c1;

    mVertex.z = (d0*(c3*c5-c4*c4)- d1*(c5*c1 - c2*c4) + d2*(c1*c4 - c3*c2))/e;
    mVertex.x = (- d0*(c1*c5 - c2*c4) + d1*(c0*c5 - c2*c2) - d2*(c0*c4 - c1*c2))/e;
    mVertex.y = (d0*(c1*c4 - c2*c3) - d1*(c0*c4 - c1*c2) + d2*(c0*c3 - c1*c1))/e;

    //printf("processing event %d, vertex %f, %f, %f\n",this, mVertex.x , mVertex.y, mVertex.z);

    for(i = 0; i < mNumTracks; i++)
      {	      
	//if( ! mTracks[i]->flag ) continue;//***************************************************
	aa.x = mTracks[i]->a.x - mVertex.x;
	aa.y = mTracks[i]->a.y - mVertex.y;
	aa.z = mTracks[i]->a.z - mVertex.z;
	bb = mTracks[i]->b;
	mTracks[i]->chi2_2 = aa.x*aa.x + aa.y*aa.y + aa.z*aa.z - 
	  (aa.x*bb.x + aa.y*bb.y + aa.z*bb.z)*(aa.x*bb.x + aa.y*bb.y + aa.z*bb.z)/
	  (bb.x*bb.x + bb.y*bb.y + bb.z*bb.z);

	//mTracks[i]->flag = (mTracks[i]->chi2_2 < 0.02*0.02);
      };

    int nSelectedTracks = 0;
    for(i = 0; i < mNumTracks; i++)
      {
       	if (mTracks[i]->numberOfHits == 3)
	  c = (mTracks[i]->chi2_1 + mTracks[i]->chi2_2)/4;
	else 
	  c = (mTracks[i]->chi2_1 + mTracks[i]->chi2_2)/5.;
	
        mTracks[i]->flag = (c<0.03);//0.0002

	if (mTracks[i]->flag)
	  {
	    nSelectedTracks++;
	    mTotalChi2 += c;
	  };
      };
    mTotalChi2/=(double)nSelectedTracks;
    //printf("   processTracks : nSelectedTracks=%d\n", nSelectedTracks );

    return mTotalChi2;
};


//--------------------------------------------------------------------------------------
// 'cosmic alignment' : no primary vertex constraint


int StSsdEvent::setCosmicChi2(int ichi2, int track, int hitnumber, globalPoint &p)
{
  double chi2= chi2ab_Np(mTracks[track]->a, mTracks[track]->b,
			 &(mTracks[track]->p[0]), mTracks[track]->numberOfHits, p, hitnumber);

  if (ichi2==1) mTracks[track]->chi2_1 = chi2;
  else  mTracks[track]->chi2_2 = chi2;
  return 1;
};


double StSsdEvent::processCosmics()
{
  double cosmicsChi2 = 0.;
  for(int i = 0; i < mNumTracks; i++)
    {
      mTracks[i]->flag = (1==1); 
      mTracks[i]->chi2_1 = chi2ab_Np( mTracks[i]->a, mTracks[i]->b, &(mTracks[i]->p[0]),
				      mTracks[i]->numberOfHits, mTracks[i]->p[0], 0);
      cosmicsChi2 += mTracks[i]->chi2_1;
    };
  return (cosmicsChi2/mNumTracks);
};
