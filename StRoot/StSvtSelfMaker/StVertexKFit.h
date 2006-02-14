/*!
 * \class StVertexKFit 
 * \author Victor Perev, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StVertexKFit.h,v 1.1 2006/02/14 19:02:09 perev Exp $
 *
 * Author: Victor Perev, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertexKFit.h,v $
 * Revision 1.1  2006/02/14 19:02:09  perev
 * Svt self alignment maker
 *
 *
 **************************************************************************/
#ifndef StVertexKFit_hh
#define StVertexKFit_hh
#include "TObject.h"


class StVertexKFit : public TObject {
public:
    StVertexKFit();
   ~StVertexKFit(){}


void SetVtx(const double *vtx,const double *etx);
void SetVtx(const float  *vtx,const float  *etx);
void SetTrk(const double *xyz,const double *dir,double curv);
void SetTrk(const float  *xyz,const float  *dir,float  curv);

double Update();
void Print(const char *opt = "") const;

const double *GetVtx() const 		{return mVtx;}
const double *GetEtx() const 		{return mEtx;}
      double  GetChi2(int i=0) const	{return mChi2[i];}
         int  GetNTk() const		{return mNTk;}
private:
char   mBeg[1];
double mChi2[2];
	/// Vertex x,y,z 
double mVtx[3];
  	/// Vertex error matrix. In form:
  	/// xx
  	/// yx yy
  	/// zx zy zz
double mEtx[6];  

	/// track point x,y,x
double mXTk[3];
	/// track direction x,y,x. Normalized to 1
double mDTk[3];
	/// track curvature
double mCurv;

int    mNTk;
char   mEnd[1];

  ClassDef(StVertexKFit,0);
};
  
#endif
