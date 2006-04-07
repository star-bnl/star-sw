/*!
 * \class StVertexKFit 
 * \author Victor Perev, Jan 2006
 */
/***************************************************************************
 *
 * $Id: StVertexKFit.h,v 1.2 2006/04/07 17:33:30 perev Exp $
 *
 * Author: Victor Perev, Jan 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVertexKFit.h,v $
 * Revision 1.2  2006/04/07 17:33:30  perev
 * Too big Chi2 of VTX fit fixed
 *
 * Revision 1.1  2006/02/14 19:02:09  perev
 * Svt self alignment maker
 *
 *
 **************************************************************************/
#ifndef StVertexKFit_hh
#define StVertexKFit_hh
#include "TObject.h"
#include "TArrayD.h"


class StVertexKFitAux;
class StVertexKFit : public TObject {
public:
    StVertexKFit();
   ~StVertexKFit(){}


void SetVtx(const double *vtx,const double *etx);
void SetVtx(const float  *vtx,const float  *etx);
void AddTrk(const double *xyz,const double *dir,double curv,const double *erk=0);
void AddTrk(const float  *xyz,const float  *dir,float  curv,const float  *erk=0);

double Fit();
void Print(const char *opt = "") const;

const double *GetVtx() const 		{return mVtx;}
const double *GetEtx() const 		{return mEtx;}
      double  GetChi2(int i=0) const	{return mChi2[i];}
         int  GetNFit() const		{return mNTk;}
private:
TArrayD mArr; 
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

StVertexKFitAux *mAux;
int    mNTk;
int    mNAux;
char   mEnd[1];

  ClassDef(StVertexKFit,0);
};
  
#endif
