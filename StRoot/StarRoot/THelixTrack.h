#ifndef THELIXTRACK_H
#define THELIXTRACK_H
#include "TObject.h"

class THelixTrack : public TObject 
{
public:

	THelixTrack();
	THelixTrack(const double *xyz,const double *dir,double rho,const double *hxyz=0,double drho=0);
	THelixTrack(const double *pnts,int npnts, int size = 3);
	THelixTrack(const THelixTrack &from);

	void Set   (const double *xyz,const double *dir,double rho,const double *hxyz=0,double drho=0);
	void Set   (double rho,double drho=0);
	void Backward();
	double Fit(const double *pnts,int npnts, int size = 3);
	double Move(double step);
	double Steb(double step, double *xyz, double *dir=0) const;
	double Step(double step, double *xyz, double *dir,double &rho) const;
	double Step(double step, double *xyz, double *dir=0) const;
        double Step(double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(const double *point,double *xyz=0, double *dir=0) const;
        double Step(const double *point,double *xyz=0, double *dir=0) const;
        double GetDCA  () const;
        double GetDCAz () const;
        double GetDCAxy() const;
        const double *GetXYZ() const {return fX;}
        const double *GetDir() const {return fP;}
        double GetRho() const {return fRho ;}
        double GetDRho()const {return fDRho ;}
        double GetCos() const {return fCosL;}
        double GetSin() const {return fHP  ;}
        double GetPeriod() const ;

        void Print(Option_t *opt="") const;
private:
static  int  SqEqu(double *, double *);
protected:
        double Step(double stmin,double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double Steb(double stmin,double stmax, const double *surf, int nsurf, double *x=0, double *dir=0) const;
        double StepHZ(const double *surf, int nsurf, double *x=0, double *dir=0) const;
	void Build();

	double fX[3];
	double fP[3];
	double fPxy[3];
	double fH[3];
	double fHXP[3];
	double fRho;
	double fDRho;
	double fHP;
	double fCosL;
   	double fMax;
   	mutable double fDCA[2];
        mutable Int_t  fKind;
ClassDef(THelixTrack,0)
};
#endif // THELIXTRACK_H
