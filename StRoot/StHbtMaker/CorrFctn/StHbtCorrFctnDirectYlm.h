/****************************************************************************
 * $Id: StHbtCorrFctnDirectYlm.h,v 1.1 2013/01/18 14:46:02 yyang Exp $
 *
 * Author: Yan Yang, hce137@gmail.com
 * **************************************************************************
 * Description: Correlation function that is binned in Ylms directly
 *				Provides a way to store the numerator and denominator
 *				in Ylms directly and correctly calculate the correlation
 *				function from them.
 *				The original author is not known, I guess Kisiel would be.
 ***************************************************************************/

#ifndef StHbtCorrFctnDirectYlm_hh
#define StHbtCorrFctnDirectYlm_hh


#include <math.h>
#include <complex>
#include <TH1D.h>
#include <TH3D.h>
#include <TFile.h>
#include "StHbtCorrFctn.hh"
#include "StHbtYlm.h"

using namespace std;
class TRootIOCtor;

class StHbtCorrFctnDirectYlm : public StHbtCorrFctn
{
public:
	StHbtCorrFctnDirectYlm();
	StHbtCorrFctnDirectYlm(const char* name, int maxl, int ibin, double vmin, double vmax);
	~StHbtCorrFctnDirectYlm();

	virtual StHbtString Report();

	virtual void AddRealPair(const StHbtPair* aPair) { AddRealPair(aPair,  1); }
	virtual void AddMixedPair(const StHbtPair* aPair) { AddMixedPair(aPair, 1); }
	void AddRealPair(const StHbtPair* aPair, double weight);
	void AddMixedPair(const StHbtPair* aPair, double weight);

	void SetR2Factor(bool aFactor);
	void DoEMCIC();

	virtual void Finish();
	virtual void Write() {}
	void Write(TFile* rfile);

	void SetPairCut(StHbtPairCut* pc) { mPairCut = pc; }
	void ReadFromFile(TFile* ifile);
	void CalcCorrFctn();

	TH1D* GetNumRealHist(int el, int em);
	TH1D* GetNumImagHist(int el, int em);

	TH1D* GetDenRealHist(int el, int em);
	TH1D* GetDenImagHist(int el, int em);

//TH1D *GetCfnRealHist(int el, int em);
//TH1D *GetCfnImagHist(int el, int em);

//void SetAdvancedNormalization(double radius, double bohr, double purity, int binmin, int binmax);

private:
	double ClebschGordan(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm);
	double WignerSymbol(double aJot1, double aEm1, double aJot2, double aEm2, double aJot, double aEm);
	double DeltaJ(double aJot1, double aJot2, double aJot);

	void GetMtilde(complex<double>* aMat, double* aMTilde);

	int  GetMaxJM();
	void GetElEmForIndex(int aIndex, double* aEl, double* aEm);
	void GetElEmForIndex(int aIndex, int* aEl, int* aEm);
	int  GetBin(int qbin, int ilmzero, int zeroimag, int ilmprim, int primimag);

	int  PackYlmVector(double* invec, double* outvec);
	int  PackYlmVectorIndependentOnly(double* invec, double* outvec);

	int  PackYlmMatrix(double* inmat, double* outmat);
//void UnPackYlmMatrix(double *inmat, double *outmat);

	int  PackYlmMatrixIndependentOnly(double* inmat, double* outmat);
	void UnPackYlmMatrixIndependentOnly(double* inmat, double* outmat, int insize);

	void InvertYlmIndependentMatrix(double* inmat, double* outmat);

	int GetIndexForLM(int el, int em);

	void PackCovariances();
	void UnpackCovariances();
//void PackCfcCovariance();

	TH1D** numsreal;          //! Real parts of Ylm components of the numerator
	TH1D** numsimag;          //! Imaginary parts of Ylm components of the numerator
	TH1D** densreal;          //! Real parts of Ylm components of the denominator
	TH1D** densimag;          //! Imaginary parts of Ylm components of the denominator

	TH1D** cftnreal;          //Real parts of Ylm components of the correlation function
	TH1D** cftnimag;          //Imaginary parts of Ylm components of the correlation function

	TH1D* binctn;             //! Bin occupation for the numerator
	TH1D* binctd;             //! Bin occupation for the denominator

	TH3D* covnum;             //! Numerator covariance matrix packed into TH3D
	TH3D* covden;             //! Denominator covariance matrix packed into TH3D
//TH3D *covcfc;               // Correlation function covariance matrix packed into TH3D

	double* covmnum;          //! Covariance matrix for the numerator
	double* covmden;          //! Covariance matrix for the denominator
	double* covmcfc;          //! Covariance matrix for the correlation function

	int fMaxL;                //l cut-off of the decomposition

	int		maxjm;            //number of l-m combinations
	int*	elsi;             //! table of integer l's
	int*	emsi;             //! table of integer m's
	double* els;              //! table of l's
	double* ems;              //! table of m's

	complex<double>* ylmbuffer; //! buffer for ylm calculation
	double*			 factorials; //! Helper table of factorials

	int fR2factor;            //If 1 all entries are scaled by 1/r^2

	bool   mDoEMCIC;
	TH1D** emcicP1P2T;    //! emcic
	TH1D** emcicP1P2Z;    //! emcic
	TH1D** emcicE1E2;     //! emcic
	TH1D** emcicE1plusE2; //! emcic
	string mName;
	int    mNbins;
	double mkmin;
	double mkmax;

	double mNormRadius;     //Asymptotic radius for normalization
	double mNormBohr;       //Bohr radius for advances normalization
	double mNormPurity;     //Purity scaling the asymptotic hehavior
	int	   mNormBinMin;     //Minimum bin for normalization
	int	   mNormBinMax;     //Maximum bin for normalization

#ifdef __ROOT__
	ClassDef(StHbtCorrFctnDirectYlm, 1)
#endif

};

#endif

/***************************************************************************
 * $Log: StHbtCorrFctnDirectYlm.h,v $
 * Revision 1.1  2013/01/18 14:46:02  yyang
 * Add ultilities for SHD of CF
 *
 * ***********************************************************************/
