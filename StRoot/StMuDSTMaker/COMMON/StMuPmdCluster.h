/*****************************************************************
 * $Id: StMuPmdCluster.h,v 1.2 2004/05/02 04:10:14 perev Exp $
 *
 * Class : StMuPmdCluster
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Cluster class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdCluster.h,v $
 * Revision 1.2  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.1  2004/04/02 03:36:21  jeromel
 * New files for PMD
 *
 * ****************************************************************/

#ifndef StMuPmdCluster_h
#define StMuPmdCluster_h

#include "TObject.h"
#include "TArrayS.h"

class StMuPmdCluster : public TObject
{
 public:
	 	StMuPmdCluster();
		StMuPmdCluster(StMuPmdCluster*);
  virtual	~StMuPmdCluster();
  
  Int_t		GetSuperModule()	{return mSuperModule;}
  Int_t		GetNcell()		{return mNcell;}
  Float_t	GetEta()		{return mEta;}
  Float_t 	GetPhi()		{return mPhi;}
  Float_t	GetSigma()		{return mSigma;}
  Float_t	GetEnergy()		{return mEnergy;}
  Int_t		GetEnergyPID()		{return mEnergyPID;}//PID from Energy cut
  Int_t		GetPID()		{return mPID;}//PID from other method (e.g. NN)
  Int_t		GetMcPID()		{return mMcPID;}//PID from GEANT, 0 for Data

  void		SetSuperModule(Int_t var)	{mSuperModule = var;}
  void		SetNcell(Int_t var)		{mNcell = var;}
  void		SetEta(Float_t var)		{mEta = var;}
  void		SetPhi(Float_t var)		{mPhi = var;}
  void		SetSigma(Float_t var)		{mSigma = var;}
  void		SetEnergy(Float_t var)		{mEnergy = var;}
  void		SetEnergyPID(Int_t var)		{mEnergyPID = var;}
  void		SetPID(Int_t var)		{mPID = var;}
  void		SetMcPID(Int_t var)		{mMcPID = var;}

 protected:
  Int_t		mSuperModule;
  Int_t		mNcell;
  Float_t	mEta;
  Float_t	mPhi;
  Float_t	mSigma;
  Float_t	mEnergy;
  Int_t		mEnergyPID;
  Int_t		mPID;
  Int_t		mMcPID;

 ClassDef(StMuPmdCluster,1)
};
#endif
  
			
