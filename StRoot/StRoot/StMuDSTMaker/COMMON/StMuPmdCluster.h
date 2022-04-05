/*****************************************************************
 * $Id: StMuPmdCluster.h,v 1.3 2004/10/19 01:44:29 mvl Exp $
 *
 * Class : StMuPmdCluster
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Cluster class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdCluster.h,v $
 * Revision 1.3  2004/10/19 01:44:29  mvl
 * Changed names of getters and setters (Star coding conventions)
 *
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
  
  Int_t		superModule()	{return mSuperModule;}
  Int_t		ncell()		{return mNcell;}
  Float_t	eta()		{return mEta;}
  Float_t 	phi()		{return mPhi;}
  Float_t	sigma()		{return mSigma;}
  Float_t	energy()	{return mEnergy;}
  Int_t		energyPID()	{return mEnergyPID;}//PID from Energy cut
  Int_t		PID()		{return mPID;}//PID from other method (e.g. NN)
  Int_t		mcPID()		{return mMcPID;}//PID from GEANT, 0 for Data

  void		setSuperModule(Int_t var)	{mSuperModule = var;}
  void		setNcell(Int_t var)		{mNcell = var;}
  void		setEta(Float_t var)		{mEta = var;}
  void		setPhi(Float_t var)		{mPhi = var;}
  void		setSigma(Float_t var)		{mSigma = var;}
  void		setEnergy(Float_t var)		{mEnergy = var;}
  void		setEnergyPID(Int_t var)		{mEnergyPID = var;}
  void		setPID(Int_t var)		{mPID = var;}
  void		setMcPID(Int_t var)		{mMcPID = var;}

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
  
			
