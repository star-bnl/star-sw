/*****************************************************************
 * $Id: StMuPmdHit.h,v 1.1 2004/10/19 01:40:21 mvl Exp $
 *
 * Class : StMuPmdHit
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Hit class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdHit.h,v $
 * Revision 1.1  2004/10/19 01:40:21  mvl
 * New class to hold Pmd hits (raw adc information)
 *
 * ****************************************************************/

#ifndef StMuPmdHit_h
#define StMuPmdHit_h

#include "TObject.h"

class StMuPmdHit : public TObject
{
 public:
	 	StMuPmdHit();
		StMuPmdHit(StMuPmdHit*);
  virtual	~StMuPmdHit();
  
  Int_t		superModule()	{return (Int_t)mSuperModule;}
  Int_t		subDetector()	{return (Int_t)mSubDetector;}
  Int_t		row()		{return (Int_t)mRow;}
  Int_t 	column()	{return (Int_t)mCol;}
  Float_t	energy()	{return mEnergy;}
  Int_t		adc()		{return mADC;}

  void		setSuperModule(Int_t var)	{mSuperModule = (Short_t)var;}
  void		setSubDetector(Int_t var)	{mSubDetector = (Short_t)var;}
  void		setRow(Int_t var)		{mRow = (Short_t)var;}
  void		setColumn(Int_t var)		{mCol = (Short_t)var;}
  void		setEnergy(Float_t var)		{mEnergy = var;}
  void		setADC(Int_t var)		{mADC = var;}

 protected:
  Short_t	mSuperModule;
  Short_t	mSubDetector;
  Short_t	mRow;
  Short_t	mCol;
  Float_t	mEnergy;
  Int_t		mADC;

 ClassDef(StMuPmdHit,1)
};
#endif
  
			
