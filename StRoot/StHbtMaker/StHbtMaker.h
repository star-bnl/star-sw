/***************************************************************************
 *
 * $Id: StHbtMaker.h,v 1.2 1999/07/15 13:57:11 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Maker class is the interface with root4star/Maker framework
 *
 ***************************************************************************
 *
 * $Log: StHbtMaker.h,v $
 * Revision 1.2  1999/07/15 13:57:11  perev
 * cleanup
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtMaker_HH
#define StHbtMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Infrastructure/StHbtManager.h"
//class StHbtManager;


class StHbtMaker : public StMaker {
 
 private:
  StHbtManager* mHbtManager;//! tells cint to skip it

 public:


  StHbtMaker(const char* name = "StHbt", const char* title = "StHbtTit");
  virtual ~StHbtMaker();
  virtual void  Clear(const char* opt="");
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  StMaker* currentChain;
  //  StHbtManager* HbtManager();//! tells cint to skip that
  StHbtManager* HbtManager();

  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHbtMaker.h,v 1.2 1999/07/15 13:57:11 perev Exp $ built "__DATE__" "__TIME__ ; return cvs;}

  ClassDef(StHbtMaker, 1)

};

inline StHbtManager* StHbtMaker::HbtManager(){return mHbtManager;}



#endif
