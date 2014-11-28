/***************************************************************************
 *
 * $Id: StHbtMaker.h,v 1.9 2014/08/06 11:43:19 jeromel Exp $
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
 * Revision 1.9  2014/08/06 11:43:19  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.8  2012/01/21 16:50:55  yyang
 * this is a test commit
 *
 * Revision 1.7  2003/09/10 19:47:19  perev
 * ansi corrs
 *
 * Revision 1.6  2003/09/07 03:49:02  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.5  2001/09/05 20:40:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.4  2000/01/25 17:33:38  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/07/26 16:21:25  lisa
 * always convert string to char when output - needed on solaris
 *
 * Revision 1.2  1999/07/15 13:57:11  perev
 * cleanup
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtMaker_HH
#define StHbtMaker_HH

#ifdef __ROOT__
#ifndef StMaker_H
#include "StMaker.h"
#endif
#else
typedef int Int_t;
#endif

#include "StHbtMaker/Infrastructure/StHbtManager.h"
//class StHbtManager;


class StHbtMaker
#ifdef __ROOT__ 
: public StMaker 
#endif
{
 
 private:
  StHbtManager* mHbtManager;//! tells cint to skip it

  int mDebug;
 public:
  StHbtMaker(const char* name = "StHbt", const char* title = "StHbtTit");
  virtual ~StHbtMaker();
  virtual void  Clear(const char* opt="");
  virtual Int_t Init();//!
  virtual Int_t Make();
  virtual Int_t Finish();//!

  int Debug() const;
  void SetDebug(int);

#ifdef __ROOT__
  StMaker* currentChain;
#endif
  //  StHbtManager* HbtManager();//! tells cint to skip that
  StHbtManager* HbtManager();

  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StHbtMaker.h,v 1.9 2014/08/06 11:43:19 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
#ifdef __ROOT__
  ClassDef(StHbtMaker,0)
#endif
};

inline StHbtManager* StHbtMaker::HbtManager(){return mHbtManager;}
inline  int StHbtMaker::Debug() const {return mDebug;}
inline  void StHbtMaker::SetDebug(int d){mDebug=d;}

//this is test comment for testing cvs commit

#endif
