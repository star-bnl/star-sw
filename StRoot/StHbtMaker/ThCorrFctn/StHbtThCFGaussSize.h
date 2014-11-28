/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : This class is used by StHbtThCFGaussFit to store information 
 * about the size of the source. 
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtThCFGaussSize_h
#define StHbtThCFGaussSize_h


#include <Stiostream.h>

#include "StHbtMaker/ThCorrFctn/StHbtThCorrFctnCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
class StHbtThCorrFctn;
class StHbtThPair;


class StHbtThCFGaussSize {

 public:
  StHbtThCFGaussSize();
  StHbtThCFGaussSize( const char* aName, double aX, double aY,double aZ, double aT);
  ~StHbtThCFGaussSize();

  StHbtThCFGaussSize* Copy( const char* aName, double aX, double aY,double aZ, double aT);

  void AddCorrFctn(const StHbtThCorrFctn* );
  void FillPair(StHbtThPair* );

  void Finish();
  StHbtString Report();

  double GetSizeX() const;
  double GetSizeY() const;
  double GetSizeZ() const;
  double GetTime() const;

 protected:
  double mSizeX;
  double mSizeY;
  double mSizeZ;
  double mTime;
  char* mName;
  StHbtThCorrFctnCollection mThCorrFctnColl;
};

#endif
