/***************************************************************************
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 **************************************************************************/

#ifndef StHbtTagWriter_hh
#define StHbtTagWriter_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "HbtTag.h"

class StHbtTagWriter{
public:
  static StHbtTagWriter* Instance();
  void   Clear();
  void SetTag(const char*, unsigned char, float);
  float Tag(const char*, unsigned char);

  
  friend class StHbtTagMaker;
protected: 
  StHbtTagWriter();
  HbtTag_st mHbtTag; 
private:
  static StHbtTagWriter* _instance;

  
  
#ifdef __ROOT__
  ClassDef(StHbtTagWriter,0)
#endif

};


#endif
