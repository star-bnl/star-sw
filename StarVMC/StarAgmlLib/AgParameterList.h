#ifndef __AgParameterList_h__
#define __AgParameterList_h__
#include "TString.h"
#include <map>

//
// Templated parameter list.  Meant to be a replacement for functionality duplicated in
// AgPlacement, AgTransform, probably elsewhere as well.
//

template<typename T>
class AgParameterList 
{
public:

  T&    par  ( const char* name );
  bool  isSet( const char* name ) const;

  enum { kOnly=0, kMany };

  void SetBlock ( const char* name ){ mBlock=name;  }
  void SetMother( const char* name ){ mMother=name; }
  void SetGroup ( const char* name ){ mGroup=name;  }
  void SetTable ( const char* name ){ mTable=name;  }

  const char* block(){ return mBlock.Data(); }
  const char* mother(){ return mMother.Data(); }
  const char* group(){ return mGroup.Data(); }
  const char* table(){ return mTable.Data(); }

private:

  std::map< TString, T > mParameters;

  TString mBlock;
  TString mMother;
  TString mGroup;
  TString mTable;

  //  ClassDef(AgParameterList, 1);

};

#endif

