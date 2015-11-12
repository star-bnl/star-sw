#ifndef __AgParameterList_h__
#define __AgParameterList_h__
#include "TString.h"
#include <map>
#include <vector>
#include <algorithm>

//
// Templated parameter list.  Meant to be a replacement for functionality duplicated in
// AgPlacement, AgTransform, probably elsewhere as well.
//

template<typename T>
class AgParameterList 
{
public:

  AgParameterList() 
    : mParameters(), mParameterList(), _Block(), _Mother(), _Group(), _Table()
  {/* nada */ };

  virtual ~AgParameterList(){ /* nada */ };

  /// Returns a reference to the named parameter.
  virtual  T&    par   ( const char* name ){ TString key=name; return mParameters[name]; }
  /// Returns true if the parameter is set
  virtual  bool  isSet ( const char* name ) const { TString key=name; return (mParameters.find(key) != mParameters.end() ); }

  /// Returns true if the parameter appears in the list of valid parameters.
  /// If the list has not been defined with register, will always return true.
  virtual  bool  hasPar( const char* name ) const { TString key=name; return (std::find(mParameterList.begin(),mParameterList.end(),key) != mParameterList.end() && mParameterList.size()>0); }

  /// Unset specified parameter
  virtual  void  unSet( const char *name ) { TString key; typename std::map<TString,T>::iterator pos = mParameters.find(key); if (pos!=mParameters.end()) mParameters.erase(pos); };

  enum { kOnly=0, kMany };

  void SetBlock ( const char* name ){ _Block=name;  }
  void SetMother( const char* name ){ _Mother=name; }
  void SetGroup ( const char* name ){ _Group=name;  }
  void SetTable ( const char* name ){ _Table=name;  }

  const char* block() { return _Block.Data();  }
  const char* mother(){ return _Mother.Data(); }
  const char* group() { return _Group.Data();  }
  const char* table() { return _Table.Data();  }

private:
protected:

  std::map< TString, T > mParameters;
  std::vector< TString > mParameterList;

  TString _Block;
  TString _Mother;
  TString _Group;
  TString _Table;

public:
  /// Copy ctor
  AgParameterList( const AgParameterList& other ) 
    : mParameters(other.mParameters), mParameterList(other.mParameterList),
      _Block(other._Block), 
      _Mother(other._Mother),
      _Group(other._Group), 
      _Table(other._Table)
  { /* nada */ }

  /// Assignment operators
  AgParameterList &operator=(const AgParameterList& other) 
  {
    mParameters = other.mParameters;
    mParameterList = other.mParameterList;
    _Block = other._Block;
    _Mother = other._Mother;
    _Group = other._Group;
    _Table = other._Table;
    return *this;
  }


  //  ClassDef(AgParameterList, 1);

};



#endif

