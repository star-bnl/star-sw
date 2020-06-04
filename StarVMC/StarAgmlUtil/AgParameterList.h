#ifndef __AgParameterList_h__
#define __AgParameterList_h__
#include <string>
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
    : mParameters(), mParameterList(), 
      _Block(), _Mother(), _Group(), _Table(), _Chair(), _Row(0)
  {/* nada */ };

  virtual ~AgParameterList(){ /* nada */ };

  /// Returns a reference to the named parameter.
  virtual  T&    par   ( const char* name ){ std::string key=name; return mParameters[name]; }
  //  virtual  T&    par   ( std::string name ){ return par(name.c_str()); }
  /// Returns true if the parameter is set
  virtual  bool  isSet ( const char* name ) const { std::string key=name; return (mParameters.find(key) != mParameters.end() ); }
  //  virtual  bool  isSet ( std::string name ) const { return isSet(name.c_str()); }

  /// Returns true if the parameter appears in the list of valid parameters.
  /// If the list has not been defined with register, will always return true.
  virtual  bool  hasPar( const char* name ) const { std::string key=name; return (std::find(mParameterList.begin(),mParameterList.end(),key) != mParameterList.end() && mParameterList.size()>0); }
  //  virtual  bool  hasPar( std::string name ) const { return hasPar(name.c_str()); }

  /// Unset specified parameter
  virtual  void  unSet( const char *name ) { std::string key; typename std::map<std::string,T>::iterator pos = mParameters.find(key); if (pos!=mParameters.end()) mParameters.erase(pos); };
  //  virtual  void  unSet( std::string name ) { unSet( name.c_str() ); }

  enum { kOnly=0, kMany };

  void SetBlock ( const char* name ){ _Block=name;  }
  void SetMother( const char* name ){ _Mother=name; }
  void SetGroup ( const char* name ){ _Group=name;  }
  void SetTable ( const char* name, int row=0 ){ _Table=name; _Row=row; }
  void SetChair ( const char* name ){ _Chair=name; }

  const char* block() { return _Block.c_str();  }
  const char* mother(){ return _Mother.c_str(); }
  const char* group() { return _Group.c_str();  }
  const char* table() { return _Table.c_str();  }
  const char* chair() { return _Chair.c_str();  }
  const int   row()   { return _Row; }

private:
protected:

  std::map< std::string, T > mParameters;
  std::vector< std::string > mParameterList;

  std::string _Block;
  std::string _Mother;
  std::string _Group;
  std::string _Table;
  std::string _Chair;
  int         _Row;

public:
  /// Copy ctor
  AgParameterList( const AgParameterList& other ) 
    : mParameters(other.mParameters), mParameterList(other.mParameterList),
      _Block(other._Block)   , 
      _Mother(other._Mother) ,
      _Group(other._Group)   , 
      _Table(other._Table)   ,
      _Chair(other._Chair)   ,
      _Row(other._Row)
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
    _Chair = other._Chair;
    _Row   = other._Row;
    return *this;
  }

};



#endif

