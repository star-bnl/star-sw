#if !defined(AFX_NAMED_H__2FA55545_A08B_44B7_A871_200FE41E6994__INCLUDED_)
#define AFX_NAMED_H__2FA55545_A08B_44B7_A871_200FE41E6994__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include <string>
using std::string;

/*! 
\class Named
This class encapsulates the notion of "name". It should be used as
base class to provide a "named" property to objects.#include <string>
use STD;

\author Claude A Pruneau
*/

class Named  
{
public:
  virtual ~Named();
  
  /// Set the name of the object
  void setName(const string & newName);
  
  /// Get the name of the object
  const string getName() const;
  
  /// Determine whether name is set, i.e object has a name
  bool isNamed() const;
  
  /// Determine whether name equals given name
  bool isName(const string & aName) const;
  
  /// Determine whether name equals that of given object
  bool isNamedAs(const Named & named) const;
  
 protected:
  
  /// Only derived class are Named
  Named(const string & aName=" ");
  
  string _name;
};



#endif // !defined(AFX_NAMED_H__2FA55545_A08B_44B7_A871_200FE41E6994__INCLUDED_)
