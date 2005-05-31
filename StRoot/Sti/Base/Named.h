#ifndef NAMED_H
#define NAMED_H
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
  const string &getName() const;
  
  
  /// Determine whether name equals given name
  bool isName(const string & aName) const;
  
protected:
  /// Only derived class are Named
  Named(const string & aName=" ");

protected:

  string _name;
};



#endif // 
