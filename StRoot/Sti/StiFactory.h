#ifndef StiFactory_H
#define StiFactory_H

#include <string>
using std::string;

class StiFactory
{
 public:

    //StiFactory(); //Not implemented
    StiFactory(const string& name);
    virtual ~StiFactory();
    
    const string& getName() const;
    
protected:
    
    const string& name;
};

inline const string& StiFactory::getName() const
{
  return name;
}
 

#endif
