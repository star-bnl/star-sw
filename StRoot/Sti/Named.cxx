// Named.cpp: implementation of the Named class.
//
//////////////////////////////////////////////////////////////////////

#include "Named.h"

Named::Named(const string & aName)
{
   _name = aName;
}

Named::~Named()
{}

void Named::setName(const string & newName)
{
    _name = newName;
}

const string Named::getName()
{
    return string(_name);
}

bool Named::isNamed()
{
   return (_name.size()>0 && _name!=" ");
}

bool Named:: isName(const string & aName)
{
   return _name==aName;
}

bool Named::isNamedAs(const Named & named)
{
  return _name==named._name;
}

