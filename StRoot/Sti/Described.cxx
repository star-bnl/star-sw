#include "Described.h"

Described::Described(const string & description)
{
   _description = description;
}

Described::~Described()
{}

void Described::setDescription(const string & description)
{
    _description = description;
}

const string Described::getDescription()
{
    return string(_description);
}

bool Described::isDescribed()
{
   return (_description.size()>0 && _description!=" ");
}

bool Described:: isDescription(const string & description)
{
   return _description==description;
}

bool Described::sameDescriptionAs(const Described & described)
{
  return _description==described._description;
}

