#include "Sti/Base/Described.h"

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

const string Described::getDescription() const
{
    return string(_description);
}

bool Described::isDescribed() const
{
   return (_description.size()>0 && _description!=" ");
}

bool Described:: isDescription(const string & description) const
{
   return _description==description;
}

bool Described::sameDescriptionAs(const Described & described) const
{
  return _description==described._description;
}

