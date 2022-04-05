// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef UNIQUESTRINGGENERATOR_HH
#define UNIQUESTRINGGENERATOR_HH

#include "Misc.hh"

#include <string>

class UniqueStringGenerator {

public:
  UniqueStringGenerator() { }
  virtual ~UniqueStringGenerator() { }

  static std::string generate()
  {
    return std::string("a") + to_string(_id++);
  }

private:

  static int _id;

};

#endif // UNIQUESTRINGGENERATOR_HH
