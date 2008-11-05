// -*- mode: c++;-*-
// $Id: StjSpinReader.h,v 1.1 2008/11/05 05:48:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJSPINREADER_H
#define STJSPINREADER_H

#include "StjTreeReader.h"

class StjSpinReader : public StjTreeReader {

public:
  StjSpinReader(TTree *tree) : StjTreeReader(tree) { }
  virtual ~StjSpinReader() { }

  int runNumber()  const { return __runNumber;  }
  int eventId()    const { return __eventId;    }
  int bx7()    const { return __bx7;    }
  int bx48()    const { return __bx48;    }
  int spin4()    const { return __spin4;    }
  int bbcTimebin()    const { return __bbcTimebin;    }
  double vertexZ()    const { return __vertexZ;    }

private:

  void SetBranchAddress(TTree *tree);

  void clearEntry();
  void readEntry();

  Int_t _runNumber;
  Int_t _eventId;
  Int_t _bx7;
  Int_t _bx48;
  Int_t _spin4;
  Int_t _bbcTimebin;
  Double_t _vertexZ;

  int __runNumber;
  int __eventId;
  int __bx7;
  int __bx48;
  int __spin4;
  int __bbcTimebin;
  double __vertexZ;

  ClassDef(StjSpinReader, 1)

};

#endif // STJSPINREADER_H
