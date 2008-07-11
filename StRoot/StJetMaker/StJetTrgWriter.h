// -*- mode: c++;-*-
// $Id: StJetTrgWriter.h,v 1.1 2008/07/11 23:32:19 tai Exp $
#ifndef STJETTRGWRITER_H
#define STJETTRGWRITER_H

class StJetTrgWriter {

public:

  StJetTrgWriter() { }
  virtual ~StJetTrgWriter() { }

  virtual void Init() { }
  virtual void Make() { }
  virtual void Finish() { }

};

#endif // STJETTRGWRITER_H
