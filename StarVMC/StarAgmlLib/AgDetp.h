#ifndef __AgDetp_h__
#define __AgDetp_h__

#include "TNamed.h"

class AgDetp : public TNamed
{
 public:
  AgDetp( const Char_t *name="", const Char_t *title="" ){ };
  ~AgDetp(){ };

  void operator()( const Char_t *tag ){ };

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }


};

#endif
