#ifndef __AgVolume_h__
#define __AgVolume_h__

#include "TNamed.h"
#include "TString.h"
#include <vector>
#include <map>

class AgVolume;
class AgVolume : public TNamed
{
 public:
  AgVolume(const Char_t *name, const Char_t *title);
  ~AgVolume(){ /* nada */ };

  static AgVolume *Make();

 private:
 protected:

  static std::map<TString, AgVolume *> mVolumeTable;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }


};

#endif
