#ifndef __StarTGeoStacker_h__
#define __StarTGeoStacker_h__

#include "StarAgmlStacker.h"
class AgPlacement;
class AgBlock;

class TGeoMaterial;
class TGeoMedium;
class TGeoShape;
class TGeoVolume;
class TGeoNode;

#include <vector>
#include <map>

#include "TString.h"

class StarTGeoStacker : public StarAgmlStacker
{
 public:
  StarTGeoStacker(const Char_t *name="dyson", const Char_t *title="Construction of STAR geometry");
  ~StarTGeoStacker(){ /* nada */ };
  
  Bool_t Build( AgBlock *block );
  Bool_t Position ( AgBlock *block, AgPlacement position );

  Bool_t SearchVolume( const AgShape &shape, const AgAttribute &attr );

  static std::map< Int_t, TString > mClassMap; // maps shape names to TGeo classes

  void AddGroup( const Char_t *group );

 private:
 protected:

  TGeoMaterial *GetMaterial( const Char_t *name, const Char_t *block );
  friend class _StarTGeoStackerDummy_;

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built "__DATE__" "__TIME__;
    return cvs;
  }

  ClassDef(StarTGeoStacker,1);  

};

#endif
 
