#ifndef __StarAgmlStacker_h__
#define __StarAgmlStacker_h__

#include "TNamed.h"

#include "AgShape.h"
#include "AgMaterial.h"
#include "AgMedium.h"
#include "AgAttribute.h"
#include "AgPlacement.h"
#include "AgPosition.h"

#include "TString.h"
#include "TGeoVolume.h"
#include "TGeoMedium.h"
#include "TGeoMaterial.h"

#include <vector>
#include <map>
#include <string>

class AgBlock;


class StarAgmlStacker : public TNamed
{
 public:
  StarAgmlStacker(const Char_t *name="agmlStacker",
		  const Char_t *title="The STAR Geometry Stacker... but this is a virtual class so you shouldn't ever see this!"):TNamed(name,title){ };
  ~StarAgmlStacker(){ /* nada */ };

  virtual Bool_t Build   ( AgBlock *block )=0;//{ return true; };
  virtual Bool_t Position( AgBlock *block, AgPlacement placement )=0;//{ return true; };
  virtual Bool_t Position( AgBlock *block, AgPosition  placement )=0;//{ return true; };

  virtual Bool_t SearchVolume( const AgShape &shape, const AgAttribute &attr )=0;
			
  const std::vector<TString> &nicknames(const Char_t *name)
    {
      return mNicknames[ TString(name) ];
    }

  void Debug( const Char_t *name, const Char_t *flag="position" );

  virtual  void AddGroup( const Char_t *name ) = 0;
  
 private:
 protected:

  AgShape     mShape;
  AgMaterial  mMaterial;
  AgMedium    mMedium;
  AgAttribute mAttribute;

  void SetMediumParameters( TGeoMedium *medium );

  std::vector< TGeoVolume * > mVolumeStack;
  std::map   < TString, TGeoVolume * > mVolumeTable;

  /// Given the name of the block, returns the next possible nickname
  TString nickname( TString agname, Bool_t add=true );

public:
  /// Given the nick name, returns the real name of the block
static  TString realname( TString nname );
protected:
static  std::map   < TString, std::vector< TString > > mNicknames;
static  std::map   < TString, TString >                mRealnames;

  std::map   < TString, TString >                mDebugOptions;
  

 public:
  virtual const Char_t *GetCVS() const {
    static const Char_t cvs[]="Tag  $Name:  $ $Id $ built " __DATE__ " " __TIME__;
    return cvs;
  }


  ClassDef(StarAgmlStacker,1);

};



#endif
