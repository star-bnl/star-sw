#include "StarAgmlStacker.h"

#include "TMath.h"
#include <assert.h>
#include <iostream>

std::map <TString, std::vector< TString > > StarAgmlStacker::mNicknames;
std::map <TString, TString>                 StarAgmlStacker::mRealnames;

void StarAgmlStacker::SetMediumParameters( TGeoMedium *medium )
{

  TGeoMaterial *material = medium -> GetMaterial();
  Double_t      radl     = material-> GetRadLen();

  // Get sensitive volume flag
  Int_t isvol = ( mMedium.isSet("isvol") )? mMedium.par("isvol") : 0;

  ///////////////////////////////////////////////////////////////////////
  //
  // From Geant3 :: gmulof.F -- Recompute the minimum step size.  The
  // minimum step size is important in light materials.  Unless the
  // user sets a value, we will set the step size to be the range of
  // an electron at energy = CUTELE + 200 keV, divided by the sqrt of
  // the radiation length of  the material
  //
  ///////////////////////////////////////////////////////////////////////
  if ( !mMedium.isSet("stmin") )
    {
      
    }

  ///////////////////////////////////////////////////////////////////////
  //
  // From Geant3 :: gphysi.F -- Recompute the maximum energy loss and set
  // the maximum step size (if not set).  For normal volumes, energy 
  // loss is set at 0.25.  If radl > 0.2 we reduce this by 0.2/sqrt(radl).
  // For sensitive volumes, we set the maximum energy loss to 0.2/sqrt(radl).
  //
  ///////////////////////////////////////////////////////////////////////
  if ( !mMedium.isSet("deemax") ) // Set if user did not supply value
    {

      Double_t deemax = 0.25;

      if ( isvol == 0 ) 
	{
	  if ( radl > 2.0 ) 
	    {
	      deemax -= 0.2 / TMath::Sqrt(radl);
	    }
	}
      else 
	{
	  assert(radl>0);                   // radiation length has not been set or computed yet
	  deemax = 0.2 / TMath::Sqrt(radl);
	}

      mMedium.par("deemax") = deemax;
	  
    }

  assert(mMedium.par("deemax")>0.0);        // illegal value for deemax

  
}

// ---------------------------------------------------------------------------------------------------
TString StarAgmlStacker::nickname( TString agname, Bool_t add )
{
  // alphabet
  const char*   alpha[]={ ".",
			  "1","2","3","4","5","6","7","8","9","0",
			  "a","b","c","d","e","f","g","h","i","j",
			  "k","l","m","n","o","p","q","r","s","t",
			  "u","v","w","x","y","z" }; 

  // size of the alphabet
  constexpr unsigned int salpha = sizeof(alpha) / sizeof(char*);

  //
  // If there is no list of nicknames for this block, create an 
  // empty list and add it to the table.  Since this is the first
  // occurence of this name, return this name.
  //
  if ( mNicknames.find(agname) == mNicknames.end() )
    {
      std::vector<TString> v; 
      mNicknames[agname] = v;    
      mRealnames[agname]=agname; 
      return agname;
    }

  //
  // Otherwise, we will begin nickname convention.  Start by getting
  // the vector of nicknames for thie block.
  //
  std::vector<TString> &nicks = mNicknames[agname];
  unsigned int snicks = nicks.size() + 1;

  assert(snicks < salpha*salpha*salpha*salpha);// If you see this assert, you are creating too many volumes.  Consider setting shape at position time.

  TString mynickname = agname;

  unsigned int i3 = snicks % salpha;
  unsigned int i2 = (snicks / salpha) % salpha;
  unsigned int i1 = (snicks / salpha / salpha ) % salpha;

  TString c3 = alpha[i3]; if ( c3 != "." ) mynickname.Replace(3,1,c3);
  TString c2 = alpha[i2]; if ( c2 != "." ) mynickname.Replace(2,1,c2);
  TString c1 = alpha[i1]; if ( c1 != "." ) mynickname.Replace(1,1,c1);

  //std::cout << i3 << " " << c3 << std::endl;

  // Add nickname to list of nicknames, and associate realname with nickname
  nicks.push_back(mynickname);
  mRealnames[mynickname]=agname;

  return mynickname;

};

TString StarAgmlStacker::realname( TString nname )
{
#ifdef __NO_NICKNAMES__
  return nname;
#endif
  return mRealnames[nname];
}

// ---------------------------------------------------------------------------------------------------
void StarAgmlStacker::Debug( const Char_t *name, const Char_t *flag )
{
  mDebugOptions[name]+="|";
  mDebugOptions[name]+=flag;
  mDebugOptions[name]+="|";
  std::cout << GetName() << " -Debug- " << name << " " << mDebugOptions[name].Data() << std::endl;
}


