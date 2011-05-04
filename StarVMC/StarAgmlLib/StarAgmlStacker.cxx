#include "StarAgmlStacker.h"
ClassImp(StarAgmlStacker);

#include "TMath.h"
#include <assert.h>

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

  const Char_t *nn[]={ "1","2","3","4","5","6","7","8","9","0",
		       "a","b","c","d","e","f","g","h","i","j",
		       "k","l","m","n","o","p","q","r","s","t",
		       "u","v","w","x","y","z" }; // 36

#ifdef __NO_NICKNAMES__
  return agname;
#endif

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

   // Get a reference to the list of nicknames. 
  std::vector<TString> &nicks = mNicknames[agname];

  UInt_t i = nicks.size();

  TString mynickname=agname;
  assert(i<36*36*36*36); // TOO MANY NICKNAMES

  UInt_t i3 = i%36;       // last character will be modulo 36
  UInt_t i2 = (i/36)%36;
  UInt_t i1 = (i/36/36)%36;

  //  std::cout << Form("i3=%i i2=%i i1=%i ",i3,i2,i1);

  if ( i>=0     ) mynickname.Replace(3,1,nn[i3]);
  if ( i>=36    ) mynickname.Replace(2,1,nn[i2]);
  if ( i>=36*36 ) mynickname.Replace(1,1,nn[i1]);

  //  std::cout << mynickname << std::endl;

  nicks.push_back(mynickname);
  mRealnames[mynickname]=agname;
  return mynickname;
  
}

TString StarAgmlStacker::realname( TString nname )
{
#ifdef __NO_NICKNAMES__
  return nname;
#endif
  return mRealnames[nname];
}




