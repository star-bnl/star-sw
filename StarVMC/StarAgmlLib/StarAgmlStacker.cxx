#include "StarAgmlStacker.h"
ClassImp(StarAgmlStacker);

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

  /**********************************************************************
   *                                                                    *
          Subroutine   A g S N A M E   (Volume,Ign,CNick)
   *                                                                    *
   *  Description:  generate a NickName for (Volume,Ign) instance       *
   **********************************************************************

#include "commons/typing.inc"
#include "geant321/gcunit.inc"
   Character*1 Symb
   Character*4 Volume,Cnick
   Integer     Ign,ii,i,j,k

   Cnick=Volume;  
   Unless 0<=Ign & Ign <=1368 {
        <W> Ign,Volume;(' AgSNAME: Bad volume number ',I5,' for Volume ',A); Return;
   }

   ii=Ign;                                  ! Start with the number of generic names

   do k=4,3,-1                              ! Loop over last two characters
   {  i=mod(ii,37);   ii=ii/37;   

      check i>0                             ! Return if i==0

      j=i+48;   If(i==10) j=48;             ! 1 2 3 ... 8 9 0 ...
      If(i>10) j=96+(i-10)                  ! a b c ... x y z
      Call ITOCH(j,Symb,*:err:);   Cnick(k:k)=Symb
   }  :err:
   END


   */



  const Char_t *nn[]={ "0","1","2","3","4","5","6","7","8","9",
		       "a","b","c","d","e","f","g","h","i","j",
		       "k","l","m","n","o","p","q","r","s","t",
		       "u","v","w","x","y","z" }; // 36

  const Char_t *aa[]={ "1","2","3","4","5","6","7","8","9","0" };

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

  Int_t i3 = i%36;       // last character will be modulo 36
  Int_t i2 = (i/36)%36;
  Int_t i1 = (i/36/36)%36; 

  if ( i>=0   && i3!=0 ) mynickname.Replace(3,1,nn[i3]);
  if ( i < 10   ) mynickname.Replace(3,1,aa[i3]);
  if ( i>=36  && i2!=0 ) mynickname.Replace(2,1,nn[i2]);
  if ( i>=36*36 && i1!=0 ) mynickname.Replace(1,1,nn[i1]);

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

// ---------------------------------------------------------------------------------------------------
void StarAgmlStacker::Debug( const Char_t *name, const Char_t *flag )
{
  mDebugOptions[name]+="|";
  mDebugOptions[name]+=flag;
  mDebugOptions[name]+="|";
  std::cout << GetName() << " -Debug- " << name << " " << mDebugOptions[name].Data() << std::endl;
}


