#include "AgMedium.h"
#include "AgBlock.h"
#include "AgModule.h"
#include <assert.h>
#include <iostream>
#include "StMessMgr.h"

// -----------------------------------------------------------------------------------------------------------
// lifted std::vector< TString >             AgMedium::mParameterList;
std::map   < TString, AgMedium * > AgMedium::mMediumTable;
std::vector< TString >             AgMedium::mMediumList;
//
struct _MediumDummy{
  _MediumDummy(){
#if 0 // lifted
    AgMedium::mParameterList.push_back("isvol"); // 0
    AgMedium::mParameterList.push_back("ifield");// 1
    AgMedium::mParameterList.push_back("fieldm");// 2
    AgMedium::mParameterList.push_back("tmaxfd");// 3
    AgMedium::mParameterList.push_back("stemax");// 4
    AgMedium::mParameterList.push_back("deemax");// 5
    AgMedium::mParameterList.push_back("epsil"); // 6
    AgMedium::mParameterList.push_back("stmin"); // 7
    // for ( Int_t i=0;i<12;i++ )
    // {
    //	  TString key="user"; key+=i;
    //	  AgMedium::mParameterList.push_back(key); // Add user1 .. user12 words
    // }
    const Char_t *keys[]=
      {"cutgam", "cutele", "cutneu", "cuthad",
       "cutmuo", "bcute",  "bcutm",  "dcute",
       "dcutm",  "ppcutm"
      };
    for ( Int_t i=0;i<10;i++ )
      {
	AgMedium::mParameterList.push_back(keys[i]);
      }
#endif
    AgMedium::mMediumTable["Standard"]=new AgMedium("Standard");
    AgMedium::mMediumList.push_back("Standard");
  };
} _medium_dummy_init;

Bool_t
AgMedium::isEqual( const AgMedium &Other )
{
  AgMedium other = Other;
  for ( UInt_t i=0;i<mParameterList.size();i++ )
    {
      TString key = mParameterList[i];
      if ( isSet(key) )
	{
	  if ( !other.isSet(key) ) return false;
	  if ( par(key) != other.par(key) ) return false;
	}
    }
  return true;
}

// -----------------------------------------------------------------------------------------------------------
AgMedium::AgMedium(const Char_t *name):TNamed(name,Form("Medium for %s",name))
{

  // Add to parameter list
  mParameterList = {
    "isvol", "ifield", "fieldm", "tmaxfd", 
    "stemax","deemax","epsil", "stmin",
    "cutgam", "cutele", "cutneu", "cuthad",
    "cutmuo", "bcute",  "bcutm",  "dcute",   
    "dcutm",  "ppcutm"
  };


  par("isvol")=0;              // Set default parameters
  par("ifield")=1;
  par("fieldm")=20;
  par("tmaxfd")=20;
  par("epsil")=0.01;
  par("stemax") = 10.0; 
  // Leave stmin and deemax unset for calculation in StarAgmlStacker

    const Char_t *keys[]=
      {"cutgam", "cutele", "cutneu", "cuthad",
       "cutmuo", "bcute",  "bcutm",  "dcute",
       "dcutm",  "ppcutm"
      };
    for ( Int_t i=0;i<10;i++ )	par(keys[i])=0.001; // default cuts to old starsim values


}

// -----------------------------------------------------------------------------------------------------------
#if 0 // lifted
Bool_t AgMedium::isSet( const Char_t *par ) const
{
  const char* key=par;
  return ( mParameters.find(key) != mParameters.end() );
}

// -----------------------------------------------------------------------------------------------------------
Bool_t AgMedium::hasPar(const Char_t *par ) const
{
  const char* key=par;
  std::vector<std::string> parlist = mParameterList;
  for ( UInt_t i=0;i<parlist.size();i++ )
    if ( parlist[i] == key ) return true;
  return false;
}

// -----------------------------------------------------------------------------------------------------------
Double_t &AgMedium::par(const Char_t *name){ return mParameters[name]; }
#endif

// -----------------------------------------------------------------------------------------------------------
void AgMedium::Inherit( AgBlock *block )
{
  AgMedium *other = block->medium();

  assert(other);
  // Keep a list of properties which we inherit from the parent block
  TString inlist;

  // Loop over the list of parameters
  for ( UInt_t i=0;i<mParameterList.size();i++ )
    { TString key=mParameterList[i];

      // Skip any parameter which is set on this material
      if (isSet(key)) continue;

      // If the other material has the current key
      // and is set, then we set that material property
      if ( other->hasPar(key) && other->isSet(key) )
      {
    	  par(key) = other->par(key);
    	  inlist += Form("%s ",key.Data());
      }
    }

  TString name = GetName();
  if ( name == "None" )
    {
      SetName  ( other->GetName() );
    }
  SetTitle ( Form("Medium %s inheriting %s from block %s",GetName(),inlist.Data(),block->GetName()) );

}

// -----------------------------------------------------------------------------------------------------------
AgMedium AgMedium::CopyMedium( const Char_t *name )
{
//  AgMedium *medium = mMediumTable[name];
//  if ( !medium )
//    {
//      std::cout << "AgMedium::Copy(const Char_t *name)" << Form("No such medium %s in medium table",name) << std::endl;
//      assert(2+2==5);
//    }
//  // Copy the medium and return the copy
//  AgMedium copy=*medium;
//  return copy;
	return Get(name);
}

// -----------------------------------------------------------------------------------------------------------
AgMedium &AgMedium::Get( const Char_t *name )
{

	  AgModule   *module = AgBlock::module();
  //	  AgBlock    *block  = 0; //  AgBlock::active(); // uncomment to ensure independent media per volume when declared by user
	  AgMedium   *medium = 0;

	  TString modname="None";
	  if ( module ) {
		  modname = module->GetName();
	  }


	  //////////////////////////////////////////////////////////////
	  //
	  // If there is no module, probably being intialized above.
	  //
	  //////////////////////////////////////////////////////////////
	  if ( modname == "None" )
	    {
	      medium = Find(name);
	      if ( !medium )
		  {
		     medium = new AgMedium(name);
		     mMediumList.push_back(name);
		     mMediumTable[name]=medium;
		  }
	      return (*mMediumTable[name]);

	    }

	  //////////////////////////////////////////////////////////////
	  //
	  // Lookup medium by name MODULE_MEDIUM and return
	  // it if it exists
	  //
	  //////////////////////////////////////////////////////////////

	  TString   medname = modname+"_"+name;

	  medium = Find(medname);
	  if ( medium )
	    {
	      return (*medium);
	    }

	  //////////////////////////////////////////////////////////////
	  //
	  // If it doesn't exist, lookup the named material and
	  // copy it into MODULE_MATERIAL.  Then return.
	  //
	  //////////////////////////////////////////////////////////////

	  medium = Find(name);
	  if ( medium )
	    {

	      AgMedium nm(medname);
	      nm = *medium;

	      mMediumTable[medname]=new AgMedium(nm);
	      mMediumTable[medname]->SetName(medname);
	      mMediumTable[medname]->SetTitle(Form("Medium %s created in module %s",medname.Data(),modname.Data()));
	      mMediumList.push_back(medname);
	      return (*mMediumTable[medname]);
	    }

	  //////////////////////////////////////////////////////////////
	  //
	  // Otherwise we cannot find an appropriate medium so we
	  // will be creating a new one which inherits all of its
	  // properties from the mother block
	  //
	  //////////////////////////////////////////////////////////////

	  //std::cout << "Returning new medium " << medname.Data() << std::endl;

	  medium = new AgMedium(medname);

	  if ( AgBlock::previous() )                    // Inherit from creating block
	    medium->Inherit( AgBlock::previous() );
	  else if ( AgBlock::module() )                 // Or from the current material loaded in module
	    medium->Inherit( AgBlock::module() );

	  if ( AgBlock::active() )                      // And finally the active block
	    medium->Inherit( AgBlock::active() );

	  medium->SetName(medname);
	  mMediumTable[medname]=medium;
	  mMediumList.push_back(medname);

	  return (*mMediumTable[medname]);

  //  return (*medium);
}

// -----------------------------------------------------------------------------------------------------------
AgMedium *AgMedium::Find( const Char_t *name )
{
  if (!IsDefined(name))
    {
      return NULL;
    }
  return mMediumTable[name];
}

//------------------------------------------------------------------------------------------------------------
Bool_t AgMedium::IsDefined( const Char_t *name )
{
  TString key = name;
  return mMediumTable.find(key) != mMediumTable.end();
}

// -----------------------------------------------------------------------------------------------------------
AgMedium::AgMedium( const AgMedium &other )
{
  SetName( other.GetName() );
  SetTitle( other.GetTitle() );
  mParameters = other.mParameters;
}

// -----------------------------------------------------------------------------------------------------------
void AgMedium::Print( const Option_t *opts ) const
{

	TString name = GetName();
	//LOG_INFO << "["<< ((mLock)?"-":"+") << "] " << Form("%30s:",name.Data()) << " ";
	LOG_INFO << Form("%20s",name.Data()) << " ";
	std::map<std::string, Double_t > mypar=mParameters;
	for ( UInt_t j=0;j<mParameterList.size();j++ )
	{
	  const char* key=mParameterList[j].c_str();
		if ( isSet(key) )
		  { LOG_INFO << Form(" %s=%9.5g",key,mypar[key]); }
		else
		  { LOG_INFO << Form(" %s= <unset> ",key); }
	}
	LOG_INFO << endm;

}

// -----------------------------------------------------------------------------------------------------------
void AgMedium::List( const Char_t *opts )
{
	TString key=opts;
	if ( TString(opts) == "ALL" )
		for ( UInt_t i=0;i<mMediumList.size();i++ )  {
			mMediumTable[ mMediumList[i] ]->Print();
		}
	else if ( IsDefined(key) ) {
		mMediumTable[ key ]->Print();
	}
	else  {
		std::cout << Form("Medium %s not found",opts) << std::endl;
	}
}

