#include "AgAttribute.h"
#include "AgBlock.h"
#include "StMessMgr.h"
ClassImp(AgAttribute);
// -----------------------------------------------------------------------------------------------------------
std::vector< TString > AgAttribute::mParList;
//std::map   < TString, Double_t > AgAttribute::mParameters;

struct _AttributeDummy{
  _AttributeDummy(){
    AgAttribute::mParList.push_back("seen");
    AgAttribute::mParList.push_back("colo");
    AgAttribute::mParList.push_back("serial");
    AgAttribute::mParList.push_back("fill");
    AgAttribute::mParList.push_back("lsty");
    AgAttribute::mParList.push_back("lwid");
    AgAttribute::mParList.push_back("trans");
  };
} _attribute_dummy_init;
// -----------------------------------------------------------------------------------------------------------
AgAttribute::AgAttribute(const Char_t *name):TNamed(name,Form("Attributes for %s",name))
{

}
AgAttribute::AgAttribute(const AgAttribute &other)
{
	SetName(other.GetName());
	SetTitle(other.GetTitle());
	mParameters = other.mParameters;
}
Bool_t AgAttribute::isSet( const Char_t *par ) const
{
  TString key=par;
  return ( mParameters.find(key) != mParameters.end() );
}
Bool_t AgAttribute::hasPar(const Char_t *par ) const
{
  TString key=par;
  std::vector<TString> parlist = mParList;
  for ( UInt_t i=0;i<parlist.size();i++ )
    if ( parlist[i] == key ) return true;
  return false;
}
Double_t &AgAttribute::par(const Char_t *name)
{
  return mParameters[name];
}

Bool_t AgAttribute::unSet( const Char_t *par )
{
  TString key = par;
  std::map<TString,Double_t>::iterator pos = mParameters.find(key);
  if ( pos != mParameters.end() )
    {
      mParameters.erase(pos);
      return true; // successfully removed element
    }
  return true; // didn't need to remove element
}

void AgAttribute::Inherit( AgBlock *prev )
{
  AgAttribute *other = prev->attribute();
  for ( UInt_t i=0;i<mParList.size();i++ )
    {
      TString key = mParList[i];
      if ( !isSet(key) )
	{
	  if ( other->isSet(key) ) par(key)=other->par(key);
	}
    }
  
}

void AgAttribute::Print( const Option_t *o ) const
{
  // Copy parameters
  std::map< TString, Double_t > params = mParameters;

  // Loop over all parameters in parlist
  for ( UInt_t j=0;j<mParList.size();j++ )
    {
      // Key
      TString key=mParList[j];

      // If the key is set print it out
      if ( isSet(key) )
	{
	  LOG_INFO << Form(" %s=%7.4g",key.Data(), params[key] ) << endm;
	}
      else
	{
	  LOG_INFO << Form(" %s=<unset>",key.Data()) << endm;
	}
    }
}
