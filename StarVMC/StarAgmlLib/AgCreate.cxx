#include "AgCreate.h"
ClassImp(AgCreate);
#include "StMessMgr.h"
#include <iostream>

// ---------------------------------------------------------------------------------------
AgCreate::AgCreate( const Char_t *name ) : TNamed(name,"Instance of AgCreate")
{
  
}
// ---------------------------------------------------------------------------------------
AgCreate::AgCreate( const AgCreate &other ) 
{
  mParameters = other.mParameters;
}
// ---------------------------------------------------------------------------------------
AgCreate::~AgCreate()
{
  /* nada */ 
}
// ---------------------------------------------------------------------------------------
Double_t & AgCreate::par( const Char_t *name )
{ 
  TString key = name;
  return mParameters[ key ]; // allow the creation of a parameter at any time
}
// ---------------------------------------------------------------------------------------
Bool_t AgCreate::isSet( const Char_t *name ) const
{
  TString key = name;
  return ( mParameters.find(key) != mParameters.end() );
}
// ---------------------------------------------------------------------------------------
void AgCreate::SetParameters( AgShape &shape )
{
  std::map< TString, Double_t >::iterator iter = mParameters.begin();
  while ( iter != mParameters.end() )
    {     
      TString  key = (*iter).first;
      Double_t val = (*iter).second;
 
      if ( !shape.hasPar(key) ) goto NEXT;
 
      shape.par(key) = val;

      NEXT:
	iter++;
    }  
}
// ---------------------------------------------------------------------------------------
void AgCreate::Print( const Option_t *otps ) const
{
  LOG_INFO << "Create : " << GetName() << " " << GetTitle() << endm;
  LOG_INFO << "=============================================================" << endm;
  std::map< TString, Double_t > params = mParameters;
  std::map< TString, Double_t >::iterator iter = params.begin();
  while ( iter != params.end() )
    {     
      TString  key = (*iter).first;
      Double_t val = (*iter).second;
      LOG_INFO << key.Data() << " = " << val << endm;
      iter++;
    }
  
}
