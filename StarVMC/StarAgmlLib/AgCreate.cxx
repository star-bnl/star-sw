#include "AgCreate.h"
ClassImp(AgCreate);

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
      //      std::cout << "Set " << key.Data() << " = " << val << std::endl;
      if ( !shape.hasPar(key) ) goto NEXT;
      //      std::cout << "has par" << std::endl;
      //      if (  shape.isSet(key)  ) goto NEXT;
      //      std::cout << "not set" << std::endl;

      shape.par(key) = val;

      NEXT:
	iter++;
    }  
}
// ---------------------------------------------------------------------------------------
void AgCreate::Print( const Option_t *otps ) const
{
  std::cout << "Create : " << GetName() << " " << GetTitle() << std::endl;
  std::cout << "=============================================================" << std::endl;
  std::map< TString, Double_t > params = mParameters;
  std::map< TString, Double_t >::iterator iter = params.begin();
  while ( iter != params.end() )
    {     
      TString  key = (*iter).first;
      Double_t val = (*iter).second;
      std::cout << key.Data() << " = " << val << std::endl;
      iter++;
    }
  
}
