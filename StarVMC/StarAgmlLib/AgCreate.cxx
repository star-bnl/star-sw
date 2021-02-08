#include "AgCreate.h"
ClassImp(AgCreate);
#include "StMessMgr.h"
#include <iostream>

// ---------------------------------------------------------------------------------------
AgCreate::AgCreate( const Char_t *name ) : TNamed(name,"Instance of AgCreate"),
					   AgParameterList()
{
  
}
// ---------------------------------------------- -----------------------------------------
AgCreate::AgCreate( const AgCreate &other ) 
{
  SetName(other.GetName());
  SetTitle(other.GetTitle());
  mParameters = other.mParameters;
}
AgCreate &AgCreate::operator=(const AgCreate& other ) {
  SetName(other.GetName());
  SetTitle(other.GetTitle());
  mParameters = other.mParameters;  
  return *this;
};
// ---------------------------------------------------------------------------------------
AgCreate::~AgCreate()
{
  /* nada */ 
}
// ---------------------------------------------------------------------------------------
#if 0 // lift
Double_t & AgCreate::par( const Char_t *name )
{ 
  TString key = name;
  return mParameters[ key ]; // allow the creation of a parameter at any time
}
// ---------------------------------------------------------------------------------------
Bool_t AgCreate::isSet( const Char_t *name ) const
{
  //TString key = name;
  return ( mParameters.find(name) != mParameters.end() );
}
#endif
// ---------------------------------------------------------------------------------------
void AgCreate::SetParameters( AgShape &shape )
{
  std::map< std::string, Double_t >::iterator iter = mParameters.begin();
  while ( iter != mParameters.end() )
    {     
      std::string key = (*iter).first;
      Double_t val = (*iter).second;
 
      if ( !shape.hasPar(key.c_str()) ) goto NEXT;
 
      shape.par(key.c_str()) = val;

      NEXT:
	iter++;
    }  
}
// ---------------------------------------------------------------------------------------
void AgCreate::Print( const Option_t *otps ) const
{
  LOG_INFO << "Create : " << GetName() << " " << GetTitle() << endm;
  LOG_INFO << "=============================================================" << endm;
  std::map< std::string, Double_t > params = mParameters;
  std::map< std::string, Double_t >::iterator iter = params.begin();
  while ( iter != params.end() )
    {     
      std::string  key = (*iter).first;
      Double_t val = (*iter).second;
      LOG_INFO << key << " = " << val << endm;
      iter++;
    }
  
}
