#include "AgModule.h"
#include "TClass.h"

ClassImp(AgModule);

//std::vector< TString > AgModule::mDetectorParameters;

// ---------------------------------------------------------------------------------------------
AgModule::AgModule( const Char_t *name, const Char_t *comment ) : AgBlock(name,comment)
{  
  _module = this;
}
// ---------------------------------------------------------------------------------------------
AgBlock *AgModule::AddBlock( const Char_t *name )
{
  AgBlock *block = mBlocks[ name ];
  if ( !block )
    {
      TClass *_class = TClass::GetClass( name );
      if ( !_class )
      {
    	  Warning("AgModule::AddBlock(const Char_t *name)",Form("Block %s declared in content but not defined.",name));
    	  return NULL;
      }
      mBlocks[name] = (AgBlock *) _class -> New();
      block = mBlocks[name];
      block -> Init();
      block -> SetModule( this );
      AgBlock::mBlockTable[ name ] = block; // add block to the block table
    } 
  return block;
}
// ---------------------------------------------------------------------------------------------
AgModule::~AgModule()
{
  _module = NULL;
}
// ---------------------------------------------------------------------------------------------
AgStructure *AgModule::GetStructure( const Char_t *name )
{
  // Get the name of the current module
  TString module = AgModule::module()->GetName();

  // Get the sub folder for this module
  TFolder *folder = (TFolder*)AgStructure::top().FindObject(module.Data());
  if ( !folder ) 
    {
      Warning(GetName(),Form("/DETP/%s/%s not found",GetName(),name));
      return NULL;
    }
  
  AgStructure *structure = (AgStructure*)folder->FindObjectAny(name);
  return structure;

}













//void AgModule::FunctorLoop()
//{
//	for ( UInt_t i=0;i<mDetpFunctors.size();i++ )
//	{
//		(*mDetpFunctors[i])();
//	}
//}

// ---------------------------------------------------------------------------------------------
// void AgModule::SetParameter( const Char_t *module, const Char_t *structure, const Char_t *selector, const Char_t *variable, Int_t    value )
// {
// 	TString detp = Form("scalar: %s,%s,%s,%s,%i",module,structure,selector,variable,value);
// 	mDetectorParameters.push_back(detp);
// }

// void AgModule::SetParameter( const Char_t *module, const Char_t *structure, const Char_t *selector, const Char_t *variable, Int_t n, Int_t    *value )
// {
// 	TString detp = Form("array:  %s,%s,%s,%s,%i",module,structure,selector,variable,n);
// 	for ( Int_t i=0;i<n;i++ )
// 	{
// 		detp += Form(",%i",value[i]);
// 	}
// 	mDetectorParameters.push_back(detp);
// }

// void AgModule::ListParameters( Option_t *opts)
// {
// 	for ( UInt_t i=0;i<mDetectorParameters.size();i++ )
// 	{
// 		TString detp = mDetectorParameters[i];
// 		std::cout << detp.Data() << std::endl;
// 	}
// }
