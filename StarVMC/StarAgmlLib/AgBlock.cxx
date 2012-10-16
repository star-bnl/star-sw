#include "AgBlock.h"
ClassImp(AgBlock);

#include "AgShape.h"
#include <iostream>
#include "AgMaterial.h"
#include "AgShape.h"
//#include "AgMedium.h"
#include "AgModule.h"
#include "AgPlacement.h"

#include "StarAgmlStacker.h"

// Setup static members
AgBlock                      *AgBlock::mCurrent=0;
std::map<TString, AgBlock *>  AgBlock::mBlockTable;
std::vector< AgBlock *>       AgBlock::mStack;

StarAgmlStacker *AgBlock::_stacker = 0;
AgModule *AgBlock::_module = 0;

struct _AgBlockDummy {
  _AgBlockDummy(){
    AgBlock::mStack.push_back(NULL);
  };
} _agblockdumdum_;

AgBlock::AgBlock(const Char_t *name, const Char_t *title)
  : TNamed(name,title)
{
  addNickname(name); // ensure that the vector is populated
  _valid = true;
}



void AgBlock::Create( const Char_t *name )
{
  // Get pointer to the named block
  AgBlock *block = mBlockTable[ name ];
  if ( !block )
    {
      Warning("Create(const Char_t *name)",Form("The specified block %s is not in the block table.",name));
      Warning("Create(const Char_t *name)",Form("Was the block added to the module with AddBlock?"));
      return;	      
    }
  
  // Push the current block onto the stack
  mStack.push_back( this );

  // Execute the ** next ** block's code
  //$$$  mStack.back()->Block();
  block -> Block( _create );

  // And any cleanup code
  //$$$ mStack.back()->End();
  block -> End();

  // Pop the stack to restore parentage
  mStack.pop_back();

}


void AgBlock::AddGroup( const Char_t *name )
{
  mGroups.push_back(name);
  _stacker -> AddGroup(name);
}


;//{ mGroups.push_back( name ); }


// >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> WHY IS THIS COMMENTED OUT???
void AgBlock::Inherit()
{
  // Nothing to do here because this is an orphan block
  if ( !mMother ) 
    return;
  //$$$  _shape.Inherit( mMother -> _shape );
}

void AgBlock::Inherit( const AgShape &shape )
{
}

void AgBlock::Inherit( const AgMaterial &material )
{
}

void AgBlock::Inherit( const AgMedium &medium )
{
}

void AgBlock::Inherit( const AgAttribute &attribute )
{
}


void AgBlock::InheritFrom( AgShape &shape, Bool_t zeros )
{
  
  // List of parameters defined on the shape
  std::vector< TString > pars = shape.parList();

  // Loop over the parameters
  for ( UInt_t i=0;i<pars.size();i++ )
    {

    }
  
}

AgBlock* AgBlock::previous(Int_t offset)
{ 
  UInt_t size = mStack.size();
  UInt_t off  = offset;
  UInt_t index = size - off - 1;
  //  std::cout << mStack.back() << " ==??== " << mStack[size-1] << std::endl;
  if (index>=0)
    {
      return mStack[index];
    }

  return NULL;
  //return mStack.back(); 
}


AgBlock *
AgBlock::Find( const Char_t *name)
{
  return mBlockTable[ name ];
}

void 
AgBlock::Print(Option_t *opts)const
{
  std::cout << "========================================================================" << std::endl;
  std::cout << GetName() << " :: " << GetTitle() << std::endl;
  std::cout << "Current instance:  " << mNicknames.back().Data() << std::endl;
  std::cout << "Defined in module: " << _module -> GetName() << std::endl;
  std::cout << "========================================================================" << std::endl;
  _material.Print(); std::cout << std::endl;
  _medium.Print();   std::cout << std::endl;
  _shape.Print();
}

void AgBlock::List(Option_t *opts)
{

  TString Opts=opts;

  std::cout << "========================================================================================" << std::endl;
  std::cout << "List of known blocks: " << std::endl;
  std::map< TString, AgBlock *>::iterator iter = mBlockTable.begin();
  while ( iter != mBlockTable.end() )
    {
      TString name = (*iter).first;
      if ( Opts!="ALL" ) if ( name!=opts ) 
	{
	  iter++;
	  continue;
	}     
      AgBlock *ptr = (*iter).second;
      TString cmnt = "";
      if ( ptr ) cmnt=ptr->GetTitle();
      //std::cout << name.Data() << " ptr=" << ptr << std::endl;
      std::cout << Form("%s %80s ptr=%p",name.Data(),cmnt.Data(),ptr) << " ";
      std::vector<TString> nicks = _stacker -> nicknames(name);
      UInt_t nnicks = nicks.size();
      if ( nicks.size() ) 
	{
	  std::cout << nicks[0].Data() << " ";
	  if ( nicks.size()>1 )
	    std::cout << ".. " << nicks.back().Data()<< " ";
	  std::cout << nnicks << std::endl;
	}
      std::cout << std::endl;

      iter++;
    }
  std::cout << std::endl;
  
}

// ---------------------------------------------------------------------------------------------------
// --
//Bool_t AgBlock::isSame( const AgAttribute &attr )
//{
//  Bool_t same = true;
//  same &= attr("serial") == _attribute("serial");
//  return same;
//
//}

