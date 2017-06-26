#include "AgModule.h"
#include "TClass.h"
#include "TDataSet.h"
#include "TObjectSet.h"
#include "TGenericTable.h"


TDataSet *AgModule::mGeomSet = 0;

// ---------------------------------------------------------------------------------------------
AgModule::AgModule( const Char_t *name, const Char_t *comment ) : AgBlock(name,comment)
{  
  _module = this;
  mDataSet = (TDataSet *)( new TObjectSet( name, this, false ) );
  mDataSet -> SetTitle(comment);
  mDataSet -> Shunt( Geom() );
  mDataSet -> Add( mHitsSet   = new TDataSet("hits") );   mHitsSet   -> SetTitle("Hit digitization parameters" );
  mDataSet -> Add( mBlocksSet = new TDataSet("blocks") ); mBlocksSet -> SetTitle("Volume construction blocks" );
}

TDataSet *AgModule::Geom()
{
  if (!mGeomSet) return mGeomSet = new TDataSet("Geom", NULL, false );
  else           return mGeomSet;
};

Bool_t AgModule::AddHit( string _for, string meas, Float_t bitns, Float_t mn, Float_t mx, string opts )
{
  TGenericTable *hits = dynamic_cast<TGenericTable *>(mDataSet->Find(Form("hits/%s",_for.c_str())));
  if ( !hits ) {
    mDataSet -> Find("hits") -> Add( hits = new TGenericTable("HitSet_t",_for.c_str()) );
  };
  HitSet_t myhit;// = { meas.c_str(), bitns, mn, mx, opts.c_str() };
  {
    {size_t len = meas.copy( myhit.meas, TMath::Min( sizeof(myhit.meas)-1, meas.size() ) ); myhit.meas[len]='\0';}
    {size_t len = opts.copy( myhit.opts, TMath::Min( sizeof(myhit.opts)-1, opts.size() ) ); myhit.opts[len]='\0';}
    myhit.nb = bitns;
    myhit.min = mn;
    myhit.max = mx;
    hits->AddAt(&myhit);
  }
  return true;
};

Bool_t AgModule::AddCut( string block, string cut, Float_t value )
{

  TDataSet      *BLOCKS   = mDataSet -> Find("blocks");
  TDataSet      *THEBLOCK = BLOCKS   -> Find(block.c_str());
  TGenericTable *G3cuts   = dynamic_cast<TGenericTable *>( THEBLOCK -> Find("G3cuts") );


  if ( 0==G3cuts )
    {
      //      cout << "Register new cut table for block " << block.c_str() << endl;
      // Create the table and add default set of cuts
      THEBLOCK -> Add( G3cuts = new TGenericTable( "Gccuts_t", "G3cuts" ) );
      Gccuts_t cuts; /* new default cuts */ 
      G3cuts -> AddAt( &cuts );
    }

  // Set new cut value for this cut
  ((Gccuts_t *)G3cuts->At(0))->set( cut, value );
  return true;
};

Bool_t AgModule::AddPar( string block, string cut, Float_t value )
{

  TDataSet      *BLOCKS   = mDataSet -> Find("blocks");
  TDataSet      *THEBLOCK = BLOCKS   -> Find(block.c_str());
  TGenericTable *G3birk   = dynamic_cast<TGenericTable *>( THEBLOCK -> Find("G3birk") );
  TGenericTable *G3phlt   = dynamic_cast<TGenericTable *>( THEBLOCK -> Find("G3phlt") );


  if ( 0==G3birk || 0==G3phlt )
    {
      //      cout << "Register new par tables for block " << block.c_str() << endl;
      // Create the table and add default set of cuts
      THEBLOCK -> Add( G3birk = new TGenericTable( "Gcbirk_t", "G3birk" ) );
      THEBLOCK -> Add( G3phlt = new TGenericTable( "Gcphlt_t", "G3phlt" ) );
      Gcbirk_t birk;       G3birk -> AddAt( &birk );
      Gcphlt_t phlt;       G3phlt -> AddAt( &phlt );
    }

  // Set new cut value for this cut
  ((Gcbirk_t *)G3birk->At(0))->set( cut, value );
  ((Gcphlt_t *)G3phlt->At(0))->set( cut, value );
  return true;
};



// Bool_t AgModule::AddHit( const Char_t *for, const Char_t *meas, Float_t bins, Float_t mn=0, Float_t mx=0, const Char_t *opts="" ) {
//   return true;
// };

// ---------------------------------------------------------------------------------------------
AgBlock *AgModule::AddBlock( const Char_t *name, AgBlock *_block )
{
 
  TString Name      = name;
  TString Module    = GetName();
  TString NameSpace = Module;  NameSpace.ToUpper();

  Module.ToUpper();
  Name = Module + "::" + Name;

  // Get block from list of blocks defined in this module
  AgBlock *block = mBlocks[ name ];   

  if ( !block )
    {
      mBlocks[name]=_block; // should use a singleton to pass here

      block = mBlocks[name];
      block -> Init();
      block -> SetModule( this );

      //
      // We store the block in the master block table.  NOTE WELL -- we may
      // not have solved all namespace issues here.  Ideally we should be
      // hashing the block name according to the namespace... i.e.
      //
      //   mBlockTable[ NameSpace + "::" + name ] = block.
      //
      // The problem is one of accessing... there are some cases where Find
      // may not work properly.  This needs to be revisited.  For now, this
      // workaround seems to do the trick.
      //
      AgBlock::mBlockTable[ name ] = block; // add block to the block table


      //
      // Add block dataset
      //
      TDataSet *set = mDataSet -> Find("blocks");
      set -> Add( new TObjectSet( name, block, false ) );
      TDataSet *bset = set -> Find( name );
      bset -> SetTitle( block -> GetTitle() );
      
      //
      // Setup block-specific tables
      //
      //      bset->Add( new TGenericTable( "Gccuts_t", "G3cuts" ) ); bset->Find("G3cuts")->SetTitle("G3 propagation cuts in this medium");
      //      bset->Add( new TGenericTable( "Gcphys_t", "G3phys" ) ); bset->Find("G3phys")->SetTitle("G3 physics params in this medium");


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
