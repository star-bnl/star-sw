#include "AgModule.h"
#include "TClass.h"
#include "TDataSet.h"
#include "TObjectSet.h"
#include "TGenericTable.h"

#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "AgMLExtension.h"
#include "StMessMgr.h"

ClassImp(AgModule);

TDataSet *AgModule::mGeomSet = 0;

// ---------------------------------------------------------------------------------------------
AgModule::AgModule( const Char_t *name, const Char_t *comment ) : AgBlock(name,comment), mDataSet(0), mHitsSet(0), mBlocksSet(0), mTrackingFlag(1), mHitScoring() 
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

void AgModule::AddHitScoring( TString name, AgMLScoring* sc ){ mHitScoring[name] = sc; }

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
//______________________________________________________________________________________________
void AgModule::AddHitScoring( TString name, AgMLScoring* sc ) {
  TString key (name(0,4));
  //  LOG_INFO << "Add hit scoring " << key.Data() << endm;
  mHitScoring[name] = sc;
  auto* volume = gGeoManager->FindVolumeFast(key);
  if (0==volume) {
    LOG_WARN << "Volume " << key.Data() << " has not been created yet, no user hits defined " << name.Data() << endm;
  }
  else {
    auto* ext = AgMLExtension::get( volume );
    ext->AddHitScoring(sc);
  }
}
//______________________________________________________________________________________________








