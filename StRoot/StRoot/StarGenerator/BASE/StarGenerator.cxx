#include "StarGenerator.h"
ClassImp(StarGenerator);
#include "TClass.h"
#include "TTree.h"

#include "StarGenerator/EVENT/StarGenParticle.h"
#include "TSystem.h"

//TString altLib = "StarGeneratorPool";

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
StarGenerator::StarGenerator( const Char_t *name )//, StarGenEvent *event ) 
  : StMaker(name), mm(0.1), cm(1.0),
    mId(0),
    mNumberOfParticles(0),
    mEvent(0),
    mBlue("-"),
    mYell("-"),
    mFrame("CMS"),
    mRootS(510.0),
    mDirect(1.0),
    mImpactMin(0.0),
    mImpactMax(-1.0),
    mBlueMomentum(0,0,+255,255.0), 
    mYellMomentum(0,0,-255,255.0),
    mBlueMass(0),
    mYellMass(0),
    mIOMode(0),
    mInputTree(0),
    mOutputTree(0),
    mIsPileup(false),
    mPileup(1.0),
    mParticleDb(0),
    mLibrary("na")
{

  //  mEvent = (event)? event : new StarGenEvent("default");
  for ( Int_t i=0;i<4;i++ ) { mBlueMomentum[i]=0;
                              mYellMomentum[i]=0; }

  mParticleDb = &StarParticleData::instance();

  Clear();
    
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::Clear(const Option_t *opts)
{
  mNumberOfParticles=0;
  if(mEvent) mEvent->Clear(); 
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarGenerator::Finalize()
{
  // Increment the event counter
  ++(*mEvent);
  return 1;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetBlue( const Char_t *b )
{
  mBlue=b;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetYell( const Char_t *y )
{
  mYell=y;
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetFrame( const Char_t *frame, const Double_t value )
{
  mFrame=frame;
  mFrame.ToUpper();
  mRootS=TMath::Abs(value);
  mDirect=value / mRootS;  assert(mDirect==1.0 || mDirect==-1.0); 
  
  if ( mFrame!="CMS" && mFrame != "FIXT" )
    {
      Error(GetName(),"This method only applies to CMS and FIXT frames");
    }
  assert(mFrame=="CMS"||mFrame=="FIXT");

}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetFrame( const Char_t *frame, const Double_t *pb, const Double_t *py )
{
  mFrame=frame;
  mFrame.ToUpper();
  if ( !mFrame.Contains("MOM") )
    {
      Error(GetName(),"This method only applies to 3MOM, 4MOM or 5MOM frames");
    }
  assert(mFrame=="3MOM"||mFrame=="4MOM"||mFrame=="5MOM");

  Int_t n=3; if ( mFrame=="4MOM" ) n=4;
  for ( Int_t i=0;i<n;i++ ) {
    mBlueMomentum[i]=pb[i];
    mYellMomentum[i]=py[i];
  }
  if (mFrame=="5MOM") {
    mBlueMass=pb[4];
    mYellMass=py[4];
  }

  cout << "== SetFrame == " << endl;
  mBlueMomentum.Print();
  mYellMomentum.Print();
  cout << "mBlue: " 
       << mBlueMomentum.X() << " " 
       << mBlueMomentum.Y() << " " 
       << mBlueMomentum.Z() << " " 
       << mBlueMomentum.T() << " "  << endl;
  cout << "mYell: " 
       << mYellMomentum.X() << " " 
       << mYellMomentum.Y() << " " 
       << mYellMomentum.Z() << " " 
       << mYellMomentum.T() << " "  << endl;

}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetOutputTree( TTree *tree )
{
  mOutputTree = tree;
  mIOMode = 1;
  TString bname  = GetName();
  TString bclass = mEvent->IsA()->GetName();
  ///   Split level should depend on the class of the branch...
  ///   It should be split such that user classes have the
  ///   base class variables revealed.
  mOutputTree -> Branch( bname, bclass, &mEvent, 64000, 1 );
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetInputFile( const Char_t *filename, const Char_t *treename, const Char_t *branchname )
{
  TFile *file = TFile::Open( filename, "read" );
  if ( 0 == file )
    {
      LOG_WARN << "-- WARNING: root event file " << filename << " not found." << endm;
      return;
    }
  TTree *tree = (TTree *)file->Get(treename);
  if ( 0 == tree )
    {
      LOG_WARN << "-- WARNING: tree "<< treename<< " not found in " << filename << endm;
      return;
    }
  SetInputTree(tree, branchname);

};
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
void StarGenerator::SetInputTree( TTree *tree, const Char_t *name )
{
  mInputTree = tree;
  mIOMode = 2;
  TString bname = mEvent->GetName();  
  if ( name ) 
    {
      bname = name;
    }

  mInputTree -> SetBranchAddress( bname, &mEvent );
  //  TBranch *branch = mInputTree->GetBranch( bname );
  //  branch -> SetAddress( &mEvent );

  LOG_INFO << "Input Tree with " << tree -> GetEntries() << " loaded" << endm;
  
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarGenerator::PreGenerateHook()
{  

  // Always add a 0th line to the event, containing some (redundant) summary
  // information:
  // 
  Int_t status = -201; // Flag this as the event generator line
  Int_t pdgid  = 0;    // Rootino
  
  //
  // NOTE: PDG values for std model particles are obtained from PDG book.
  //       PDG values for heavy ions are calculated.
  //

  Int_t mother1 = 0, kid1 = -1, mother2 = 0, kid2 = -1;

  //////////////////////////////////////////////////////////////////////////////
  //
  // Extract out the blue beam and yellow beam PDG ids based on particle name.
  //
  TParticlePDG *blue = mParticleDb -> GetParticle( mBlue ); 
  if ( blue ) {
    mother1 = blue->PdgCode();
  }
  else {
    // ADD PDG id
  }
    

  TParticlePDG *yell = mParticleDb -> GetParticle( mYell ); 
  if ( yell ) {
    mother2 = yell->PdgCode();
  }
  else {
    // ADD PDG id
  }
  //
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  //
  // kid1 = the generator ID
  // kid2 = the number of particles generated (set after PostGenerate())
  //
  //////////////////////////////////////////////////////////////////////////////
  kid1 = mId;

  Double_t px   = mBlueMomentum.Px();       // variables illustrate the event record entry
  Double_t py   = mBlueMomentum.Py();
  Double_t pz   = mBlueMomentum.Pz();
  Double_t E    = mBlueMomentum.Energy();
  Double_t M    = mRootS;

  Double_t vx   = mYellMomentum.Px();
  Double_t vy   = mYellMomentum.Py();
  Double_t vz   = mYellMomentum.Pz();
  Double_t tof  = mYellMomentum.Energy();

  mEvent -> AddParticle( status, pdgid, mother1, mother2, kid1, kid2, px, py, pz, E, M, vx, vy, vz, tof );

  //
  // General event information
  //
  mEvent -> SetBlue( mother1 );
  mEvent -> SetYell( mother2 );
  mEvent -> SetRootS( mRootS );
  

  return PreGenerate();
}
// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------
Int_t StarGenerator::PostGenerateHook()
{

  Int_t stat = PostGenerate();

  // Set the last daughter to be the number of particles generated in this event
  Int_t kid2 = GetNumberOfParticles();
  (*mEvent)[0]->SetLastDaughter( kid2 );

  return stat;
}

