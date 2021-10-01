#include "StMCParticleStack.h"

#include <assert.h>

#include "TMCProcess.h"
using namespace std;

#include <StMessMgr.h>
#include <TVirtualMC.h>

#include <TGeoManager.h>
#include <TGeoNavigator.h>
#include <TGeoVolume.h>
#include <TGeoMedium.h>

#include <StarVMC/StarAgmlLib/AgMLExtension.h>
#include <GeometryUtils.h>
#include <StMaker.h>

const int kDefaultStackSize = 400;
const int kDefaultArraySize = 4000;

ostream&  operator<<(ostream& os,  const StarMCVertex&   vert) {
  os << Form("[StMCVertex %p] (%f,%f,%f)",
	     (void*)&vert,
	     vert.vx(),
	     vert.vy(),
	     vert.vz());
  return os;
}
ostream&  operator<<(ostream& os,  const StarMCParticle& part) {
  os << Form("[StMcParticle %s %p] stat=%i pdg=%i p=(%f,%f,%f;%f) v=(%f,%f,%f)",part.GetName(),(void*)&part,
	     part.GetStatus(), part.GetPdg(),
	     part.px(),part.py(),part.pz(),part.E(),
	     part.vx(),part.vy(),part.vz() 
            );



  if ( part.start() ) {
    os << std::endl << " [start] " << *part.start();
  }

  for ( auto i : part.intermediate() ) {
    os << std::endl << " [inter] " << *i;
  }

  if ( part.stop() ) {
    os << std::endl << " [stop]  " << *part.stop();
  }

  return os;
}
ostream&  operator<<(ostream& os,  const      TParticle& part) {
  os << Form("[TParticle    %s %p] stat=%i pdg=%i p=(%f,%f,%f;%f) v=(%f,%f,%f) mother=%i daughters=%i %i",
	     part.GetName(),
	     (void*)&part,
	     part.GetStatusCode(),part.GetPdgCode(),
	     part.Px(),part.Py(),part.Pz(),part.Energy(),
	     part.Vx(),part.Vy(),part.Vz(),
	     part.GetFirstMother(), part.GetFirstDaughter(), 
	     part.GetLastDaughter());
  return os;
}




//___________________________________________________________________________________________________________________
//___________________________________________________________________________________________________________________
StMCParticleStack::StMCParticleStack( const Char_t *name ) : 
  TVirtualMCStack(),
  mNumPrimary(0),
  mCurrent(-1),
  mArraySize(0),
  mArray(0),
  mStackSize(0),
  mStack(),
  mStackIdx(),
  mTruthTable(),
  mParticleTable(),
  mVertexTable(),
  mStackToTable(),
  mIdTruthFromParticle(),
  mScoringRmax(450.0),
  mScoringZmax(2000.0),
  mScoringEmin(0.01)
{

  mArray     = new TClonesArray("TParticle", kDefaultArraySize );

}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
StMCParticleStack::~StMCParticleStack()
{
  if ( mArray ) delete mArray; 
}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
void StMCParticleStack::PushTrack( int toDo, int parent, int pdg, 
					   double px, double py, double pz, double energy,
					   double vx, double vy, double vz, double vt,
					   double polx, double poly, double polz,
					   TMCProcess mech, int& ntr, double weight,
					   int is )
{

  // Determine whether we are in a tracking region or calorimeter region
  static auto* navigator    = gGeoManager->GetCurrentNavigator();
         auto* node         = navigator->FindNode( vx, vy, vz );   
         auto* volume       = (node) ? node->GetVolume() : 0;

	 if ( 0 == volume ) {
	   LOG_INFO << "Attempt to push a track @ v="<< vx << "," << vy << "," << vz << " ... no node found (FindNode)" << endm;
	   LOG_INFO << "... current path = " << navigator->GetPath() << endm;
	   return; // drop track on the ground
	 }

  // AgMLExtension* agmlext = dynamic_cast<AgMLExtension*>( node->GetUserExtension() );
  // if ( 0==agmlext ) {
  //   agmlext = dynamic_cast<AgMLExtension*>( volume->GetUserExtension() );
  // }

  AgMLExtension* agmlext = getExtension( node );

  int agmlreg = (agmlext) ? agmlext->GetTracking() : 2; // 1=calorimeter 2=tracking 0=blackhole
  
  if (0==agmlreg) {
    LOG_WARN << "... dropped into a blackhole ..." << endm;
    return; // complete drop of particle into blackhole
  }    
    
  //
  // Add a new particle to the array.  Note:  VMC standard PushTrack does not specify the children
  // (or 2nd parent) of particles.  So these are entered with value of -1.
  //
  TClonesArray &array = (*mArray);
  TParticle *particle = new(array[mArraySize]) TParticle( pdg, is, parent, -1, -1, -1, 
						      px, py, pz, energy, vx, vy, vz, vt);
  particle->SetPolarisation( polx, poly, polz );
  particle->SetWeight(weight);
  particle->SetUniqueID(mech);

  bool isPrimary = parent<0;
  mNumPrimary += isPrimary;

  // Add to the stack of particles
  if ( toDo )    {

      mStack.push_back( particle );
      mStackIdx.push_back( mArraySize ); // store stack ID

    }  
  ntr = mArraySize; // guess this is supposed to be track number (index in array)
          
  // Increment mArraySize
  mArraySize++;


  double Rmax2=mScoringRmax*mScoringRmax;
  double vr2 = vx*vx+vy*vy;

  bool tracing = (vr2<Rmax2) && (TMath::Abs(vz)<mScoringZmax) && energy > mScoringEmin;
  

  //
  // And handle region-based track persistence
  //
  if ( agmlreg == 2 && tracing || isPrimary ) {

    StarMCVertex* vertex = GetVertex( vx, vy, vz, vt, mech );

    mParticleTable.push_back(new StarMCParticle(particle,vertex)); // mParticleTable owns the pointer

    mIdTruthFromParticle[ mParticleTable.back() ] = mParticleTable.size();
    // if ( parent > 0 ) { 
    //   mTruthTable.push_back( mParticleTable.back() );
    // }

    // Set corrspondance between stack ID and table ID
    mStackToTable[ntr] = mParticleTable.back();
    mStackToTable[ntr]->setIdStack( ntr ); 

    // add this particle as a daughter of the vertex
    vertex->addDaughter( mParticleTable.back() );  

    auto* navigator = gGeoManager->GetCurrentNavigator();
    auto* volume    = navigator->GetCurrentVolume();
    auto* medium    = volume->GetMedium();
    int   imed      = medium->GetId();

    vertex->setMedium(imed);
    vertex->setProcess( mech );

  }


}
//___________________________________________________________________________________________________________________
StarMCParticle* StMCParticleStack::GetPersistentTrack( int stackIndex ) {    
  auto track = mStackToTable[ stackIndex ];
  assert(track);
  return track;
}
//___________________________________________________________________________________________________________________
StarMCVertex* StMCParticleStack::GetVertex( double vx, double vy, double vz, double vt, int proc ) {

  StarMCVertex* vertex = 0;

  // TODO: Make eps a class parameter
  const double eps=0.0000005;  
  if ( proc >= 0 ) 
  for ( auto vtx : mVertexTable ) {
    double dist = vtx->distance(vx,vy,vz);   
    if ( dist < eps ) {
      if ( vtx->process() == proc ) {
	vertex=vtx;
	break;
      }
    }
  }

  if ( 0==vertex ) {
    mVertexTable.push_back( vertex = new StarMCVertex(vx,vy,vz,vt) ); // mVertexTable owns pointer
    auto* navigator = gGeoManager->GetCurrentNavigator();
    auto* volume    = navigator->GetCurrentVolume();
    mVertexTable.back()->setVolume( volume->GetName() );
  }

  return vertex;

}
//___________________________________________________________________________________________________________________
TParticle *StMCParticleStack::PopNextTrack( int &itrack )
{

 
  // Start with invalid track index
  itrack = -1;

  // The stack is empty.  Signal the end.
  if ( mStack.empty() ) 
    {
      return NULL;
    }

  // Get the particle on the top of the stack
  TParticle *particle = mStack.back();    mStack.pop_back();
  itrack              = mStackIdx.back(); mStackIdx.pop_back();
  mCurrent            = itrack;
  
  return particle;

}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
TParticle *StMCParticleStack::PopPrimaryForTracking( int i ) 
{
  assert(i<mArraySize);
  return (TParticle *)(*mArray)[i];
}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
void StMCParticleStack::SetCurrentTrack( int tn )
{
  mCurrent = tn;
}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
int StMCParticleStack::GetNtrack() const
{
  return mArray->GetEntriesFast();
}
//___________________________________________________________________________________________________________________
//
//___________________________________________________________________________________________________________________
TParticle *StMCParticleStack::GetCurrentTrack() const
{
  return GetParticle(mCurrent);
}
//___________________________________________________________________________________________________________________
int StMCParticleStack::GetCurrentTrackNumber() const
{
  return mCurrent;
}
//___________________________________________________________________________________________________________________
int StMCParticleStack::GetCurrentParentTrackNumber() const
{
  TParticle *current = GetCurrentTrack();
  return (current)? current->GetFirstMother() : -1;
}
//___________________________________________________________________________________________________________________
TParticle *StMCParticleStack::GetParticle( int idx ) const
{
  return (TParticle *)(*mArray)[idx];
}
//___________________________________________________________________________________________________________________
void StMCParticleStack::Clear( const Option_t *opts )
{

#if 0
  LOG_INFO << "TParticle table" << endm;
  int index = 0;
  for ( auto obj : (*mArray) ) {

    TParticle* part = dynamic_cast<TParticle*>( obj );  assert(part);
    LOG_INFO << Form("[%04i] ",index++) << *part << endm;

  }

  index = 0;
  LOG_INFO << "StarMCParticle table" << endm;  
  for ( auto p : mParticleTable ) {
    LOG_INFO << Form("[%04i] ",index++) << *p << endm;
  }

  index = 0;
  LOG_INFO << "StarMCVertex table" << endm;
  for ( auto v : mVertexTable ) {
    LOG_INFO << Form("[%04i] ",index++) << *v << endm; 
    if ( v->parent() ) {
      LOG_INFO << "      " << *(v->parent()) << " --> " << endm; 
    }
    for ( auto d : v->daughters() ) {
        LOG_INFO << "       " << *d << endm; 
    } 
  }
#endif


  mArray->Clear();
  mStack.clear();
  mCurrent = -1;
  mArraySize = 0;
  mStackSize = 0;

  for ( auto v : mVertexTable ) { if (v) delete v; }
  for ( auto p : mParticleTable ) { if (p) delete p; }

  mVertexTable.clear();
  mTruthTable.clear();
  mParticleTable.clear(); 

}
//___________________________________________________________________________________________________________________







//___________________________________________________________________________________________________________________
StarMCParticle::StarMCParticle( TParticle* part, StarMCVertex* vert ) : 
  mStacked(part), 
  mStartVertex(vert),
  mIntermediateVertices(),
  mStopVertex(0),
  mIdStack(-1), 
  mNumHits(0),
  mHits()
{
  
}
//___________________________________________________________________________________________________________________						
void StarMCParticle::addHit( DetectorHit* hit ) {
  mHits.push_back( hit );
}
//___________________________________________________________________________________________________________________						
StarMCVertex::StarMCVertex() : mVertex{0,0,0,0},
			       mParent(0),
			       mDaughters(),			   					   
		    mMechanism(kPNoProcess),
		    mMedium(0),
		    mVolume("unkn"),
		    mIntermediate(false)
		    
{


}
//___________________________________________________________________________________________________________________						
StarMCVertex::StarMCVertex( double x, double y, double z, double t, StarMCParticle* parent) : mVertex{x,y,z,t},
			       mParent(parent),
			       mDaughters(),			   					   
		    mMechanism(kPNoProcess),
		    mMedium(0),
		    mVolume("unkn"),
		    mIntermediate(false)
{


}
//___________________________________________________________________________________________________________________						
double StarMCParticle::vx() const { assert(mStartVertex); return mStartVertex->vx(); }
double StarMCParticle::vy() const { return mStartVertex->vy(); }
double StarMCParticle::vz() const { return mStartVertex->vz(); }
