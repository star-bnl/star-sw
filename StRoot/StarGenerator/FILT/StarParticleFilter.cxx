#include "StarParticleFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

//______________________________________________________________________________________________
StarParticleFilter::StarParticleFilter( const char* name ) : StarFilterMaker(name)
{

}
//______________________________________________________________________________________________
void StarParticleFilter::AddTrigger( int id, double mnpt, double mxpt, double mneta, double mxeta )
{
  mTriggers.push_back( Trigger_t( id, mnpt, mxpt, mneta, mxeta ) );
}
//______________________________________________________________________________________________
int StarParticleFilter::Filter( StarGenEvent *_event ) 
{
  // Get a reference to the current event
  StarGenEvent& event = (_event)? *_event : *mEvent;

  // Loop over tracks to find particles of interest
  int npart = event.GetNumberOfParticles();
  for ( int ipart=1 /*skip header*/; 
	ipart<npart; 
	ipart++ )
    {
      StarGenParticle *part = event[ipart];
      // Make sure we're looking at something with no color
      if ( TMath::Abs(part->GetId()) < 10 ) continue;


      TLorentzVector momentum = part->momentum();
      double pT  = momentum.Perp();
      if ( pT < 0.05 ) continue;
      double eta = momentum.Eta();

      for ( auto trig : mTriggers )
	{
	  if ( trig.pdgid != part->GetId() )              continue;
	  if ( pT  < trig.ptmn  )                         continue;
	  if ( pT  > trig.ptmx && trig.ptmn < trig.ptmx ) continue;
	  if ( eta < trig.etamn )                         continue;
	  if ( eta > trig.etamx )                         continue;
	  return StarGenEvent::kAccept;	  
	}

    }

  return StarGenEvent::kReject;

}
