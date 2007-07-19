//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 19 July 2007
//
// Algorithms and implementations by Michael Betancourt (MIT)
//

#include "StMcEvent/StMcEventTypes.hh"
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"
#include "StGammaPythiaEvent.h"
#include "StGammaPythiaEventMaker.h"

ClassImp(StGammaPythiaEventMaker);

int StGammaPythiaEventMaker::Make()
{
  // Get StMcEvent
  StMcEvent* mcEvent = (StMcEvent*)GetDataSet("StMcEvent");
  if (!mcEvent)
    {
      LOG_WARN << "No StMcEvent" << endm;
      return kStWarn;
    }

  long pid = mcEvent->subProcessId();

  // Check pointer to Pythia event
  if (!mPythia)
    {
      LOG_WARN << "No StGammaPythiaEvent" << endm;
      return kStWarn;
    }

  // Get base StPythiaEvent pointer from StMCAsymMaker
  StMCAsymMaker* asym = (StMCAsymMaker*)GetMaker("MCAsym");
  if (!asym) {
    LOG_WARN << "No StMCAsymMaker" << endm;
    return kStWarn;
  }

  new (mPythia) StGammaPythiaEvent(asym->pythiaEvent());

  // Collect photons from primary vertex
  StMcVertex *primary=mcEvent->primaryVertex();
        
  // Loop over daughters of primary vertex
  for(UInt_t id = 0; id < primary->numberOfDaughters(); ++id)
    {
        
      StMcTrack *track = primary->daughter(id);
      if ( !track ) continue;
      const StLorentzVectorF& trackV=track->fourMomentum();
            
      // Store all pion Four Momentum
      if (track->geantId() == 7)
	{
	  mPythia->pion0().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
	}
            
      //////////////////////////////
      // Check for prompt photons //
      //////////////////////////////
            
      // Check if subprocess can produce a prompt photon
      if( (pid==14)||(pid==18)||(pid==29)||(pid==114)||(pid==115) )
	{
                
	  // Require that track is a photon
	  bool prompt_flag = (track->geantId() == 1);
	  // Require that track is not a daughter of the initial partons
	  //prompt_flag &= particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 > 5;
	  prompt_flag &= track->parent()->eventGenLabel() >= 5;
	  // Require that track is a daughter of a photon (how PYTHIA handles the evoluton of the
	  // final state prompt photon to a stable photon)
	  //prompt_flag &= particleTable[particleTable[track->eventGenLabel() - 1].jmohep[0] - 1].idhep == 22;
	  prompt_flag &= track->parent()->pdgId() == 22;
                
	  // Store any prompt photons
	  if(prompt_flag)
	    {
	      mPythia->prompt().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
	    }
	  // All other photons are treated as decay photons
	  else if(track->geantId() == 1)
	    {

	      //bool radiation_flag = particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 < 6;
	      bool radiation_flag = track->parent()->eventGenLabel() <= 6;
	      if(radiation_flag)
		{
		  mPythia->initial().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		}
	      else
		{
            
		  bool decay_flag = track->parent()->geantId() == 7;
		  decay_flag |= track->parent()->geantId() == 17;
		  if(decay_flag)
		    {
		      mPythia->decay().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		    }
		  else
		    {
		      mPythia->frag().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		    }
                        
		}
                
	    }
                
	}
        
      // Store 2-2 QCD decay photons
      if( (pid==11)||(pid==12)||(pid==13)||(pid==28)||(pid==53)||(pid==68)||(pid==96) )
	{
            
	  if(track->geantId() == 1)
	    {

	      //bool radiation_flag = particleTable[track->eventGenLabel() - 1].jmohep[0] - 1 < 6;
	      bool radiation_flag = track->parent()->eventGenLabel() <= 6;
	      if(radiation_flag)
		{
		  mPythia->initial().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		}
	      else
		{          
            
		  bool decay_flag = track->parent()->geantId() == 7;
		  decay_flag |= track->parent()->geantId() == 17;
		  if(decay_flag)
		    {
		      mPythia->decay().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		    }
		  else
		    {
		      mPythia->frag().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
		    }
                        
		}
                
	    }

	}
        
    }

  // Collect photons from secondary vertices
  collectDecayPhotons();  
    
  return kStOk;
}

void StGammaPythiaEventMaker::collectDecayPhotons()
{
  if (StMcEvent* event = (StMcEvent*)GetDataSet("StMcEvent")) 
    {
      if (StMcVertex* vertex = event->primaryVertex()) 
        {
	  for (unsigned i = 0; i < vertex->numberOfDaughters(); ++i) 
            {
	      StMcTrack* track = vertex->daughter(i);
	      if (StMcVertex* stopVertex = track->stopVertex()) 
                {
		  collectDecayPhotons(stopVertex);
                }
            }
        }
    }
}

void StGammaPythiaEventMaker::collectDecayPhotons(StMcVertex* vertex)
{

  for (unsigned i = 0; i < vertex->numberOfDaughters(); ++i) 
    {
    
      StMcTrack* track = vertex->daughter(i);
      const StLorentzVectorF& trackV = track->fourMomentum();
        
      // Store all pion Four Momentum
      if (track->geantId() == 7)
        {
	  mPythia->pion0().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
        }
        
      if (track->geantId() == 1) 
        {
            
	  bool decay_flag = track->parent()->geantId() == 7;
	  decay_flag |= track->parent()->geantId() == 17;
	  if(decay_flag)
            {
	      mPythia->decay().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }
	  else
            {
	      mPythia->frag().push_back(TLorentzVector(trackV.px(),trackV.py(),trackV.pz(),trackV.e()));
            }

        }

      if (StMcVertex* stopVertex = track->stopVertex()) 
        {
	  collectDecayPhotons(stopVertex);
        }
    }
}
