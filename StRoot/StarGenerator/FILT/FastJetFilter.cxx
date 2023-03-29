#include "FastJetFilter.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

// Defining a User Class to store PID info for particles used as input to FastJet
class MyParticleId : public fastjet::PseudoJet::UserInfoBase{
    public:
        MyParticleId(const int & pdg_id_in, const int & part_id_in) : _pdg_id(pdg_id_in), _part_id(part_id_in){}

        int pid() const{
            return _pdg_id;
        }
        int partn() const{
            return _part_id;
        }
    private:
        int _pdg_id;
        int _part_id;
};


// function to calculate invariant mass of a pair of objects
//___________________________________________________________________________________________
Double_t Mass(Double_t m1, Double_t m2, Double_t E1, Double_t E2, Double_t px1, Double_t px2, Double_t py1, Double_t py2, Double_t pz1, Double_t pz2) {
  Double_t m;
  m = TMath::Sqrt(pow(m1,2) + pow(m2,2) + 2*(E1*E2 - px1*px2 - py1*py2 - pz1*pz2));

  return m;
}

Double_t standardPhi(Double_t phi){
  Double_t phi_standard = phi;
  if (phi_standard < 0) phi_standard+=2*(TMath::Pi()); //FIXME
  if (phi_standard < 0) cout << "Something wrong with angle!" << endl;
  return phi_standard;
}

// This function is to count the number of events in the vertex file.
int countlines(const char* VertexFileName = "./VERTEXFULL/st_physics_15094070_raw_0000007.txt"){
    int numberoflines = 0;
    std::string line;
    std::ifstream file(VertexFileName, ios::in);

    while (std::getline(file, line))
        ++numberoflines;

    return numberoflines;
}

//______________________________________________________________________________________________
FastJetFilter::FastJetFilter( const char* name ) : StarFilterMaker(name)
{

  SetAttr("algorithm","antikt");
  SetAttr("radius", 0.4 );

}
//______________________________________________________________________________________________
void FastJetFilter::AddTrigger( int id, double mnpt, double mxpt, double mneta, double mxeta, int idm )
{
  mTriggers.push_back( Trigger_t( id, mnpt, mxpt, mneta, mxeta, idm ) );
}
//______________________________________________________________________________________________
int FastJetFilter::Init() {

  TString algo = SAttr("algorithm");  algo.ToLower();

  if ( algo == "kt" ) algorithm = fastjet::kt_algorithm;
  else if ( algo == "cambridge" ) algorithm = fastjet::cambridge_algorithm; 
  else algorithm = fastjet::antikt_algorithm;

  double R = DAttr("radius"); assert(R>0);

  jetdefinition = new fastjet::JetDefinition(algorithm, R, recombScheme, strategy);

}
//______________________________________________________________________________________________
int FastJetFilter::Filter( StarGenEvent *_event ) 
{
  // Get a reference to the current event
  StarGenEvent& event = (_event)? *_event : *mEvent;

  // Defining Vectors for FastJet inputs
  std::vector <fastjet::PseudoJet> fjInputs; // Will store px, py, pz, E info

  int npart = event.GetNumberOfParticles();

  // Ensure a D0 is w/in our acceptance
  bool go = false;
  for ( int ipart=1 /*skip header*/; 
	ipart<npart; 
	ipart++ )
    {
      StarGenParticle *part = event[ipart];
      if ( part->GetStatus() != StarGenParticle::kFinal ) continue; // Must be a final state particle

      //      part->Print();

      TLorentzVector momentum = part->momentum();
      double pT  = momentum.Perp();
      double eta = momentum.Eta();

      // Make sure we're looking at something with no color
      if ( TMath::Abs(part->GetId()) != 421 ) continue;
      if ( pT < 0.200 || TMath::Abs(eta) > 1 ) continue;

      go = true;
      break;

    }

  //  if ( false == go ) LOG_INFO << "No candidate D0, reject event" << endm;
  

  if ( false == go ) return StarGenEvent::kReject;



  for ( int ipart=1 /*skip header*/; 
	ipart<npart; 
	ipart++ )
    {
      StarGenParticle *part = event[ipart];

      // Make sure we're looking at something with no color
      if ( TMath::Abs(part->GetId()) < 10 ) continue;

      if ( part->GetStatus() != StarGenParticle::kFinal ) continue; // Must be a final state particle

      TLorentzVector momentum = part->momentum();
      double pT  = momentum.Perp();
      if ( pT < 0.05 ) continue;
      double eta = momentum.Eta();
      if ( pT < 0.20 || TMath::Abs(eta) > 1 ) continue;

      fastjet::PseudoJet pseudoJet(momentum[0], momentum[1], momentum[2], momentum[3]);
      pseudoJet.set_user_info( new MyParticleId( part->GetId(), ipart ) ); // correspondance between PDG and line number

      fjInputs.push_back(pseudoJet);
			           
    }

  vector <fastjet::PseudoJet> inclusiveJets, sortedJets, selectedJets;
  fastjet::ClusterSequence clustSeq(fjInputs, *jetdefinition);
  
  // Extract Inclusive Jets sorted by pT
  inclusiveJets = clustSeq.inclusive_jets(3.); //Only jets with pT > 3 GeV/c are selected
  sortedJets = sorted_by_pt(inclusiveJets);

  LOG_INFO << "Inclusive jets size = " << inclusiveJets.size() << endm;
  LOG_INFO << "Sorted    jets size = " << sortedJets.size() << endm;
  
  fastjet::Selector eta_selector = fastjet::SelectorEtaRange(-0.6, 0.6); // Selects jets with |eta| < 0.6
  fastjet::Selector selector = eta_selector;
  selectedJets = eta_selector(sortedJets);

  bool fInterestingEvent = kFALSE; //Event worth writing to the file

  LOG_INFO << "Looking for D0 jet in " << selectedJets.size() << " candidates" << endm;
  
  for(unsigned jetid = 0; jetid < selectedJets.size(); jetid++){

    // Loop over jet constituents to get some info
    vector <fastjet::PseudoJet> constituents = selectedJets[jetid].constituents();
    vector <fastjet::PseudoJet> sortedconstituents = sorted_by_pt(constituents);
    
    bool d0Jet = kFALSE;
    
    for (unsigned j = 0; j < sortedconstituents.size(); j++){
      if (sortedconstituents[j].user_info<MyParticleId>().pid() == 421){
	d0Jet = kTRUE;
      }
      if (d0Jet) break;
    }
    
    if (d0Jet) {
      fInterestingEvent = kTRUE;
    }

    if (fInterestingEvent) {
      LOG_INFO << "D0 jet found return kAccept! " << endm;
      return StarGenEvent::kAccept;
    }    
  }
  
  //  LOG_INFO << "No D0 jet found, reject event" << endm;

  return StarGenEvent::kReject;

}
