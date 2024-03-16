#ifndef __unit_tests__
#define __unit_tests__
#include <StarGenerator/EVENT/StarGenParticle.h>
#include <StarGenerator/BASE/StarPrimaryMaker.h>
#include <StarGenerator/Kinematics/StarKinematics.h>
#include <StChain.h>
#include <iostream>
#include <TVector3.h>
#include <TSystem.h>
#include <TStopwatch.h>
#include <TMath.h>
#include <g2t_track.h>
#include <g2t_vertex.h>
#include <g2t_tpc_hit.h>
#include <g2t_fts_hit.h>
#include <g2t_emc_hit.h>
#include <g2t_epd_hit.h>
#include <g2t_ctf_hit.h>
#include <g2t_mtd_hit.h>
#include <TTable.h>
#include <TROOT.h>
#include <string>
#define __COLOR__
#ifdef __COLOR__
const std::string FAIL  = "\u001b[31m -failed- \u001b[0m";
const std::string PASS  = "\u001b[32m -passed- \u001b[0m";
const std::string NOPE  = "\u001b[33m - nope - \u001b[0m";
const std::string YES   = "\u001b[33m - yes - \u001b[0m";
const std::string UNKN  = "\u001b[33m -unknown- \u001b[0m";
const std::string TODO  = "\u001b[36m -todo- \u001b[0m";
const std::string NADA  = "\u001b[36m - N/A - \u001b[0m";
const std::string GIVEN = "\u001b[34m - given - \u001b[0m";
#else
const std::string FAIL = " -failed- ";
const std::string PASS = " -passed- ";
const std::string UNKN = " -unknown- ";
const std::string TODO = " -todo- ";
const std::string NADA = " - n/a - ";
#endif

std::string __PREFIX__ = " \u001b[35m | \u001b[0m";

bool Conditional( std::string result ) {
  bool value = true;
  if ( result.find(FAIL) != std::string::npos ) {
    value = false;
  }
  return value;
}

using namespace std;
//___________________________________________________________________
#define LOG_TEST std::cout << __PREFIX__
//___________________________________________________________________
TTable* hit_table    = 0;
TTable* track_table  = 0;
TTable* vertex_table = 0;
static TVector3 _vector3;
//___________________________________________________________________
TStopwatch timer;
//___________________________________________________________________
double _pmom = 0;
void throw_muon( double eta, double phid, double pT = 25.0, int q=1 ) {
  // eta  = pseudorapidity
  // phid = azimuthal angle in degrees
  double phi = TMath::Pi() * phid / 180.0;
  TVector3 momentum;
  momentum.SetPtEtaPhi(pT,eta,phi);
  _pmom = momentum.Mag();
  auto* chain = StMaker::GetChain();
  auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
  auto* particle = _kine->AddParticle( (q==1)?"mu+":"mu-" );
  particle->SetPx(momentum[0]);
  particle->SetPy(momentum[1]);
  particle->SetPz(momentum[2]);
  double mass = particle->GetMass();
  double ener = sqrt( momentum.Mag2() + mass*mass );
  particle->SetEnergy(ener);
  chain->Clear();
  chain->Make();
}
void throw_particle( const char* part, double eta, double phid, double pT = 25.0, int q=1 ) {
  // eta  = pseudorapidity
  // phid = azimuthal angle in degrees
  double phi = TMath::Pi() * phid / 180.0;
  TVector3 momentum;
  momentum.SetPtEtaPhi(pT,eta,phi);
  _pmom = momentum.Mag();
  auto* chain = StMaker::GetChain();
  auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
  auto* particle = _kine->AddParticle( part );
  particle->SetPx(momentum[0]);
  particle->SetPy(momentum[1]);
  particle->SetPz(momentum[2]);
  double mass = particle->GetMass();
  double ener = sqrt( momentum.Mag2() + mass*mass );
  particle->SetEnergy(ener);
  chain->Clear();
  chain->Make();
}
void throw_particle( int n, const char* part, double ptmn, double ptmx, double etamn, double etamx, double phimn, double phimx ) {
  auto* chain = StMaker::GetChain();
  auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );
  _kine->Kine( n, part, ptmn, ptmx, etamn, etamx, phimn, phimx );
  chain->Clear();
  chain->Make();
}
void add_particle( const char* part, double eta, double phid, double pT = 25.0, int q=1 ) {
  double phi = TMath::Pi() * phid / 180.0;
  TVector3 momentum;
  momentum.SetPtEtaPhi(pT,eta,phi);
  _pmom = momentum.Mag();
  auto* chain = StMaker::GetChain();                                                                                                                                                                                                                                                
  auto* _kine = dynamic_cast<StarKinematics*>( chain->GetMaker("StarKine") );                                                                                                                                                                                                       
  auto* particle = _kine->AddParticle( part );                                                                                                                                                                                                                                      
  particle->SetPx(momentum[0]);                                                                                                                                                                                                                                                     
  particle->SetPy(momentum[1]);                                                                                                                                                                                                                                                     
  particle->SetPz(momentum[2]);                                                                                                                                                                                                                                                     
  double mass = particle->GetMass();                                                                                                                                                                                                                                                
  double ener = sqrt( momentum.Mag2() + mass*mass );                                                                                                                                                                                                                                
  particle->SetEnergy(ener);                                
}
//___________________________________________________________________
std::string check_track_table( std::string message, std::function<std::string(g2t_track_st* begin_, g2t_track_st* end_)> f) {
  g2t_track_st* first = static_cast<g2t_track_st*>( track_table->GetArray() );
  g2t_track_st* last  = static_cast<g2t_track_st*>( track_table->GetArray() ) + track_table->GetNRows();
  std::string result = "\u001b[37m [" + message + "] " + (track_table? f(first,last):FAIL );  
  LOG_TEST << result << std::endl;
  return result;
}
//___________________________________________________________________
std::string check_track( std::string message, std::function<std::string(const g2t_track_st*)> f, int idx=0) {
  const g2t_track_st* track = static_cast<const g2t_track_st*>( track_table->At(idx) );  
  std::string result = "\u001b[37m [" + message + "] " + (track? f(track):FAIL );
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_vertex( std::string message, std::function<std::string(const g2t_vertex_st*)> f, int idx=0) {
  const g2t_vertex_st* vertex = static_cast<const g2t_vertex_st*>( vertex_table->At(idx) );
  std::string result = "\u001b[37m [" + message + "] " + (vertex? f(vertex):FAIL );
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_tpc_hit( std::string message, const g2t_tpc_hit_st* hit, std::function<std::string(const g2t_tpc_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_stg_hit( std::string message, const g2t_fts_hit_st* hit, std::function<std::string(const g2t_fts_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_ctf_hit( std::string message, const g2t_ctf_hit_st* hit, std::function<std::string(const g2t_ctf_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_mtd_hit( std::string message, const g2t_mtd_hit_st* hit, std::function<std::string(const g2t_mtd_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_emc_hit( std::string message, const g2t_emc_hit_st* hit, std::function<std::string(const g2t_emc_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
std::string check_emc_hit( std::string message, std::function<std::string(const g2t_emc_hit_st*)> f, int idx=0) {
  const g2t_emc_hit_st* hit = static_cast<const g2t_emc_hit_st*>( hit_table->At(idx) );
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
//___________________________________________________________________
std::string check_epd_hit( std::string message, const g2t_epd_hit_st* hit, std::function<std::string(const g2t_epd_hit_st*)> f) {
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};
std::string check_epd_hit( std::string message, std::function<std::string(const g2t_epd_hit_st*)> f, int idx=0) {
  const g2t_epd_hit_st* hit = static_cast<const g2t_epd_hit_st*>( hit_table->At(idx) );
  std::string result = "\u001b[37m [" + message + "] " + (hit? f(hit):FAIL);
  LOG_TEST << result << std::endl;
  return result;
};

//___________________________________________________________________
template<typename Hit>
TH1F* gimmeTH1F( std::string name, std::string title, int nbin, double xmn, double xmx, 
		 std::function<bool(const Hit*, TH1F* histogram)> filler ) {
  TH1F* histo = new TH1F(name.c_str(), title.c_str(), nbin, xmn, xmx );
  for ( int i=0;i<hit_table->GetNRows();i++ ){
    const Hit* hit = static_cast<const Hit*>( hit_table->At(i) );
    filler( hit, histo );
  }
  return histo;
}
//___________________________________________________________________
ostream& operator<<(  ostream& os, const g2t_vertex_st& v ) {
  os << Form("g2t_vertex id=%i ",v.id);
  os << Form("volume=%s ",v.ge_volume);
  os << Form("np=%i nd=%i itrmd=%i x=(%f, %f, %f)",v.n_parent,v.n_daughter,v.is_itrmd,v.ge_x[0],v.ge_x[1],v.ge_x[2]);
  return os;
};
//___________________________________________________________________
ostream& operator<<(  ostream& os, const g2t_tpc_hit_st& h ) {
  os << Form("g2t_tpc_hit id=%i de=%f ds=%f",h.id,h.de,h.ds);
  return os;
};
//___________________________________________________________________
ostream& operator<<(  ostream& os, const g2t_emc_hit_st& h ) {
  double r2 = h.x*h.x+h.y*h.y;
  os << Form("g2t_emc_hit id=%i idtruth=%i volume=%i de=%f R=%f",h.id,h.track_p,h.volume_id,h.de,sqrt(r2));
  return os;
};
//___________________________________________________________________
// ostream& operator<<(  ostream& os, const g2t_wca_hit_st& h ) {
//   double r2 = h.x*h.x+h.y*h.y;
//   os << Form("g2t_wca_hit id=%i idtruth=%i volume=%i de=%f R=%f",h.id,h.track_p,h.volume_id,h.de,sqrt(r2));
//   return os;
// };
// //___________________________________________________________________
// ostream& operator<<(  ostream& os, const g2t_hca_hit_st& h ) {
//   double r2 = h.x*h.x+h.y*h.y;
//   os << Form("g2t_hca_hit id=%i idtruth=%i volume=%i de=%f R=%f",h.id,h.track_p,h.volume_id,h.de,sqrt(r2));
//   return os;
// };
#endif
