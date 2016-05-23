//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 3 August 2015
//

#include "StjTowerRegion.h"

ClassImp(StjTowerRegion);

StjTowerEnergyList StjTowerRegion::Do( const StjTowerEnergyList& towerList, const StJetCandidate* leadingjet, const TString bname)
{
  StjTowerEnergyList elist;
  // Tower loop
  for (StjTowerEnergyList::const_iterator iTower = towerList.begin(); iTower != towerList.end(); ++iTower) {
    StjTowerEnergy tower = *iTower;
    if(bname == "toward"){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - tower.towerPhi)) < (mphiplus * TMath::DegToRad()) && TMath::Abs(tower.towerEta) <  mdeta ){
	elist.push_back(tower);
      }
    }
    if(bname == "away"){
      if(TMath::Abs(TVector2::Phi_mpi_pi( leadingjet->phi() - tower.towerPhi)) > (mphiplus * TMath::DegToRad()) && TMath::Abs(tower.towerEta) <  mdeta ){
	elist.push_back(tower);
      }
    }
    if(bname == "transP" || bname == "transM"){ 
      if(TVector2::Phi_mpi_pi( leadingjet->phi() - tower.towerPhi) < (mphiplus * TMath::DegToRad()) &&  TVector2::Phi_mpi_pi( leadingjet->phi() - tower.towerPhi) > (mphiminus * TMath::DegToRad()) && TMath::Abs(tower.towerEta) <  mdeta ){
	elist.push_back(tower);
      }
    }
  } // End tower loop  
  return elist;
}
