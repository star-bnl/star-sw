/***************************************************************************
 *
 * $Id: StV0SpectraAnalysis.cc,v 1.2 2000/03/28 03:20:06 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: Strangeness Analysis class
 *
 ***************************************************************************
 *
 * $Log: StV0SpectraAnalysis.cc,v $
 * Revision 1.2  2000/03/28 03:20:06  munhoz
 * correcting normalization of weighted histogram
 *
 * Revision 1.1  2000/03/23 03:21:49  munhoz
 * added V0 classes
 *
 **************************************************************************/

#include "StV0SpectraAnalysis.h"
#include "StSpectraCutDcaParent.h"
#include "StSpectraCutDcaDaughters.h"
#include "StMessMgr.h"
#include "StPionPlus.hh"
#include "StPionMinus.hh"
#include "StProton.hh"
#include "StAntiProton.hh"

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include <string.h>
#include <typeinfo>

StV0SpectraAnalysis::StV0SpectraAnalysis():StSpectraAnalysis()
{
  // Default Constructor. Sets default values for Mass axis
  setZAxis(0.,1.5,150);
}

StV0SpectraAnalysis::~StV0SpectraAnalysis() 
{}

void StV0SpectraAnalysis::setParticle(string particle)
{
  // This method is overloaded from StSpectraAnalysis to allow
  // the definition of the daughter particles

  StSpectraAnalysis::setParticle(particle);

  if (mParticle->name() == "kaon0S") {
    mDaughterPos = StPionPlus::instance();
    mDaughterNeg = StPionMinus::instance();
  }
  else if (mParticle->name() == "lambda") {
    mDaughterPos = StProton::instance();
    mDaughterNeg = StPionMinus::instance();
  }
  else if (mParticle->name() == "anti_lambda") {
    mDaughterPos = StPionPlus::instance();
    mDaughterNeg = StAntiProton::instance();
  }
  else {
    gMessMgr->Message("This is NOT a V0 particle!","E");
    return;
  }
}

void StV0SpectraAnalysis::setZAxis(float lZbin, float uZbin, int nZbin) 
{
  // Set the Invariant mass axis
  mlMassBin = lZbin;
  muMassBin = uZbin ;
  mnMassBins = nZbin;
}

void StV0SpectraAnalysis::bookHistograms() 
{
  mNumEvent = 0;
 
  // use abscissa and ordinate types

  string hlab2D = "";
  if (mAbscissa == kRapidity && mOrdinate == kPperp) {
    hlab2D = "YPt";
  } else if (mAbscissa == kRapidity && mOrdinate == kTransverseMass) {
    hlab2D = "YMt";
  } else if (mAbscissa == kPseudoRapidity && mOrdinate == kPperp) {
    hlab2D = "EtaPt";
  } 

  string hlab3DSpectra = hlab2D + "InvMass" + mTitle;
  const char* h3DSpectra = hlab3DSpectra.c_str(); 
  m3DInvMass = new TH3D(h3DSpectra,"Invariant Mass 3-D spectra",
			  mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
			  mnbinOrdinate,mlbinOrdinate,mubinOrdinate,
			  mnMassBins,mlMassBin,muMassBin);
  m3DInvMass->Sumw2();

  string hlab3DWeighted = hlab2D + "InvMassWeighted" + mTitle;
  const char* h3DWeighted = hlab3DWeighted.c_str(); 
  m3DInvMassWeighted = new TH3D(h3DWeighted,"Invariant Mass 3-D Weighted spectra",
				mnbinAbscissa, mlbinAbscissa,mubinAbscissa,
				mnbinOrdinate,mlbinOrdinate,mubinOrdinate,
				mnMassBins,mlMassBin,muMassBin);
  m3DInvMassWeighted->Sumw2();  
}

void StV0SpectraAnalysis::fillHistograms(StEvent& event) 
{
  StSPtrVecV0Vertex& v0Vertices = event.v0Vertices();
  StSPtrVecV0VertexIterator iterV0;     
  StV0Vertex *v0Vertex;

  StThreeVectorF v0Mom;
  StThreeVectorF posMom;
  StThreeVectorF negMom;

  double pdotn, posEnergy, negEnergy, effic, weight;    
  double massInv, E, pz, pt, mt, y, pseudoy;

  double mMassPid = mParticle->mass();

  double xvalue=-1000.;
  double yvalue=0.;

  StVertex* primvtx = event.primaryVertex();
  if (primvtx==0) return;
  mNumEvent++ ; 
 
  //Loop over all V0 vertices
  for (iterV0 = v0Vertices.begin(); iterV0 != v0Vertices.end(); iterV0++) {

    v0Vertex = *iterV0;
    if (!v0Vertex) continue;

    // 
    // check to see if v0 vertex satisfies the quality cuts set up
    // for this analysis
    //
    vector<StSpectraCut*>::const_iterator cutIter;
    bool satisfiesAllCuts = true ;
    
    for (cutIter = mEffic.mSpectraCutContainer.begin();
	 cutIter != mEffic.mSpectraCutContainer.end();
	 cutIter++) {
      if ((typeid(**cutIter) == typeid(StSpectraCutDcaParent)) ||
	  (typeid(**cutIter) == typeid(StSpectraCutDcaDaughters))) {
	if (!((*cutIter)->satisfiesCut(v0Vertex,&event)) && satisfiesAllCuts)
	  satisfiesAllCuts = false;
      }
      else {
	if (!((*cutIter)->satisfiesCut(v0Vertex->daughter(positive),&event)) && satisfiesAllCuts)
	  satisfiesAllCuts = false;
	if (!((*cutIter)->satisfiesCut(v0Vertex->daughter(negative),&event)) && satisfiesAllCuts)
	  satisfiesAllCuts = false;
      }
    }

    if (satisfiesAllCuts) {
      
      // Calculate the V0 mass
      posMom = v0Vertex->momentumOfDaughter(positive);
      negMom = v0Vertex->momentumOfDaughter(negative);
      
      posEnergy = sqrt(posMom.mag2() + mDaughterPos->mass()*mDaughterPos->mass());
      negEnergy = sqrt(negMom.mag2() + mDaughterNeg->mass()*mDaughterNeg->mass());
      
      pdotn = ( posMom.x()*negMom.x() +
		posMom.y()*negMom.y() +
		posMom.z()*negMom.z());
      
      massInv = sqrt( mDaughterPos->mass()*mDaughterPos->mass() + 
		      mDaughterNeg->mass()*mDaughterNeg->mass() +
		      2*(posEnergy*negEnergy - pdotn) );
      
      //Calculate pt, mt, eta and y
      v0Mom  = v0Vertex->momentum();            
      pt  = v0Mom.perp();
      mt = sqrt( v0Mom.perp2() + mMassPid*mMassPid );
      E  = sqrt( v0Mom.mag2() + mMassPid*mMassPid );
      pz = v0Mom.z();
      y  = 0.5*log((E+pz)/(E-pz)); 
      pseudoy = 0.5*log((v0Mom.mag()+pz)/(v0Mom.mag()-pz));
      if (mAbscissa == kRapidity) {
	xvalue = y;
      } else if (mAbscissa == kPseudoRapidity) {
	xvalue = pseudoy;
      } 
      if (mOrdinate == kPperp) {
	yvalue = pt;
      } else if (mOrdinate == kTransverseMass) {
	yvalue = mt;
      } 
      
      //Fill non-corrected yield histogram
      m3DInvMass->Fill(xvalue,yvalue,massInv);

      //Calculate Efficiency
      effic = mEffic.efficiency(v0Vertex);
      //  cout << effic << endl;

      //Fill efficiency corrected yield histogram
      if (effic > 0. && effic < 1.) {
	weight = 1./effic;
	m3DInvMassWeighted->Fill(xvalue,yvalue,massInv,weight);	
      }
    }
  }               
}

void StV0SpectraAnalysis::projectHistograms() {

  if (mNumEvent==0) return;
  float xnorm = 1./float(mNumEvent);
  m3DInvMassWeighted->Scale(xnorm);

  Stat_t stats[8];
  m3DInvMassWeighted->GetStats(stats);
  cout << "sum of weights " << stats[0] << endl;
}

void StV0SpectraAnalysis::writeHistograms(){

  const char* outputName = ((*this).getTitle()+".root").c_str();
  TFile* analysisOutputFile = new TFile(outputName,"RECREATE");
  m3DInvMass->Write();
  m3DInvMassWeighted->Write();
  delete analysisOutputFile;
}








