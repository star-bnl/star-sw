/***************************************************************
 * $Id: StRichDrawableTMip.cxx,v 2.0 2000/08/09 16:28:03 gans Exp $
 *
 * Description:
 *
 ***************************************************************
 * $Log: StRichDrawableTMip.cxx,v $
 * Revision 2.0  2000/08/09 16:28:03  gans
 * Created New Maker for all drawable objects.
 *
 * Revision 2.0  2000/08/09 16:28:03  gans
 * Created New Maker for all drawable objects.
 *
 * Revision 1.1  2000/06/16 02:37:11  horsley
 * many additions, added features to pad plane display (MIPS, rings, etc)
 * along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 * Revision 1.0  2000/05/25 21:35:32  gans
 * Intitial
 *
 ***************************************************************/
#include <iostream.h>
#include <fstream.h>
#ifdef __ROOT__
#include "StRichDrawableTMip.h"
#include "StRichPIDMaker/StRichTrack.h"
#include "StRichDrawableTTrack.h"
#include "StParticleDefinition.hh"
#include "StParticleTypes.hh"
#include "StRichPIDMaker/StRichMCTrack.h"
#include "StMcEventTypes.hh"
#include "SystemOfUnits.h"

ClassImp(StRichDrawableTMip)
StRichDrawableTMip::StRichDrawableTMip(StRichDrawableTTrack * drawableTrackP)
   : TMarker(){
StRichDrawableTMip::StRichDrawableTMip() {/*nopt*/}
    mTLineMIPPointer = 0;
StRichDrawableTMip::StRichDrawableTMip(StRichDrawableTTrack * drawableTrackP) : TMarker(){

    mMomText = 0;
    
    mTrackPointer = drawableTrackP;

    geantTrackP = 0;
    mMomText    = 0;
    mTrackPointer  = drawableTrackP;
    double Degrees = 180.0/M_PI;
    
    StRichTrack * trackP = drawableTrackP->getTrack();
    if(trackP){
	this->SetMarkerColor(6);
	this->SetMarkerSize(3);
	this->SetMarkerStyle(27); // diamond
	
	this->SetX( trackP->getProjectedMIP().x() );
	this->SetY( trackP->getProjectedMIP().y() );
	mMomText = new TText(this->GetX()+.3,this->GetY()+.1,tempChar);
	char tempChar[100];
	sprintf(tempChar,"%.3f",trackP->getMomentum().mag());
	mMomText = new TText(this->GetX()+.35,this->GetY()+.15,tempChar);
	mMomText->SetTextSize(.018);
	mMomText->Draw();
	
	mXImpactRadiator = trackP->getImpactPoint().x();
	mYImpactRadiator = trackP->getImpactPoint().y();
	
	mThreeMomMag = trackP->getMomentum().mag();
	mEta = trackP->getMomentum().pseudoRapidity();
	mPhi = trackP->getPhi()*Degrees;
    }

    mGeantPhi = 0;
    mGeantThreeMomMag = 0;
    mGeantYImpactRadiator= 0;
    mGeantPhi   = 0;
    mGeantXImpactRadiator = 0;
    mGeantStopVertexProcess = 0;
    mGeantXStopVertex = 0;
    mGeantCommonTpcHits  = 0;
    mGeantStopVertexProcess   = 0;
    mGeantHitsInRadiator = 0;
    mGeantCommonTpcHits    = 0;
    mGeantTrackID = 0;
    mGeantX = -999;
    mGeantY = -999;
    mGeantTrackID   = 0;
    mGeantX        = -999;
    mGeantY        = -999;
    mGeantResidual = -999;
        
    mFastEnoughPion = trackP->fastEnough(StPionPlus::instance()) ;
    mFastEnoughKaon = trackP->fastEnough(StKaonPlus::instance()) ;
    mFastEnoughProton = trackP->fastEnough(StProton::instance()) ;

    geantTrackP = dynamic_cast<StRichMCTrack*>(trackP);

	
    if(geantTrackP){	
	    
	mGeantX = geantTrackP->getGeantMIP().x();
				  + pow(mGeantY - trackP->getProjectedMIP().y(),2));
	    
	if(mGeantX > -998 && mGeantY > -998){
	    mGeantResidual =sqrt( pow(mGeantX - trackP->getProjectedMIP().x(),2)
				+ pow(mGeantY - trackP->getProjectedMIP().y(),2));
	}
	mGeantThreeMomMag = geantTrackP->getGeantMomentumAtRadiator().mag();
	mGeantTheta = geantTrackP->getGeantThetaAtRadiator()*Degrees;
	mGeantPhi = geantTrackP->getGeantPhiAtRadiator()*Degrees;
	
	mGeantXStopVertex = geantTrackP->getGeantStopVertex().x();
	mGeantYStopVertex = geantTrackP->getGeantStopVertex().y();
	mGeantThreeMomMag     = geantTrackP->getGeantMomentumAtRadiator().mag();
	mGeantTheta           = geantTrackP->getGeantThetaAtRadiator()*Degrees;
	    mGeantStopVertexProcess = geantTrackP->getStMcTrack()->stopVertex()->geantProcess();
	
	    mGeantStopVertexNumDaught = geantTrackP->getStMcTrack()->stopVertex()->numberOfDaughters();
	    mGeantXStopVertex         = geantTrackP->getStMcTrack()->stopVertex()->position().x();
	mGeantCommonTpcHits = geantTrackP->getCommonTpcHits();
	}
	mGeantHitsInRadiator = geantTrackP->getNumberOfGeantHitsInRadiator();
	mGeantHitsInGap = geantTrackP->getNumberOfGeantHitsInGap();
	mGeantNumberOfPartners = geantTrackP->getNumberOfPartners();
	mGeantHitsInRadiator   = geantTrackP->getNumberOfGeantHitsInRadiator();
	mGeantHitsInGap        = geantTrackP->getNumberOfGeantHitsInGap();
	if(geantTrackP->getStMcTrack()){
	  mGeantTrackID = geantTrackP->getStMcTrack()->geantId();
	}

	if(mGeantTrackID == 8 || mGeantTrackID == 9)   // if Pion
	    this->SetMarkerColor(2);                   // red
	else if(mGeantTrackID == 11 || mGeantTrackID == 12) // if Kaon
	    this->SetMarkerColor(1);                        // black
	else if(mGeantTrackID == 14 || mGeantTrackID == 15) //if proton
	    this->SetMarkerColor(3);                        // green
	
    }
}


StRichDrawableTMip::~StRichDrawableTMip() {
    delete mTLineMIPPointer;
    delete mTMarkerMIPPointer;
    delete mMomText;
}

StRichDrawableTTrack* StRichDrawableTMip::getDrawableTTrack() { return mTrackPointer;}

void StRichDrawableTMip::clearRings(){
    for(int i = 0; i < mTrackPointer->numberOfRings();i++)
	mTrackPointer->getRing(i)->clear();
}

// Usually will not return error message as ring does exist
// just has no lines. Will Fix this so returns error
void StRichDrawableTMip::drawPionRing(){
    if(mTrackPointer->getRing(StPionPlus::instance()))
	mTrackPointer->getRing(StPionPlus::instance())->draw();
    else if(mTrackPointer->getRing(StPionMinus::instance()))
	mTrackPointer->getRing(StPionMinus::instance())->draw();
    else
	cerr << " No Pion Ring -- Probably Not Above Threshold\n";
}
void StRichDrawableTMip::drawKaonRing()
{
    if(mTrackPointer->getRing(StKaonPlus::instance()))
	mTrackPointer->getRing(StKaonPlus::instance())->draw();
    else if(mTrackPointer->getRing(StKaonMinus::instance()))
	mTrackPointer->getRing(StKaonMinus::instance())->draw();
    else
	cerr << " No Kaon Ring -- Probably Not Above Threshold\n";
}
void StRichDrawableTMip::drawProtonRing(){
    if(mTrackPointer->getRing(StProton::instance()))
	mTrackPointer->getRing(StProton::instance())->draw();
    else if(mTrackPointer->getRing(StAntiProton::instance()))
	mTrackPointer->getRing(StAntiProton::instance())->draw();
    else
	cerr << " No Proton Ring -- Probably Not Above Threshold\n";
}
    
void StRichDrawableTMip::drawAllRings(){
    this->drawPionRing();
    this->drawProtonRing();
}


void StRichDrawableTMip::findRealMip(){
    if(mGeantX > -998 && mGeantY > -998){ // don't want to do exact compare on double
	
	mTLineMIPPointer = new TLine(this->GetX(),this->GetY(),mGeantX,mGeantY);
	mTLineMIPPointer->Draw();
	
	mTMarkerMIPPointer = new TMarker(this->GetX(),this->GetY(),30);
	mTMarkerMIPPointer->SetMarkerSize(3.2);
	mTMarkerMIPPointer->SetMarkerColor(this->GetMarkerColor());
	mTMarkerMIPPointer->Draw();
    }
    else
	cout << "No Geant Mip";
}
void StRichDrawableTMip::clearRealMip(){
    delete mTLineMIPPointer;
    delete mTMarkerMIPPointer;
    delete mMomText;
}

void StRichDrawableTMip::clearMomText(){
    delete mMomText;
}

#endif /* ROOT */
