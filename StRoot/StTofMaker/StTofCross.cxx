/*******************************************************************
 * $Id: StTofCross.cxx,v 1.1 2001/04/24 20:27:08 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 *****************************************************************
 * Description:
 * Public functions to check if a track crosses TOF
 *
 *****************************************************************
 * 
 * $Log: StTofCross.cxx,v $
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *              
 *****************************************************************/

#include "StTofCross.h"
#include "PhysicalConstants.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StTofCross.h"

StTofCross::StTofCross() {}

StTofCross::~StTofCross() {}

Bool_t StTofCross::tofSlatCross(StThreeVectorD& point,
                                           StructTofSlat* tofSlat) const
{

// this memeber function checks if a point is in a slat

    bool  slatCross;
    float phi, etaMin, etaMax;

// rearrange database of STAF which does not meet StRoot standard

/*
   phi in StThreeVectorD is between -pi to +pi (radius)
   phi in TOF database is between 0 to 360 (degree)
   the constant degree = pi/180 = 0.01745
*/

    phi = point.phi();
    if(phi < 0) phi = (phi + twopi)/degree;
    else phi = phi/degree;

// swap eta min and max in STAF database for slats with eta > 0.     

    if(tofSlat->slatEta.eta < 0) { 
       etaMin = tofSlat->slatEta.eta_min;
       etaMax = tofSlat->slatEta.eta_max;
    }
    else { 
       etaMin = tofSlat->slatEta.eta_max;
       etaMax = tofSlat->slatEta.eta_min;
    }

// start to check
    slatCross = false;
    if((float) point.pseudoRapidity() >= etaMin &&
       (float) point.pseudoRapidity() <= etaMax) { 

// the 3rd phi of 5W slat is a special (phi_max = 0.5, phi_min = 359.5 degree) 
        if((tofSlat->slatEta.ieta == 10 || tofSlat->slatEta.ieta == 11) &&
                                               tofSlat->slatPhi.iphi == 3) {
           if ((phi <= tofSlat->slatPhi.phi_max && phi >= 0.0) ||
               (phi >= tofSlat->slatPhi.phi_min && phi < twopi))
                slatCross = true;
        }  

// all others
        else  {
           if (phi >= tofSlat->slatPhi.phi_min  &&
               phi <= tofSlat->slatPhi.phi_max)   
               slatCross = true;
        }
    }
    return slatCross;
}


Int_t StTofCross::tofSlatCrossId(Int_t volumeId,
                                           StTofDatabase* tofDb) const
{
  // decode the volumeId and return a constructed slatId
  
  int phiId=-1;
  int etaId=-1;

  int trayEta    = int(volumeId/100000) ;
  int trayPhi    = (short)fmod((double)volumeId,100000.)/1000 ;
  int counterPhi = (short)fmod((double)volumeId,1000.)/100 ;
  int counterEta = (short)fmod((double)volumeId,100.) ;

  if ( trayEta == 1 ) {
    phiId = 14 - trayPhi ;
    if ( phiId < 1 ) phiId = phiId + 60 ;
    if (counterEta==1)
      phiId = phiId * 5 - counterPhi + 1;   // 5w row
    else
      phiId = phiId * 4 - counterPhi + 1;   // 4w rows
    etaId = counterEta + 10 ;
  }
  else if ( trayEta == 2 ) {
    phiId = trayPhi - 42 ;
    if ( phiId < 1 ) phiId = phiId + 60 ;
    if (counterEta==1)
      phiId = phiId * 5 + counterPhi - 5;   // 5w row
    else
      phiId = phiId * 4 + counterPhi - 4;   // 4w rows
    etaId = 11 - counterEta ;
  }
  else
    cout<<" StTofCross::tofSlatCrossId  unknown trayid  "<<trayEta<<endl ;
  
  int nEtas = tofDb->tofSlatEtaVec().size();
  int slatId = (phiId - 1) * nEtas + etaId;

  return slatId;
}



Int_t StTofCross::tofSlatCrossId(StThreeVectorD& point,
                                           StTofDatabase* tofDb) const
{

// return the index of a slat if the point is in the slat 

    static const int nPhiRows5w=300, nPhiRows4w=240, offsetMult4w = 540;
    int nEtas, slatId, etaId, phiId;
    float phi, etaMin, etaMax;
    etaId = -1;
    phiId = -1;

// rearrange database of STAF which does not meet StRoot standard

/*
   phi in StThreeVectorD is between -pi to +pi (radius)
   phi in TOF database is between 0 to 360 (degree)
   the constant degree = pi/180 = 0.01745
*/
    phi = point.phi();
    if(phi < 0) phi = (phi + twopi)/degree;
    else phi = phi/degree;

    nEtas = tofDb->tofSlatEtaVec().size();

    for(int i=0; i<nEtas; i++) {

// swap eta min and max in STAF database for slats with eta > 0.     

       if(tofDb->tofSlatEtaVec()[i].eta < 0) { 
          etaMin = tofDb->tofSlatEtaVec()[i].eta_min;
          etaMax = tofDb->tofSlatEtaVec()[i].eta_max;
       }
       else { 
          etaMin = tofDb->tofSlatEtaVec()[i].eta_max;
          etaMax = tofDb->tofSlatEtaVec()[i].eta_min;
       }

// get eta index if any
       if((float) point.pseudoRapidity() >= etaMin &&
          (float) point.pseudoRapidity() <= etaMax)  {
           etaId = tofDb->tofSlatEtaVec()[i].ieta;
           break;
       }
    }
    
// determine the boundaries of a loop over phi for 5W and 4W slats
    int iPhiBegin, iPhiEnd;
    if(etaId == 10 || etaId == 11) {             // 5W rows
         iPhiBegin = 0;
         iPhiEnd = nPhiRows5w;
    }
    else if (etaId == 9 || etaId == 12) {        // 4W[2] rows
         iPhiBegin = nPhiRows5w;
         iPhiEnd = offsetMult4w;
    }
    else {                                         // 4W[3-10] rows
         iPhiBegin =offsetMult4w;
         iPhiEnd=offsetMult4w + nPhiRows4w;
    }

// get phi index if any
    for(int i = iPhiBegin; i <iPhiEnd; i++) {

// the 3rd phi of 5W slat is a special (phi_max = 0.5, phi_min = 359.5 degree) 
 
       if((etaId == 10 || etaId == 11) && i == 2) {
           if ((phi <= tofDb->tofSlatPhiVec()[i].phi_max) ||
              (phi >= tofDb->tofSlatPhiVec()[i].phi_min)) {
              phiId = tofDb->tofSlatPhiVec()[i].iphi; 
              break;
           }
       }  

// all others
       else  {
           if (phi >= tofDb->tofSlatPhiVec()[i].phi_min &&
              phi <= tofDb->tofSlatPhiVec()[i].phi_max) {  
              phiId = tofDb->tofSlatPhiVec()[i].iphi; 
              break;
           }
       }
    }
/*    
   calculate slat index (negative index indicates that the point is out 
   of all slats)
*/ 
    if(etaId > 0 && phiId > 0) slatId = (phiId - 1) * nEtas + etaId;
    else slatId = -1;

    return slatId;
}

idVector StTofCross::tofClusterFinder(StThreeVectorD& pointAtCyl,
                                                StTofDatabase* tofDb) const
{
/*
  first, this function finds a slat closest to a point pointAtCyl (at 
  TOF cylinder) which a helix crosses, then determines a cluster of slats 
  (up to 9) which surround the founded center slat. 
*/
    
    static const int nPhiRows5w=300, nPhiRows4w=240, offsetMult4w = 540;
    int iEtaCenter, iPhiCenter, centerSlatId, slatId, validPhiId;
    float phi, deltaPhi, minDeltaPhi;
    float eta, deltaEta, minDeltaEta=2.0;
    int nEtas = tofDb->tofSlatEtaVec().size();

    idVector  slatIdVec;
    slatIdVec.clear();

// find slats with  eta closest to the eta of the point at TOF cylinder 
    eta = pointAtCyl.pseudoRapidity(); 
    for(int i=0; i<nEtas;i++) {
       deltaEta = fabs(tofDb->tofSlatEtaVec()[i].eta -eta);
       if(deltaEta <= minDeltaEta) {
          minDeltaEta = deltaEta;
          iEtaCenter = tofDb->tofSlatEtaVec()[i].ieta;
       }
    } 

// find a slat with its phi closest to the phi of the point at TOF cylinder 
    int iPhiBegin, iPhiEnd;
    if(iEtaCenter == 10 || iEtaCenter == 11) {             // 5W rows
         iPhiBegin = 0; 
         iPhiEnd = nPhiRows5w;
    }
    else if (iEtaCenter == 9 || iEtaCenter == 12) {        // 4W[2] rows
         iPhiBegin = nPhiRows5w; 
         iPhiEnd = offsetMult4w;
    }
    else {                                                // 4W[3-10] rows
         iPhiBegin =offsetMult4w; 
         iPhiEnd=offsetMult4w + nPhiRows4w;
    }

    phi = pointAtCyl.phi();
    if (phi < 0.0) phi = (phi + twopi)/degree;
    else phi = phi/degree;

// general cases 4W[3-9]
    if((iEtaCenter>1 && iEtaCenter<9) || (iEtaCenter>12 && iEtaCenter<nEtas)) {
       minDeltaPhi = pi;
       for(int iPhi=iPhiBegin; iPhi<iPhiEnd;iPhi++) {
         deltaPhi = fabs(phi - tofDb->tofSlatPhiVec()[iPhi].phi); 
         if (deltaPhi < minDeltaPhi) {
           minDeltaPhi = deltaPhi;
           iPhiCenter = tofDb->tofSlatPhiVec()[iPhi].iphi; 
         }
       }

       for (int k = iPhiCenter - 1; k < iPhiCenter + 2; k++) { 
          if(k == 0) validPhiId = k + nPhiRows4w;
          else if (k > nPhiRows4w) validPhiId = k - nPhiRows4w;
          else validPhiId = k;
          centerSlatId = (validPhiId - 1)*nEtas + iEtaCenter;
          for (slatId = centerSlatId - 1; slatId<centerSlatId+2; slatId++) 
                                              slatIdVec.push_back(slatId);
       }
    }

// one more loop over phis for other cases: 4W[3,10] and 5W
    else {
       int iEtaBegin, iEtaEnd;
       if(iEtaCenter == 1) iEtaBegin = 1;
       else iEtaBegin = iEtaCenter - 1;
       if(iEtaCenter == nEtas) iEtaEnd = nEtas;
       else iEtaEnd = iEtaCenter + 1;
/*
       cout << "iEtaCenter... = " << iEtaCenter << " " << iEtaBegin << " "
                                                      << iEtaEnd << endl;
*/
       for (int iEta = iEtaBegin; iEta< iEtaEnd + 1; iEta++) {
          if(iEta == 10 || iEta == 11) {             // 5W rows
            iPhiBegin = 0; 
            iPhiEnd = nPhiRows5w;
          }
          else if (iEta == 9 || iEta == 12) {        // 4W[2] rows
            iPhiBegin = nPhiRows5w; 
            iPhiEnd = offsetMult4w;
          }
          else {                                     // 4W[3-10] rows
            iPhiBegin =offsetMult4w; 
            iPhiEnd=offsetMult4w + nPhiRows4w;
          }

          minDeltaPhi = pi;
          for(int iPhi=iPhiBegin; iPhi<iPhiEnd;iPhi++) {
             if (phi < 0.0) phi = phi+360.0;
             deltaPhi = fabs(phi - tofDb->tofSlatPhiVec()[iPhi].phi); 
             if (deltaPhi < minDeltaPhi) {
                minDeltaPhi = deltaPhi;
                iPhiCenter = tofDb->tofSlatPhiVec()[iPhi].iphi; 
             }
          }

          if(iPhiBegin != 0 ) {                                   // 4W[2-10]
            for (int k = iPhiCenter - 1; k < iPhiCenter + 2; k++) { 
               if(k == 0) validPhiId = k + nPhiRows4w;
               else if (k > nPhiRows4w) validPhiId = k - nPhiRows4w;
               else validPhiId = k;
               slatId = (validPhiId - 1)*nEtas + iEta;
               slatIdVec.push_back(slatId);
            }
          }
          else {                                                   // 5W
            for (int k = iPhiCenter - 1; k < iPhiCenter + 2; k++) { 
               if(k == 0) validPhiId = k + nPhiRows5w;
               else if (k > nPhiRows5w) validPhiId = k - nPhiRows5w;
               else validPhiId = k;
               int slatId = (validPhiId - 1)*nEtas + iEta;
               slatIdVec.push_back(slatId);
            }
          }
       }
    }

    return slatIdVec;
}
   
tofSlatHitVector StTofCross::tofHelixToSlat(TH1F* hist, 
                                            StPhysicalHelixD& helix, 
                                            Double_t pathLengthToCyl, 
                                            StTofDatabase* tofDb)     
/*
tofSlatHitVector StTofCross::tofHelixToSlat(StPhysicalHelixD helix, 
                                           Double_t pathLengthToCyl, 
                                           StTofDatabase* tofDb)     
*/
{
// this member function finds slats crossed by a track-helix. 

// find a TOF-cylinder point. pathLenthToCyl was determined in StTofMaker.cxx.  
    StThreeVectorD hitAtCyl = helix.at(pathLengthToCyl);

// find a cluster of slats  
    idVector slatIdVec=tofClusterFinder(hitAtCyl, tofDb);

    idVector     idErasedVec = slatIdVec;
    idVectorIter slatIdIter, idErasedIter;

    bool   middle, inner, outer;
    double pathLength;

    float slatThickness = tofDb->tofParam().counter_thickness;

    StructSlatHit slatHit;
    tofSlatHitVector slatHitVec;
    slatHitVec.clear();

// determine which stats in cluster are crossed by the track-helix.
    while (slatIdVec.size() != 0) {

// the first slat in the cluster
        slatIdIter = slatIdVec.begin();

        int trayId = tofDb->tofSlat(*slatIdIter)->trayId;
        float cosang = tofDb->tofSlat(*slatIdIter)->slatEta.cosang;
        int iEta = tofDb->tofSlat(*slatIdIter)->slatEta.ieta;

// middle of the slat
        StThreeVectorD slatNormMiddle = tofDb->tofPlaneNormPoint(*slatIdIter);

// get normal vector of the plane
        StThreeVectorD slatNormal = slatNormMiddle/slatNormMiddle.mag();

// two edges (inner and outer) of the slat
        StThreeVectorD slatNormInner = slatNormMiddle*((slatNormMiddle.mag() -
                                       slatThickness)/ slatNormMiddle.mag());
        StThreeVectorD slatNormOuter = slatNormMiddle*((slatNormMiddle.mag() +
                                        slatThickness)/ slatNormMiddle.mag());
// points at three planes
        pathLength = helix.pathLength(slatNormMiddle, slatNormal);
        StThreeVectorD hitAtMiddle = helix.at(pathLength);
        pathLength = helix.pathLength(slatNormInner, slatNormal);
        StThreeVectorD hitAtInner = helix.at(pathLength);
        pathLength = helix.pathLength(slatNormOuter, slatNormal);
        StThreeVectorD hitAtOuter = helix.at(pathLength);

// loop over all slats in idErasedVec (=slatIdVec at the begnining of while)
        idErasedIter = idErasedVec.begin();
        while (idErasedIter != idErasedVec.end()) {

// check if any slat in the plane where the first slat lies
            if(tofDb->tofSlat(*idErasedIter)->slatEta.cosang == cosang &&
                   tofDb->tofSlat(*idErasedIter)->slatEta.ieta == iEta &&
                   tofDb->tofSlat(*idErasedIter)->trayId == trayId) {

// check if any slat in the plane is fired
                middle=tofSlatCross(hitAtMiddle, tofDb->tofSlat(*idErasedIter));
                inner=tofSlatCross(hitAtInner, tofDb->tofSlat(*idErasedIter));
                outer=tofSlatCross(hitAtOuter, tofDb->tofSlat(*idErasedIter));
 
// fill histogram of hit pattern 

                if(middle && inner && outer) hist->Fill(1.0); 
                if(!middle && inner && outer) hist->Fill(2.0); 
                if(middle && !inner && outer) hist->Fill(3.0); 
                if(middle && inner && !outer) hist->Fill(4.0); 
                if(!middle && !inner && outer) hist->Fill(5.0); 
                if(!middle && inner && !outer) hist->Fill(6.0); 
                if(middle && !inner && !outer) hist->Fill(7.0); 
 
// save fired slat in slatHitVec which will be returned at the end  
                if(middle||inner||outer) {
                     slatHit.slatIndex = *idErasedIter;
                     if(middle) slatHit.hitPosition = hitAtMiddle; 
                     else if(inner) slatHit.hitPosition = hitAtInner; 
                     else slatHit.hitPosition = hitAtOuter; 
                     slatHitVec.push_back(slatHit);
                }
/*
                cout << "fired? Ids, and angles = " 
                     << middle << " " << inner <<" "<< outer <<
//                 " " << slatNormal.x() <<
//                 " " << slatNormal.y() <<
//                 " " << slatNormal.z() <<
                 " " << tofDb->tofSlat(*idErasedIter)->slatEta.ieta <<
                 " " << tofDb->tofSlat(*idErasedIter)->slatPhi.iphi <<
                 " " << tofDb->tofSlat(*idErasedIter)->slatEta.eta <<
                 " " << tofDb->tofSlat(*idErasedIter)->slatPhi.phi << endl;
*/
//erase the slat entry which has been checked (preparing for the next round)
                idErasedVec.erase(idErasedIter);
                idErasedIter--;
            }
            idErasedIter++;
        }
        slatIdVec = idErasedVec;
    }
    return slatHitVec;
}
