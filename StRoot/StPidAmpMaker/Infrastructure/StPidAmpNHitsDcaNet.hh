/***************************************************************************
 *
 * $Id: StPidAmpNHitsDcaNet.hh,v 1.1 2000/04/09 16:24:51 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpNHitsDcaNet is a basic unit for building a
 *             NHitsDca instance of StPidAmpChannelCollection
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpNHitsDcaNet.hh,v $
 * Revision 1.1  2000/04/09 16:24:51  aihong
 * adding into package
 *
 *
 **************************************************************************/


#ifndef StPidAmpNHitsDcaNet_hh
#define StPidAmpNHitsDcaNet_hh

#include "StPidAmpMaker/Infrastructure/StPidAmpNet.hh"

class StPidAmpNHitsDcaNet : public StPidAmpNet {

public:

  StPidAmpNHitsDcaNet();
  StPidAmpNHitsDcaNet(StPidAmpParticle def, StPidAmpChannelInfo channelInfo);

   void fitBand(TH3D* histo);
   void fitAPath(StPidAmpPath& path, StPidAmpTrkVector* trks,TH3D* histo);
   void fitAmp(StPidAmpTrkVector* trks,TH3D* histo);
   void fitReso();
   void fillPathFittedSlices();
   ostream& put(ostream& s) const;// for calling the right put() by operator.
private:

   double ( *funcAmpPt) (double *, double *);
   double ( *funcResoPt) (double *, double *);



};



#endif
