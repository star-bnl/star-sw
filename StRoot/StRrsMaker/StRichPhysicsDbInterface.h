/***********************************************
 * $Id: StRichPhysicsDbInterface.h,v 1.1 2000/01/25 22:02:22 lasiuk Exp $
 *
 * Description:
 *
 * $Log: StRichPhysicsDbInterface.h,v $
 * Revision 1.1  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/25 22:02:22  lasiuk
 * Second Revision
 *
 ***********************************************************/
#ifndef ST_RICH_PHYSICS_INTERFACE_H
#define ST_RICH_PHYSICS_INTERFACE_H

#include <iostream.h>
#include "StGlobals.hh"

class StRichPhysicsDbInterface {
public:
    
    virtual ~StRichPhysicsDbInterface() {}
    //StRichPhysicsInterface(const StRichPhysicsInterface&);
    //StRichPhysicsInterface&(const StRichPhysicsInterface&);

    virtual double version() const = 0;
    virtual double polia() const = 0;
    virtual double averageNumberOfInteractions() const = 0;

    virtual double photonToPadEfficiency() const = 0;
    virtual double photoConversionEfficiency() const = 0;

    virtual double feedBackPhotonProbability() const = 0;
    virtual double electronicNoiseLevel() const = 0;    
    virtual double electronCharge() const = 0;    
    
    virtual double electronDistribution(int) const = 0;    

    virtual double maximumElectronEnergy() const = 0;
    virtual int averagePedestal() const = 0;
    virtual double adcConversion() const = 0;
	
    virtual int adcThreshold() const = 0;
    virtual int adcChannelWidth() const = 0;

    virtual void   print(ostream& os = cout) const = 0;
};
#endif
