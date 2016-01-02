#ifndef StElectronPair_hh
#define StElectronPair_hh
#ifdef __ROOT__

/* **************************************************
 *  A specialized pair class for calculating electron pair
 *  lorentz vector and topological decay parameters
 *  and storing them.
 *
 *  Authors:  **Kunsu OH        (kunsuoh@gmail.com)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 * **************************************************
 */

#include "TObject.h"

class StPicoTrack;
class StPicoEvent;

class StElectronPair : public TObject
{
public:
    StElectronPair();
    StElectronPair(StElectronPair const *);
    StElectronPair(StPicoTrack const * Electron, StPicoTrack const * Partner,
                   unsigned short electronIdx,unsigned short partnerIdx, float bField);
    ~StElectronPair() {}// please keep this non-virtual and NEVER inherit from this class
    
    unsigned short   electronIdx() const;	// tagged electron idx
    unsigned short   partnerIdx() const;	// partner electron idx
    float pairMass()    const;                     // electron pair mass
    float pairDca() const;                  // DCA between tagged and partner
    float positionX() const;                // conversion position X
    float positionY() const;                // conversion position Y
    float positionZ() const;                // conversion position Z

    
    
private:
    // disable copy constructor and assignment operator by making them private (once C++11 is available in STAR you can use delete specifier instead)
    StElectronPair(StElectronPair const &);
    StElectronPair& operator=(StElectronPair const &);
    
    unsigned short mElectronIdx;    // index of electron track in StPicoDstEvent (2 Bytes)
    unsigned short mPartnerIdx;     // index of partner track in StPicoDstEvent (2 Bytes)
    unsigned short mMass;           // mass * 1000 (2 Bytes)
    float mPairDca;                 // pair dca (4 Bytes)
    float mPositionX;               // conversion position x (4 Bytes)
    float mPositionY;               // conversion position y (4 Bytes)
    float mPositionZ;               // conversion position z (4 Bytes)
    
    ClassDef(StElectronPair,2)
};
inline unsigned short   StElectronPair::electronIdx() const     { return mElectronIdx;                        }
inline unsigned short   StElectronPair::partnerIdx() const      { return mPartnerIdx;                         }
inline float StElectronPair::pairMass()    const                { return static_cast<float>(mMass/1000.);     }
inline float StElectronPair::pairDca() const                    { return mPairDca;                            }
inline float StElectronPair::positionX() const                  { return mPositionX;}
inline float StElectronPair::positionY() const                  { return mPositionY;}
inline float StElectronPair::positionZ() const                  { return mPositionZ;}

#endif
#endif

