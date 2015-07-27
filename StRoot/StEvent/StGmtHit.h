/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHit
 *
 ***************************************************************************
 *
 * Description: data for individual ``hit'' on the GMT, i.e. a 1D cluster.
 *
 ***************************************************************************/

#ifndef _ST_GMT_HIT_H_
#define _ST_GMT_HIT_H_

#include <map>

#include "StHit.h"
#include "StGmtStrip.h"

// Note: not const StGmtStrip, so the clustering can modify the
// strips.  Clustering algos require these to be ordered by geoId.
// Note also, the gmtStripWeightMap_t is only used in memory, and is not
// streamed.
typedef map< StGmtStrip*, float, gmtStripPtrLessThan > gmtStripWeightMap_t;

class StGmtHit : public StHit {
public:
    // constructors
    StGmtHit( int key = -1, short module = -1, int adcX = 0, int adcY = 0,
	      int tbX = -1, int tbY = -1, double localX = 0, double localY = 0,
	      double localXErr = 10000, double localYErr = 10000 );
    // StGmtHit(const StGmtHit&);             --> use default
    // StGmtHit& operator=(const StGmtHit&);  --> use default
    
    // deconstructor
    ~StGmtHit();
virtual StDetectorId detector() const 		{return kGmtId;};   
    // accessors/modifiers for the map

    gmtStripWeightMap_t& getStripWeightMap();
    const gmtStripWeightMap_t& getStripWeightMap() const;
    
    // modifer
    void setHardwareId( short module );
    
    // other accessors
    int getKey() const;
    int getModule() const;
    int getAdcX() const;
    int getAdcY() const;
    int getTbX() const;
    int getTbY() const;
    double getLocalX() const;
    double getErrorLocalX() const;
    double getLocalY() const;
    double getErrorLocalY() const;
    // note: no getCharge, as already have charge() defined through parent StHit
    double getChargeUncert() const;
//     short getMaxAdc() const;
    
    // modifiers
    void setAdcX( int adc );
    void setAdcY( int adc );
    void setTbX( int tb );
    void setTbY( int tb );
    void setLocalX( double position );
    void setErrorLocalX( double error );
    void setLocalY( double position );
    void setErrorLocalY( double error );
    void setModule( short module );
    void setChargeUncert( double sigma );
    void     Print(Option_t *option="") const;
protected:
    void update2error();                          // set x,y part of inherited mPositionError
    
    
protected:
    // data members
    Int_t   mKey;                                 // unique label
    Int_t   mModule;                                 // module number
    Int_t mAdcX, mAdcY;     // Adc counts in X and Y 
    Int_t mTbX, mTbY;     // time bins of maximum Adc count 
    Double_t mLocalX, mErrLocalX;     // local x position and error
    Double_t mLocalY, mErrLocalY;     // local y position and error
    Float_t mChargeUncert;                        // uncertanity on the charge
    // for keeping track of which strips constribute to which cluster (not persistant)
    gmtStripWeightMap_t mStripWeightMap;             //! 
    
private:   
    ClassDef(StGmtHit,1)
}; 
ostream&              operator<<(ostream& os, StGmtHit const & v);

// inline functions

// inline short StGmtHit::getMaxAdc() const { //////// FIX ME!!!!!!!!!!
//     short mMaxAdc = -1;
//     
//     for(gmtStripWeightMap_t::const_iterator it=mStripWeightMap.begin(); it != mStripWeightMap.end(); it++ ){
//         short adcVal = it->first->getMaxAdc();
//         if( adcVal > mMaxAdc ) 
//             mMaxAdc = adcVal;
//     };
// 
//     return mMaxAdc;
// }

inline int StGmtHit::getModule() const {            
    return mModule;  
};
inline int StGmtHit::getAdcX() const {            
    return mAdcX;  
};
inline int StGmtHit::getAdcY() const {            
    return mAdcY;  
};
inline int StGmtHit::getTbX() const {            
    return mTbX;  
};
inline int StGmtHit::getTbY() const {            
    return mTbY;  
};


inline int StGmtHit::getKey() const {
    return mKey;
};

inline gmtStripWeightMap_t& StGmtHit::getStripWeightMap() {
    return mStripWeightMap;
};

inline const gmtStripWeightMap_t& StGmtHit::getStripWeightMap() const {
    return mStripWeightMap;
};

// inline void StGmtHit::setHardwareId( short module ){
//     mHardwarePosition = module;      //////// FIX ME!!!!!!!!!!
// };

inline double StGmtHit::getLocalX() const {
    return mLocalX;
};
inline double StGmtHit::getLocalY() const {
    return mLocalY;
};

inline double StGmtHit::getErrorLocalX() const {
    return mErrLocalX;
};
inline double StGmtHit::getErrorLocalY() const {
    return mErrLocalY;
};




inline void StGmtHit::setAdcX( int adc ){    
    mAdcX = adc;
};

inline void StGmtHit::setAdcY( int adc ){    
    mAdcY = adc;
};

inline void StGmtHit::setTbX( int tb ){    
    mTbX = tb;
};

inline void StGmtHit::setTbY( int tb ){    
    mTbY = tb;
};

inline void StGmtHit::setModule( short module ){    
    mModule = module;
};

// inline void StGmtHit::setIsY( int isY ){
//     mIsY = isY;
//     short module = getModule();
//     
//     setHardwareId( module );
// };

inline void StGmtHit::setLocalX( double position ){ 
    mLocalX = position;
    mPosition.setX( mLocalX );
};

inline void StGmtHit::setLocalY( double position ){ 
    mLocalY = position;
    mPosition.setY( mLocalY );
};

inline void StGmtHit::setErrorLocalX( double error ){
    mErrLocalX = error;
    update2error();
};

inline void StGmtHit::setErrorLocalY( double error ){
    mErrLocalY = error;
    update2error();
};

// charge uncertainty

inline double StGmtHit::getChargeUncert() const { return mChargeUncert; };
inline void StGmtHit::setChargeUncert( double sigma ){ mChargeUncert = sigma; };

#endif
