//  17 may 01
//  Ben Norman

#ifndef STI_GEOMETRY_TRANSFORM_HH
#define STI_GEOMETRY_TRANSFORM_HH

class StiTrack;
class StiHitContainer;
class StiKalmanTrack;
class StiKalmanTrackNode;
class StGlobalTrack;
class StTpcHit;
class StHit;
class StSvtHit;
class StSsdHit;
class StiHit;
class StPrimaryVertex;
class StPhysicalHelix;
class StTpcHitFilter;
class StiCoordinateTransform;

class StiGeometryTransform{
    
public:
    
    static StiGeometryTransform* instance();
    static void kill();

    ///Set the hit errors in the rotated (StiHit) system defined by rotation of the StHit
    /// error matrix by angle theta about the z-axis.
    void setStiHitError(const StHit* stHit, StiHit* stiHit, double theta);

public:
    /// StTpcHit -> StiHit
    void operator() (const StTpcHit*, StiHit*); 
    /// StiHit -> StTpcHit
    void operator() (const StiHit*, StTpcHit*);
    
    /// StSvtHit -> StiHit
    void operator() (const StSvtHit*, StiHit*);
    /// StiHit -> StSvtHit
    void operator() (const StiHit*, StSvtHit*);
    
    /// StSsdHit -> StiHit
    void operator() (const StSsdHit*, StiHit*);
    /// StiHit -> StSsdHit
    void operator() (const StiHit*, StSsdHit*);
    
    /// StPrimaryVertex -> StiHit
    void operator() (const StPrimaryVertex*, StiHit*);

    /// StiKalmanTrackNode -> StPhysicalHelix
    void operator() (const StiKalmanTrackNode*, StPhysicalHelix*);

    /// StGlobalTrack -> StiKalmanTrack
    void operator() (StiHitContainer*, const StGlobalTrack*, StiKalmanTrack*,
		     unsigned int maxHits=1000, 
                     const StTpcHitFilter* filter=0) const;

protected: 

    StiGeometryTransform();    
    virtual ~StiGeometryTransform();

    static StiGeometryTransform* sinstance;

    StiCoordinateTransform *m_pCoordinateTransform;
};

#endif
