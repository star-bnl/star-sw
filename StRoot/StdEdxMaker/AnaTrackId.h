//  AnaTrackId.h
//  M.L. Miller
//  5/00

#ifndef AnaTrackId_HH
#define AnaTrackId_HH

//Forward Declarations
class StTrack;
class StDedxPidTraits;
class StParticleDefinition;

class AnaTrackId {
public:
    
    //Constructor-destructor---------------------------------------
    AnaTrackId( StTrack* );
    AnaTrackId();
    virtual ~AnaTrackId();

    //Methods-----------------------------------------------------------
    void initialize();
    void findPidTrait(StTrack* );
    void findPid(StTrack* );
    
    //Access--------------------------------------------------------------
    //Get dedxPidTr
    StDedxPidTraits* getPidTraits();
    //Get guess
    StParticleDefinition* getGuess();
    const StParticleDefinition* getGuess() const;
    
private:
    
    //Members----------------------------------------------------------
    
    //dedxPidTr
    StDedxPidTraits* m_dedxPidTr;
    //guess
    StParticleDefinition* m_guess;
};

#endif
