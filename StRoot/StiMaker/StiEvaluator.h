//StiEvaluator.h
// A.Rose (WSU)]
//8/01

#ifndef StiEvaluator_HH
#define StiEvaluator_HH

//forward declarations (must #include these in the source file)
class StiTrackContainer;

class StiEvaluator
{
 public:
    static StiEvaluator* instance();
    static void kill();

    friend class nobody;

    void evaluateForEvent(const StiTrackContainer*);
    
 private:
    StiEvaluator();
    virtual ~StiEvaluator();
    
    static StiEvaluator* sinstance;
};

#endif
