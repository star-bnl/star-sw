#ifndef StDetectorDbGridLeak_h
#define StDetectorDbGridLeak_h

class StMaker;


class  TTable;
struct tpcGridLeak_st;

enum StGLpos {
  kGLinner=0,
  kGLmiddl=1,
  kGLouter=2
};

class StDetectorDbGridLeak{
public:
    static StDetectorDbGridLeak*  instance();
    double                     getGridLeakStrength(StGLpos pos);
    double                     getGridLeakRadius(StGLpos pos);
    double                     getGridLeakWidth(StGLpos pos);
    friend ostream& operator<<(ostream& os, StDetectorDbGridLeak& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbGridLeak();
    StDetectorDbGridLeak();
    tpcGridLeak_st * mGridLeak; // points to gridleak struct
    TTable* mTable; // points to table
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbGridLeak* sInstance;
};


#endif
