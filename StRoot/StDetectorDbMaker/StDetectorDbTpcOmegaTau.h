#ifndef StDetectorDbTpcOmegaTau_h
#define StDetectorDbTpcOmegaTau_h

class StMaker;


class  TTable;
struct tpcOmegaTau_st;

class StDetectorDbTpcOmegaTau{
public:
    static StDetectorDbTpcOmegaTau*  instance();
    float getOmegaTauTensorV1();
    float getOmegaTauTensorV2();
    friend ostream& operator<<(ostream& os, StDetectorDbTpcOmegaTau& v);

    // These fuction will be public
    // but should be used only for debugging
    void update(StMaker*);
    friend class nobody; // for virtual ~
    
protected:
    virtual ~StDetectorDbTpcOmegaTau();
    StDetectorDbTpcOmegaTau();
    tpcOmegaTau_st * mOmegaTau; // points to OmegaTau struct
    TTable* mTable; // points to table
    StMaker * mMaker; // Holds pointer to maker
private:
    static StDetectorDbTpcOmegaTau* sInstance;
};


#endif
