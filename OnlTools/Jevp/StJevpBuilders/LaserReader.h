#ifndef _LASER_READER_H__
#define _LASER_READER_H__

class TH1D;

class LaserReader
{
public:

    LaserReader();
    ~LaserReader(){};
    float Make(daqReader *rdr);
    void resetAll();

private:

    int run;
    double TPC_DELTAT;

    TH1D* crossingHitsHist;  // index is (s/2 + mirror + 1)
    TH1D* driftVelocityHist;
};
#endif


