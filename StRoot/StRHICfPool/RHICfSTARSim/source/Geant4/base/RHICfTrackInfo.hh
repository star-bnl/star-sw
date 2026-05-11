#ifndef RHICFTRACKINFO_H
#define RHICFTRACKINFO_H 1

#include "G4VUserTrackInformation.hh"


class RHICfTrackInfo : public G4VUserTrackInformation {
    public:
        RHICfTrackInfo(int primaryID);
        virtual ~RHICfTrackInfo();

        void SetPrimaryID(int id);
        int GetPrimaryID();

    private:
        int fPrimaryID;
};

#endif
