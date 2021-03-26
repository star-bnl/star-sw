#ifndef CRITERIA_KEEPER_H
#define CRITERIA_KEEPER_H

#include "Criteria/Criteria.h"

class CriteriaKeeper : public KiTrack::ICriterion {

  public:
    CriteriaKeeper(KiTrack::ICriterion *child) {
        mChild = child;
        values.clear();

        _name = mChild->getName();
        _type = mChild->getType();
    }

    ~CriteriaKeeper() {
        if (mChild)
            delete mChild;
        mChild = nullptr;
    }

    virtual bool areCompatible(KiTrack::Segment *parent, KiTrack::Segment *child) {
        bool result = mChild->areCompatible(parent, child);



        // capture the primary result of the criteria
        float value = mChild->getMapOfValues()[mChild->getName()];
        bool same_track = false;
        int track_id = -1;

        

        // two hit criteria
        if ((parent->getHits().size() == 1) && (child->getHits().size() == 1)) {
            KiTrack::IHit *a = parent->getHits()[0];
            KiTrack::IHit *b = child->getHits()[0];

            auto fwdA = static_cast<FwdHit *>(a);
            auto fwdB = static_cast<FwdHit *>(b);

            

            same_track = (fwdA->_tid == fwdB->_tid && fwdA->_tid != 0);
            if (same_track)
                track_id = fwdA->_tid;
            else
                track_id = -1;

            if ( _name == "Crit2_RZRatio" ){
                // LOG_INFO << "Hit a_id = " << fwdA->_id << "Hit b_id = " << fwdB->_id << endm;
                std::map < std::string , float > my_map_name_value = mChild->getMapOfValues();
                my_map_name_value["Crit2_RZRatio_x1"] = a->getX();
                my_map_name_value["Crit2_RZRatio_y1"] = a->getY();
                my_map_name_value["Crit2_RZRatio_z1"] = a->getZ();
                my_map_name_value["Crit2_RZRatio_x2"] = b->getX();
                my_map_name_value["Crit2_RZRatio_y2"] = b->getY();
                my_map_name_value["Crit2_RZRatio_z2"] = b->getZ();

                my_map_name_value["Crit2_RZRatio_h1"] = fwdA->_id;
                my_map_name_value["Crit2_RZRatio_h2"] = fwdB->_id;
                        
                allValues.push_back( my_map_name_value );
            } else {
                std::map < std::string , float > my_map_name_value = mChild->getMapOfValues();
                allValues.push_back( my_map_name_value);
            }
        }

        // three hit criteria (two two-segments)
        if ((parent->getHits().size() == 2) && (child->getHits().size() == 2)) {
            KiTrack::IHit *a = child->getHits()[0];
            KiTrack::IHit *b = child->getHits()[1];
            KiTrack::IHit *c = parent->getHits()[1];



            auto fwdA = static_cast<FwdHit *>(a);
            auto fwdB = static_cast<FwdHit *>(b);
            auto fwdC = static_cast<FwdHit *>(c);

            std::map < std::string , float > my_map_name_value = mChild->getMapOfValues();

            my_map_name_value["Crit2_RZRatio_h1"] = fwdA->_id;
            my_map_name_value["Crit2_RZRatio_h2"] = fwdB->_id;
            my_map_name_value["Crit2_RZRatio_h3"] = fwdC->_id;

            allValues.push_back( my_map_name_value );

            same_track = (fwdA->_tid == fwdB->_tid && fwdB->_tid == fwdC->_tid && fwdA->_tid != 0);
            if (same_track)
                track_id = fwdA->_tid;
            else
                track_id = -1;
        }

        values.push_back(value);
        track_ids.push_back(track_id);

        return result;
    }

    std::vector<float> getValues( ) {
        return values;
    }
    std::vector<std::map < std::string , float >> getAllValues() {
        return allValues;
    }
    std::vector<int> getTrackIds() {
        return track_ids;
    }

    void clear() {
        values.clear();
        track_ids.clear();
        allValues.clear();
    }

  protected:
    ICriterion *mChild = nullptr;

    std::vector<float> values;
    std::vector<std::map < std::string , float >> allValues;
    std::vector<int> track_ids;
};


#endif