#ifndef BDT_Criteria_h
#define BDT_Criteria_h

#include "TMVA/Reader.h"
#include "Criteria/ICriterion.h"


/** Criterion: the difference of the sqrt(x^2 + y^2) of two hits.
*/
class BDTCrit2 : public KiTrack::ICriterion{

public:


  
    BDTCrit2 ( float scoreMin , float scoreMax ){
        _scoreMax = scoreMax;  
        _scoreMin = scoreMin;

        _name = "Crit2_BDT";
        _type = "2Hit";

        _saveValues = false;

        
    }


    float EvalDeltaPhi( KiTrack::IHit*a, KiTrack::IHit*b ){
        // TODO: work on branchless version?
        float ax = a->getX();
        float ay = a->getY();
        float bx = b->getX();
        float by = b->getY();

        float phia = atan2( ay, ax );
        float phib = atan2( by, bx );
        float deltaPhi = phia - phib;

        if (deltaPhi > M_PI) deltaPhi -= 2*M_PI;           //to the range from -pi to pi
        if (deltaPhi < -M_PI) deltaPhi += 2*M_PI;           //to the range from -pi to pi
        
        if (( by*by + bx*bx < 0.0001 )||( ay*ay + ax*ax < 0.0001 )) deltaPhi = 0.; // In case one of the hits is too close to the origin

        deltaPhi = 180.*fabs( deltaPhi ) / M_PI;
        return deltaPhi;
    }
    float EvalDeltaRho( KiTrack::IHit*a, KiTrack::IHit*b ){
        float ax = a->getX();
        float ay = a->getY();
        float bx = b->getX();
        float by = b->getY();

        float rhoA =  sqrt( ax*ax + ay*ay );
        float rhoB =  sqrt( bx*bx + by*by );

        float deltaRho = rhoA - rhoB;
        return deltaRho;
    }

    float EvalRZRatio( KiTrack::IHit*a, KiTrack::IHit*b ){
        float ax = a->getX();
        float ay = a->getY();
        float az = a->getZ();

        float bx = b->getX();
        float by = b->getY();
        float bz = b->getZ();

        // the square is used, because it is faster to calculate with the squares than with sqrt, which takes some time!
        double ratioSquared = 0.; 
        if ( az-bz  != 0. ) 
            ratioSquared = ( (ax-bx)*(ax-bx) + (ay-by)*(ay-by) + (az-bz)*(az-bz) ) / ( (az-bz) * ( az-bz ) );

        return sqrt(ratioSquared);
    }

    float EvalStraightTrackRatio( KiTrack::IHit*a, KiTrack::IHit*b ){
        float ax = a->getX();
        float ay = a->getY();
        float az = a->getZ();

        float bx = b->getX();
        float by = b->getY();
        float bz = b->getZ();

        //the distance to (0,0) in the xy plane
        double rhoASquared = ax*ax + ay*ay;
        double rhoBSquared = bx*bx + by*by;

        double ratioSquared = 0;
        if( (rhoBSquared >0.) && ( az != 0. ) ){ //prevent division by 0
            // the square is used, because it is faster to calculate with the squares than with sqrt, which takes some time!
            ratioSquared = ( ( rhoASquared * ( bz*bz )  ) / ( rhoBSquared * ( az*az )  ) );
        }

        return sqrt( ratioSquared );
    }
  
  virtual bool areCompatible( KiTrack::Segment* parent , KiTrack::Segment* child ){


    if ( reader == nullptr ){
        BDTCrit2::reader = new TMVA::Reader("!Color:!Silent");

        // setup the inputs
        BDTCrit2::reader->AddVariable("Crit2_RZRatio", &BDTCrit2::Crit2_RZRatio);
        BDTCrit2::reader->AddVariable("Crit2_DeltaRho", &BDTCrit2::Crit2_DeltaRho);
        BDTCrit2::reader->AddVariable("Crit2_DeltaPhi", &BDTCrit2::Crit2_DeltaPhi);
        BDTCrit2::reader->AddVariable("Crit2_StraightTrackRatio", &BDTCrit2::Crit2_StraightTrackRatio);

        BDTCrit2::reader->BookMVA("BDT method", "bdt2-Copy1.xml");
    }

    if (( parent->getHits().size() == 1 )&&( child->getHits().size() == 1 )){
    } //a criterion for 1-segments
    else {
        std::stringstream s;
        s << "Crit2_BDT::This criterion needs 2 segments with 1 hit each, passed was a "
        <<  parent->getHits().size() << " hit segment (parent) and a "
        <<  child->getHits().size() << " hit segment (child).";

        throw KiTrack::BadSegmentLength( s.str() );
    }

    KiTrack::IHit* a = parent->getHits()[0];
    KiTrack::IHit* b = child-> getHits()[0];


    //first check, if the distance to (0,0) rises --> such a combo could not reach the IP
    

    // compute input values
    BDTCrit2::Crit2_DeltaPhi = EvalDeltaPhi( a, b );
    BDTCrit2::Crit2_DeltaRho = EvalDeltaRho( a, b );
    BDTCrit2::Crit2_RZRatio  = EvalRZRatio( a, b );
    BDTCrit2::Crit2_StraightTrackRatio  = EvalStraightTrackRatio( a, b );

    float score = BDTCrit2::reader->EvaluateMVA("BDT method");

    if (_saveValues){
      _map_name_value["Crit2_BDT"] = score;
      _map_name_value["Crit2_BDT_DeltaPhi"] = BDTCrit2::Crit2_DeltaPhi;
      _map_name_value["Crit2_BDT_DeltaRho"] = BDTCrit2::Crit2_DeltaRho;
      _map_name_value["Crit2_BDT_RZRatio"] = BDTCrit2::Crit2_RZRatio;
      _map_name_value["Crit2_BDT_StraightTrackRatio"] = BDTCrit2::Crit2_StraightTrackRatio;
    }

    if ( score < _scoreMin || score > _scoreMax ) return false;
    return true;
  }
  
  virtual ~BDTCrit2(){};
  

private:
  
  float _scoreMin{};
  float _scoreMax{};
  static TMVA::Reader *reader;
  // values input to BDT
  static float Crit2_RZRatio, Crit2_DeltaRho, Crit2_DeltaPhi, Crit2_StraightTrackRatio;
  
  
  
};




class BDTCrit3 : public KiTrack::ICriterion{

public:
  
    BDTCrit3 ( float scoreMin , float scoreMax ){
        _scoreMax = scoreMax;  
        _scoreMin = scoreMin;

        _name = "Crit3_BDT";
        _type = "3Hit";

        _saveValues = false;

        
    }


    float Eval3DAngle(KiTrack::IHit*a, KiTrack::IHit*b, KiTrack::IHit*c ){
        return 0;
    }

    float Eval2DAngle(KiTrack::IHit*a, KiTrack::IHit*b, KiTrack::IHit*c ){
        return 0;
    }

    float EvalChangeRZRatio(KiTrack::IHit*a, KiTrack::IHit*b, KiTrack::IHit*c ){
        return 0;
    }


    virtual bool areCompatible( KiTrack::Segment* parent , KiTrack::Segment* child ){

        if ( reader == nullptr ){
            BDTCrit3::reader = new TMVA::Reader("!Color:!Silent");

            // setup the inputs
            BDTCrit3::reader->AddVariable("Crit3_ChangeRZRatio", &BDTCrit3::Crit3_ChangeRZRatio);
            BDTCrit3::reader->AddVariable("Crit3_3DAngle", &BDTCrit3::Crit3_3DAngle);
            BDTCrit3::reader->AddVariable("Crit3_2DAngle", &BDTCrit3::Crit3_2DAngle);

            BDTCrit3::reader->BookMVA("BDT method", "bdt2-Copy1.xml");
        }

        if (( parent->getHits().size() == 2 )&&( child->getHits().size() == 2 )){
        } //a criterion for 1-segments
        else {
            std::stringstream s;
            s << "Crit3_BDT::This criterion needs 2 segments with 1 hit each, passed was a "
            <<  parent->getHits().size() << " hit segment (parent) and a "
            <<  child->getHits().size() << " hit segment (child).";

            throw KiTrack::BadSegmentLength( s.str() );
        }

        KiTrack::IHit* a = child->getHits()[0];
        KiTrack::IHit* b = child->getHits()[1];
        KiTrack::IHit* c = parent-> getHits()[1];

        // compute input values
        BDTCrit3::Crit3_2DAngle         =  Eval2DAngle( a, b, c );
        BDTCrit3::Crit3_3DAngle         =  Eval3DAngle( a, b, c );
        BDTCrit3::Crit3_ChangeRZRatio   =  EvalChangeRZRatio( a, b, c );
        
        float score = BDTCrit3::reader->EvaluateMVA("BDT3 method");

        if (_saveValues){
            _map_name_value["Crit3_BDT"] = score;
            _map_name_value["Crit3_BDT_2DAngle"] = BDTCrit3::Crit3_2DAngle;
            _map_name_value["Crit3_BDT_3DAngle"] = BDTCrit3::Crit3_3DAngle;
            _map_name_value["Crit3_BDT_ChangeRZRatio"] = BDTCrit3::Crit3_ChangeRZRatio;
            
        }

        if ( score < _scoreMin || score > _scoreMax ) return false;
        return true;
    }
  
    virtual ~BDTCrit3(){};

private:
  
    float _scoreMin{};
    float _scoreMax{};
    static TMVA::Reader *reader;
    // values input to BDT
    static float Crit3_ChangeRZRatio, Crit3_3DAngle, Crit3_2DAngle;
  
};

#endif