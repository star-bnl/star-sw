//StiDetPolygonSide.h
//M.L. Miller (Yale Software)
//06/01

#ifndef StiDetPolygonSide_HH
#define StiDetPolygonSide_HH

class StiDetetector;
class StiDetPolygonSide;

class StiDetPolygonSide
{
public:
    StiDetPolygonSide();
    virtual ~StiDetPolygonSide();

    //sets
    void setDetector(StiDetector* val);

    //gets
    StiDetector* detector() const;

    //Action for hits
    //unsigned int numberOfHits() const;

protected:
    StiDetector* mdetector;
    //hitvector* mhitvector;
    
private:
    
};

//Inlines

//Non-memebers
inline ostream& operator<<(ostream& os, const StiDetPolygonSide& side)
{
    return os <<(*(side.detector()));
}

#endif
