//StiTranslator.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiTranslator_HH
#define StiTranslator_HH

class StTpcCoordinateTransform;

class StiTranslator
{
public:
    StiTranslator();
    virtual ~StiTranslator();
    
protected:
    StTpcCoordinateTransform* mtpctransformer;
};

#endif

