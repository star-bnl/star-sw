#ifndef StiFactory_H
#define StiFactory_H 1

class StiFactory
{
 public:

    //StiFactory(); //Not implemented
    StiFactory(const char * name);
    virtual ~StiFactory();
    
    const char * getName();
    
protected:
    
    const char * name;
};

#endif
