#ifndef AssociationQuality_H_INCLUDED
#define AssociationQuality_H_INCLUDED
class StiTrack;

class AssociationQuality
{
public: 
  AssociationQuality();
  ~AssociationQuality();
  void reset();
  void setFirst(StiTrack *first);
  void setSecond(StiTrack *second);
  void setQuality(double quality);
  void incrementQuality();
  void setLabel(int label);
  StiTrack * getFirst() const;
  StiTrack * getSecond() const;
  int    getLabel() const;
  double getQuality() const;
  double getDifference(int type) const;
  double getRelativeDifference(int type) const;
protected:
  StiTrack * _first;
  StiTrack * _second;
  double     _quality;
  int        _label;
};


#endif
