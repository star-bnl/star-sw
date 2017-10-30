//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#include "KFPEmcCluster.h"

void KFPEmcCluster::SetParameter(const float_v& value, int iP, int iTr)
{ 
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fP[iP][iTr]) = value;
  else
  {
    const uint_v index(uint_v::IndexesFromZero());
    (reinterpret_cast<float_v&>(fP[iP][iTr])).gather(reinterpret_cast<const float*>(&value), index, float_m(index<(Size() - iTr)));
  }
}
void KFPEmcCluster::SetCovariance(const float_v& value, int iC, int iTr) 
{ 
  if( (iTr+float_vLen) < Size())
    reinterpret_cast<float_v&>(fC[iC][iTr]) = value;
  else
  {
    const uint_v index(uint_v::IndexesFromZero());
    (reinterpret_cast<float_v&>(fC[iC][iTr])).gather(reinterpret_cast<const float*>(&value), index, float_m(index<(Size() - iTr)));
  }
}

void KFPEmcCluster::Resize(const int n)
{
  for(int i=0; i<4; i++)
    fP[i].resize(n);
  for(int i=0; i<10; i++)
    fC[i].resize(n);
  fId.resize(n);
}

void KFPEmcCluster::Set(KFPEmcCluster& v, int vSize, int offset)
{
  for(int iV=0; iV<vSize; iV++)
  {
    for(int i=0; i<4; i++)
      fP[i][offset+iV] = v.fP[i][iV];
    for(int i=0; i<10; i++)
      fC[i][offset+iV] = v.fC[i][iV];
    fId[offset+iV] = v.fId[iV];
  }
}

void KFPEmcCluster::SetTracks(const KFPEmcCluster& track, const kfvector_uint& trackIndex, const int nIndexes)
{
  //track - input vector of tracks
  //trackIndex - indexes of tracks in a vector "track", which should be stored 
  //to the current vector

  if(nIndexes == 0) return;
  
  Resize(nIndexes);

  for(int iP=0; iP<4; iP++)
  {
    int iElement = 0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
      vec.gather(&(track.fP[iP][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fP[iP][iElement]);
    vec.gather(&(track.fP[iP][0]), index, float_m(iElement+uint_v::IndexesFromZero()<nIndexes));
    
  }
  for(int iC=0; iC<10; iC++)
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
      vec.gather(&(track.fC[iC][0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    float_v& vec = reinterpret_cast<float_v&>(fC[iC][iElement]);
    vec.gather(&(track.fC[iC][0]), index, float_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
  {
    int iElement=0;
    for(iElement=0; iElement<nIndexes-float_vLen; iElement += float_vLen)
    {
      const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
      int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
      vec.gather(&(track.fId[0]), index);
    }
    const uint_v& index = reinterpret_cast<const uint_v&>(trackIndex[iElement]);
    int_v& vec = reinterpret_cast<int_v&>(fId[iElement]);
    vec.gather(&(track.fId[0]), index, int_m(iElement+uint_v::IndexesFromZero()<nIndexes));
  }
}

void KFPEmcCluster::PrintTrack(int n)
{
  for(int i=0; i<4; i++)
    std::cout << fP[i][n] << " ";
  std::cout << std::endl;
  for(int i=0; i<10; i++)
    std::cout << fC[i][n] << " ";
  std::cout << std::endl;
  
  std::cout << fId[n] << std::endl;
}

void KFPEmcCluster::PrintTracks()
{
  std::cout << "NTracks " << Size() << std::endl;
  if( Size()==0 ) return;
  
  std::cout << "Parameters: " << std::endl;
  for(int iP=0; iP<4; iP++)
  {
    std::cout << "  iP " << iP << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Parameter(iP)[iTr]<< " ";
    std::cout << std::endl;
  }

  std::cout << "Cov matrix: " << std::endl;
  for(int iC=0; iC<10; iC++)
  {
    std::cout << "  iC " << iC << ": ";
    for(int iTr=0; iTr<Size(); iTr++)
      std::cout << Covariance(iC)[iTr]<< " ";
    std::cout << std::endl;
  }
  
  std::cout << "Id: " << std::endl;
  for(int iTr=0; iTr<Size(); iTr++)
    std::cout <<  Id()[iTr] << " ";
  std::cout << std::endl;
}
  