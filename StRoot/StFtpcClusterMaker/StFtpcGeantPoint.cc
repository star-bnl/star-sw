// $Id: StFtpcGeantPoint.cc,v 1.1 2000/11/24 15:02:34 hummler Exp $
// $Log: StFtpcGeantPoint.cc,v $
// Revision 1.1  2000/11/24 15:02:34  hummler
// commit changes omitted in last commit
//

//----------Author:        Holm Huemmler
//----------Last Modified: 19.11.2000

#include "StFtpcGeantPoint.hh"

//////////////////////////////////////////////////////////////////////////////
//                                                                          //
// StFtpcGeantPoint class - representation of one cluster for evaluation.   //
//                                                                          //
// This class contains additional data members with geant information from  //
// StFtpcFastSimulator                                                      //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

ClassImp(StFtpcGeantPoint)


StFtpcGeantPoint::StFtpcGeantPoint()
{
  // Default constructor.
  // Sets all pointers to zero.

  SetTrackPointer(0);
  SetGeantPID(0);
  SetPrimaryTag(0);
  SetGeantProcess(0);

  SetVertexMomentum(0.0,0.0,0.0);
  SetLocalMomentum(0.0,0.0,0.0);
  SetVertexPosition(0.0,0.0,0.0);
}



StFtpcGeantPoint::~StFtpcGeantPoint() 
{
  // Destructor.
  // Does nothing.
}


Int_t StFtpcGeantPoint::ToTable(ffs_gepoint_st *geant_st)
{
  geant_st->ge_track_p = GetTrackPointer();
  geant_st->ge_pid = GetGeantPID();
  geant_st->prim_tag = GetPrimaryTag();
  geant_st->ge_proc = GetGeantProcess();

  geant_st->p_v[0] = GetVertexMomentum(0);
  geant_st->p_v[1] = GetVertexMomentum(1);
  geant_st->p_v[2] = GetVertexMomentum(2);

  geant_st->p_g[0] = GetLocalMomentum(0);
  geant_st->p_g[1] = GetLocalMomentum(1);
  geant_st->p_g[2] = GetLocalMomentum(2);

  geant_st->vertex[0] = GetVertexPosition(0);
  geant_st->vertex[1] = GetVertexPosition(1);
  geant_st->vertex[2] = GetVertexPosition(1);

  return 1;
}

