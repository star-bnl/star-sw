//StiFactoryTypedefs.h
//M.L. Miller (Yale Software)
//05/01

#ifndef StiFactoryTypedefs_HH
#define StiFactoryTypedefs_HH

#include "StiObjectFactory.h"
#include "StiHit.h"
#include "StiDefaultMutableTreeNode.h"

typedef StiObjectFactory<StiHit> StiHitFactory;
typedef StiObjectFactory<StiDefaultMutableTreeNode> StiTreeNodeFactory;

#endif
