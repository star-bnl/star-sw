/**************************************************************************
 * Copyright(c) 2001, STAR  Experiment at BNL, All rights reserved.       *
 *                                                                        *
 * Author: STAR Integrated Track Task Force                               *
 * Contributors are mentioned in the code where appropriate.              *
 *                                                                        *
 * Permission to use, copy, modify and distribute this software and its   *
 * documentation strictly for non-commercial purposes is hereby granted   *
 * without fee, provided that the above copyright notice appears in all   *
 * copies and that both the copyright notice and this permission notice   *
 * appear in the supporting documentation. The authors make no claims     *
 * about the suitability of this software for any purpose. It is          *
 * provided "as is" without express or implied warranty.                  *
 *                                                                        *
 **************************************************************************/

/**************************************************************************
 *                                                                        *
 * StiTrackFactory					                  *				   
 *                                                                        *
 * Author:  Claude Pruneau, Wayne State University                        *
 * Created: March 2001                                                    *
 *                                                                        *
 * Description: A factory class used to provide the Sti tracking code     *
 * with StiDefaultTreeNode instances upon the demand. The instances are   *
 * created all at once and never destroyed.                               *
 *                                                                        *
 * The external interface consists of one method: getTrack() which returns*
 * upon demand a new instance of node.                                    *
 *                                                                        *
 * The original allocation of nodes  has a size determined by the para-   *
 * meter "originalSize".                                                  *
 * The instances are created internally upon construction of the factory  *
 * object.  Note that if the original allocation is insufficient, new     *
 * instances will automatically be allocated and created. The new allo-   *
 * cation will have a size determined by the parameter "incrementalSize"  *
 * New allocations are permitted a finite number of times determined by   *
 * the value of "maxIncrementCount".                                      *
 *                                                                        *
 **************************************************************************/
#include <iostream.h>

#include "StiTreeNodeFactory.h"

ClassImp(StiTreeNodeFactory) 


StiTreeNodeFactory::StiTreeNodeFactory(int originalSize, 
				       int incrementalSize=-1, 
				       int maxIncrementCount=-1,
				       int childCount=1)
  : StiObjectFactory("TreeNode",originalSize,incrementalSize,maxIncrementCount)
{
    defaultChildCount = childCount;
    createObjects(originalSize);
    currentSize = container.size();
}

StiTreeNodeFactory::~StiTreeNodeFactory()
{  
    for (tobject_vector::iterator it=container.begin(); it!=container.end(); ++it) {
	delete (*it);
	(*it) = 0;
    }
}

StiTreeNode * StiTreeNodeFactory::getTreeNode()
{
    return dynamic_cast<StiTreeNode*>( getObject() );
}
  
void StiTreeNodeFactory::createObjects(int n)
{
    for (int i=0;i<n;i++) {
	// add instance of tree node with null user object
	container.push_back( new StiDefaultMutableTreeNode(0, defaultChildCount) );
    }
}
