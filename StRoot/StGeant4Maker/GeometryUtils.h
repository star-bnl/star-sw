#ifndef __GeometryUtils_h__
#define __GeometryUtils_h__

/**
 * @file GeometryUtils.h
 * @brief Utility functions for accessing AgML geometry extensions.
 *
 * This file provides helper functions to retrieve AgMLExtension objects
 * associated with TGeoNode and TGeoVolume objects, simplifying access
 * to STAR-specific geometry metadata.
 */

#include <TGeoNode.h>
#include <TGeoVolume.h>
#include <StarVMC/StarAgmlLib/AgMLExtension.h>

/**
 * @brief Retrieves the AgMLExtension for a given TGeoNode.
 *
 * This function searches for an AgMLExtension object attached to the specified
 * TGeoNode. If not found, it checks the associated TGeoVolume, and finally
 * inherits from the mother volume if necessary.
 * @param n A pointer to the TGeoNode.
 * @return A pointer to the AgMLExtension, or nullptr if not found.
 */
AgMLExtension* getExtension( TGeoNode*   n );
/**
 * @brief Retrieves the AgMLExtension for a given TGeoVolume.
 *
 * This function is a wrapper around AgMLExtension::get(TGeoVolume*) to provide
 * a consistent interface for geometry metadata access.
 * @param v A pointer to the TGeoVolume.
 * @return A pointer to the AgMLExtension, or nullptr if not found.
 */
AgMLExtension* getExtension( TGeoVolume* v );

#endif
