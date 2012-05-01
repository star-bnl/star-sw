// StHyperUtilFilesystem.h
#ifndef __ST_HYPERUTILFILESYSTEM_H
#define __ST_HYPERUTILFILESYSTEM_H

#include <string>

namespace StHyperUtilFilesystem
{

// check if file exists on disk
bool path_exists (const std::string& file);

// converts relative path to full path
std::string resolve_path(const std::string& path);

void create_dir_recursive(std::string path);

// unlink path, constrained by base_path so no tricks with relative paths will work
unsigned long remove_dir_recursive(std::string unlink_path, const std::string& base_path);

} // namespace StHyperUtilFilesystem

#endif // __ST_HYPERUTILFILESYSTEM_H
