// StHyperUtilFilesystem.h
#ifndef __ST_HYPERUTILFILESYSTEM_H
#define __ST_HYPERUTILFILESYSTEM_H

#include <string>
#include <fstream>

namespace StHyperUtilFilesystem
{

// calculate free space percentage from path (/tmp, /scratch etc)
double get_free_space_percentage(const char* path, size_t& bytes_free, size_t& bytes_total);

// delete files using FIFO or LRU policy
bool remove_dir_fifo(std::string unlink_path, const std::string& base_path, double bytes_free);
bool remove_dir_lru(std::string unlink_path, const std::string& base_path, double bytes_free);

// check if file exists on disk
bool path_exists (const std::string& file);

// converts relative path to full path
std::string resolve_path(const std::string& path);

void create_dir_recursive(std::string path);

// unlink path, constrained by base_path so no tricks with relative paths will work
unsigned long remove_dir_recursive(std::string unlink_path, const std::string& base_path);

// get file size in bytes
std::ifstream::pos_type filesize(const char* filename);

} // namespace StHyperUtilFilesystem

#endif // __ST_HYPERUTILFILESYSTEM_H
