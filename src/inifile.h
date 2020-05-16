#ifndef INIFILE_H
#define INIFILE_H

#include <fstream>
#include <iostream>
#include <string>

#include "boost/property_tree/ini_parser.hpp"
#include "boost/property_tree/ptree.hpp"

class IniFile {
 public:
  enum ErrorCode { NoError, FileNotFound, FileParseError, KeyNotFound };

  IniFile(const std::string &filename)
      : m_initialized(false), m_filename(filename) {}

  IniFile(const char *filename) : m_initialized(false), m_filename(filename) {}

  int read() {
    std::ifstream f(this->m_filename);
    if (f.bad()) {
      this->setError(FileNotFound);
      return this->error();
    }
    try {
      boost::property_tree::read_ini(this->m_filename, this->m_iniFile);
    } catch (const boost::property_tree::ptree_error &e) {
      std::cerr << "[ERROR]: " << e.what() << std::endl;
      this->setError(FileParseError);
      return this->error();
    } catch (const boost::property_tree::ini_parser::ini_parser_error &e) {
      std::cerr << "[ERROR]: " << e.what() << std::endl;
      this->setError(FileParseError);
      return this->error();
    } catch (...) {
      std::cerr << "[ERROR]: could not read the specified ini file"
                << std::endl;
      this->setError(FileParseError);
      return this->error();
    }
    this->m_initialized = true;
    this->setError(NoError);
    return this->error();
  }

  template <typename T>
  int get(const std::string &section, const std::string &key, T &value) {
    if (this->contains<T>(section, key)) {
      value = this->m_iniFile.get<T>(section + "." + key);
      this->setError(NoError);
      return this->error();
    } else {
      this->setError<T>(KeyNotFound, section, key);
      return this->error();
    }
  }

  template <typename T>
  int put(const std::string &section, const std::string &key, const T &value) {
    this->m_iniFile.put<T>(section + "." + key, value);
    this->setError(NoError);
    return this->error();
  }

  template <typename T>
  bool contains(const std::string &section, const std::string &key) {
    return this->m_iniFile.get_optional<T>(section + "." + key)
        .is_initialized();
  }

  std::string filename() const { return this->m_filename; }

  std::string errorString() const { return this->m_errorString; }
  int error() const { return static_cast<int>(this->m_errorCode); }

  bool initialized() const;

 private:
  template <typename T>
  struct Typestring {
    static std::string name() { return typeid(T).name(); }
  };
  template <>
  struct Typestring<int> {
    static std::string name() { return "integer"; }
  };
  template <>
  struct Typestring<float> {
    static std::string name() { return "float"; }
  };
  template <>
  struct Typestring<double> {
    static std::string name() { return "double"; }
  };
  template <>
  struct Typestring<bool> {
    static std::string name() { return "bool"; }
  };
  template <>
  struct Typestring<unsigned> {
    static std::string name() { return "unsigned"; }
  };

  template <typename T>
  void setError(const ErrorCode &code, const std::string &section,
                const std::string &name) {
    this->m_errorCode = code;
    switch (this->m_errorCode) {
      case NoError:
        this->m_errorString = "No error";
        break;
      case KeyNotFound:
        this->m_errorString = "The specified key pair " + section + "-->" +
                              name + " was not found for type " +
                              Typestring<T>::name() + ".";
        break;
      default:
        this->m_errorString = "An unknown error was specified for " + section +
                              "-->" + name + ".";
        break;
    }
  }

  void setError(const ErrorCode &code) {
    this->m_errorCode = code;
    switch (this->m_errorCode) {
      case NoError:
        this->m_errorString = "No error";
        break;
      case FileNotFound:
        this->m_errorString =
            "The specfified file " + this->m_filename + " was not found.";
        break;
      case FileParseError:
        this->m_errorString =
            "The Boost library returned an error code during parsing of " +
            this->m_filename + ".";
        break;
      default:
        this->m_errorString = "An unknown error was specified";
        break;
    }
  }

  bool m_initialized;
  const std::string m_filename;
  boost::property_tree::ptree m_iniFile;
  std::string m_errorString;
  ErrorCode m_errorCode;
};

bool IniFile::initialized() const { return m_initialized; }

#endif  // INIFILE_H
