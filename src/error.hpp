#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <cassert>

#include "source_label.hpp"

namespace cello {
  enum class error_type {
    error, warning
  };

  struct error_note {
    source_label label;
    std::string msg;
  };

  struct error_msg {
    error_type type;
    source_label label;
    std::string msg;
    std::vector<error_note> notes;
    error_msg(error_type type,
              source_label label,
              std::string msg,
              std::vector<error_note> notes) :
      type(type), label(label), msg(msg), notes(notes) {}
  };

  void report_error(source_label label, std::string msg, std::vector<error_note> notes);
  void report_warning(source_label label, std::string msg, std::vector<error_note> notes);
  void report_error(source_label label, std::string msg);
  void report_warning(source_label label, std::string msg);

  /** Returns true if there are any errors */
  bool print_all_errors();
}
