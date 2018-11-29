#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <cassert>

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

  static std::vector<error_msg> error_messages;

  inline void report_error(source_label label, std::string msg, std::vector<error_note> notes) {
    error_messages.emplace_back(error_type::error, label, msg, notes);
  }

  inline void report_warning(source_label label, std::string msg, std::vector<error_note> notes) {
    error_messages.emplace_back(error_type::warning, label, msg, notes);
  }

  inline void report_error(source_label label, std::string msg) {
    report_error(label, msg, {});
  }

  inline void report_warning(source_label label, std::string msg) {
    report_warning(label, msg, {});
  }

  /** Returns true if there are any errors */
  inline bool print_all_errors() {
    if (error_messages.size() == 0) { return false; }
    for (const auto& e : error_messages) {
      switch (e.type) {
      case error_type::error: std::cerr << "[Error] "; break;
      case error_type::warning: std::cerr << "[Warning] "; break;
      default: assert(false); break;
      }
      std::cerr << e.label.to_string() << ": " << e.msg << std::endl;
      for (const auto& note : e.notes) {
        std::cerr << "\t\t[Note] " << note.label.to_string() << ": " << note.msg << std::endl;
      }
    }
    return true;
  }
}
