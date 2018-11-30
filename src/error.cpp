#include "error.hpp"

namespace cello {

  static std::vector<error_msg> error_messages;

  void report_error(source_label label, std::string msg, std::vector<error_note> notes) {
    error_messages.emplace_back(error_type::error, label, msg, notes);
  }

  void report_warning(source_label label, std::string msg, std::vector<error_note> notes) {
    error_messages.emplace_back(error_type::warning, label, msg, notes);
  }

  void report_error(source_label label, std::string msg) {
    report_error(label, msg, {});
  }

  void report_warning(source_label label, std::string msg) {
    report_warning(label, msg, {});
  }

  /** Returns true if there are any errors */
  bool print_all_errors() {
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
