#pragma once

#include <nonstd/string_view.hpp>
#include <string>
#include <cstdint>

namespace cello {
  /** A label into the source code */
  struct source_label {
    nonstd::string_view file_name;
    std::uint32_t lineno;
    std::uint32_t charno;

    /** Generate a string from the label */
    std::string to_string() const {
      return std::string(file_name) + ":" + std::to_string(lineno) + ":" + std::to_string(charno);
    }
  };
}
